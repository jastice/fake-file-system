package ffs

import java.io.{File => JFile}
import java.nio.ByteBuffer

import ffs.impl._
import manipulation._
import ffs.common.constants
import FFS._

/**
  * The Fake File System.
  *
  * Because it operates on physical files with strict evaluation semantics,
  * we can't really achieve thread-safety by immutability. Instead, we use file locks to block other processes
  * from accessing the file externally (hopefully) and have only one instance per physical file. This is ensured by a
  * private constructor and factory methods.
  */
class FFS private(physical: JFile, private[ffs] var header: HeaderBlock, private[ffs] val freeMap: FreeMap) {


  /** List all files within `path`. If path is a file, list that. */
  def ls(path: String): Seq[FileNode] = this.synchronized {
    val filePath = Path(path)


    readWithIO { io =>
      def fileNodes(addresses: Vector[Int]) =
        fileEntries(io)(addresses)
          .map { e => if (e.dir) Directory(e.name) else File(e.name) }

      if (filePath.isRoot) // ls on root dir
        fileNodes(header.rootBlockAddresses)
      else {
        fileFromRoot(io, filePath)
          .map { entry =>
            if (entry.deleted)
              Vector.empty[FileNode]
            else if (entry.dir) {
              val block = DirectoryIndexBlock(io.getBlock(entry.address))
              fileNodes(block.blockAddresses)
            }
            else Vector(File(entry.name))
      }

      }.getOrElse(Vector.empty[FileNode])
    }
  }

  /** Size of file. Always 0 for directories. */
  def size(path: String): Int = this.synchronized {
    val filePath = Path(path)
    readWithIO { io =>
      val fileEntryOpt = fileFromRoot(io, filePath)
      require(fileEntryOpt.isDefined)
      val fileEntry = fileEntryOpt.get
      if (fileEntry.dir) 0
      else FileBlock(io.getBlock(fileEntry.address)).fileSize
    }
  }

  /** Create directory in at path. Path must be directory itself.
    *
    * @return true iff directory was created
    */
  def mkdir(path: String): Boolean = this.synchronized {
    createFile(Path(path), dir = true).isDefined
  }

  /** Create empty file at path.
    *
    * @return true iff file was created
    */
  def touch(path: String): Boolean = this.synchronized {
    createFile(Path(path), dir = false).isDefined
  }

  /**
    * Delete a file.
    *
    * @return true iff file exists and was deleted
    */
  def rm(path: String): Boolean = this.synchronized {
    val filePath = Path(path)
    val name = filePath.name

    mutateWithIO { io =>

      val deleted = for {
        parentBlock <-
          if (filePath.isRoot) None // can't delete root
          else if (filePath.parent.isRoot) Some(header)
          else fileFromRoot(io, filePath.parent).map { parentEntry =>
            val parentBlockBytes = io.getBlock(parentEntry.address)
            DirectoryIndexBlock(parentBlockBytes)
          }

        (dirBlockAddress, dirBlock) <- dirBlockForName(io, parentBlock.blockAddresses, name)
        fileEntryIndex = dirBlock.files.indexWhere(_.name == name) if fileEntryIndex >= 0
        fileEntry = dirBlock.files(fileEntryIndex)
        fileBlockBytes = io.getBlock(fileEntry.address)

        freeUs <-
        if (fileEntry.dir) {
          // can only delete if all content files are already marked deleted
          val block = DirectoryIndexBlock(fileBlockBytes)
          val canDelete = block.blockAddresses.forall { a =>
            val b = DirectoryBlock(io.getBlock(a))
            b.files.forall(e => e.deleted)
          }

          if (canDelete) Some(block.blockAddresses)
          else None
        } else {
          val block = FileBlock(fileBlockBytes)
          Some(block.dataBlocks)
        }
      } yield {
        freeMap.freeBlocks(freeUs :+ fileEntry.address)
        val updatedFileEntry = fileEntry.copy(deleted = true)
        val updatedDirBlock = dirBlock.copy(files = dirBlock.files.updated(fileEntryIndex, updatedFileEntry))
        io.writeBlock(dirBlockAddress, updatedDirBlock)
        true
      }

      deleted.isDefined
    }
  }


  /** Append bytes to a file. Must not be a directory. */
  def append(path: String, bytes: Array[Byte]): Unit = this.synchronized {
    import constants.BLOCKSIZE

    val filePath = Path(path)
    require(!filePath.isRoot, "/ is not a file you can append to")

    mutateWithIO { io =>
      val fileEntryOpt = fileFromRoot(io,filePath)
      require(fileEntryOpt.isDefined, s"file $path does not exist")
      val fileEntry = fileEntryOpt.get
      require(!fileEntry.deleted, s"file '$path' does not exist")
      require(!fileEntry.dir, s"cannot append to '$path' because it is a directory")

      val fileBlock = FileBlock(io.getBlock(fileEntry.address))

      val (consumedBytes, initialUpdatedBlock) =
        if (fileBlock.dataBlocks.nonEmpty) {
          val lastBlockFilledBytes = fileBlock.fileSize % BLOCKSIZE
          val availableBytes = (BLOCKSIZE - lastBlockFilledBytes) min bytes.length

          val firstBlockIndex = fileBlock.dataBlocks.last
          val firstBlock = DataBlock(io.getBlock(firstBlockIndex))
          val firstBlockBuffer = ByteBuffer.wrap(firstBlock.data)
          firstBlockBuffer.position(lastBlockFilledBytes)
          firstBlockBuffer.put(bytes, 0, availableBytes)


          ( availableBytes, Vector((firstBlockIndex,DataBlock(firstBlockBuffer.array()))) )
        } else (0, Vector.empty)

      val bytesNeedBlocks = bytes.length - consumedBytes
      val blocksNeeded = common.ceilingDiv(bytesNeedBlocks, BLOCKSIZE)
      val blocksToFill = freeMap.takeBlocks(blocksNeeded)

      val updatedFileBlock = fileBlock.copy(
        dataBlocks = fileBlock.dataBlocks ++ blocksToFill,
        fileSize = fileBlock.fileSize + bytes.length)

      val updatedBlocks = Vector((fileEntry.address, updatedFileBlock)) ++ initialUpdatedBlock ++
        blocksToFill.foldLeft((Vector.empty[(Int,DataBlock)], consumedBytes)) { case ((acc,bytesOffset),blockIndex) =>
          val block = DataBlock(io.getBlock(blockIndex))
          val consume = BLOCKSIZE min (bytes.length-bytesOffset)
          ByteBuffer.wrap(block.data).put(bytes, bytesOffset, consume)
          (acc :+ (blockIndex,block), bytesOffset+consume)
        }._1

      io.writeBlocks(updatedBlocks)
    }
  }

  /** Read bytes from a file in given range of byte indices.
    * The range is [from,to) - from inclusive, to exclusive
    */
  def read(path: String, from: Int, to: Int): Array[Byte] = this.synchronized {
    import constants.BLOCKSIZE
    require(from >= 0, s"from-index must be positive. Was $from")
    require(to >= from, s"to-index ($to) must be at least from-index ($from)")

    val filePath = Path(path)

    val size = to - from
    val fromBlockIndex = from / BLOCKSIZE
    val toBlockIndex = (to / BLOCKSIZE) + 1
    val startOffset = from % BLOCKSIZE

    readWithIO { io =>
      val fileEntryOpt = fileFromRoot(io,filePath) // YOLO
      require(fileEntryOpt.isDefined, s"file '$path' does not exist")
      val fileEntry = fileEntryOpt.get
      require(!fileEntry.deleted, s"file '$path' does not exist")
      require(!fileEntry.dir, s"cannot append to '$path' because it is a directory")

      val fileBlock = FileBlock(io.getBlock(fileEntry.address))
      require(to <= fileBlock.fileSize, s"to position ($to) was greater than file size (${fileBlock.fileSize}")

      val out = ByteBuffer.allocate(size)

      fileBlock.dataBlocks
        .slice(fromBlockIndex, toBlockIndex)
        .foldLeft(startOffset) { (offset,address) =>
            val blockData = io.getBlock(address)
            blockData.position(offset)
            if (out.remaining() >= blockData.remaining()) out.put(blockData)
            else blockData.get(out.array(), out.arrayOffset() + out.position(), out.remaining())
            0
        }

      out.array()
    }
  }

  /** Find an entry slot at `path` and create a file or directory for the name part of path. */
  private def createFile(path: Path, dir: Boolean): Option[FileEntry] = {

    val parentPath = path.parent
    val name = path.name

    mutateWithIO { io =>

      val fileBlockAddressOpt = freeMap.takeBlocks(1).headOption

      val resultEntry = for {
        fileBlockAddress <- fileBlockAddressOpt
        (parentBlockAddress, parentBlock) <-
          if (parentPath.isRoot) Some((0,header))
          else fileFromRoot(io, Path(path.parts.init)).map { parentEntry =>
            (parentEntry.address, DirectoryIndexBlock(io.getBlock(parentEntry.address)))
          }

        fileBlock =
          if (dir) DirectoryIndexBlock(parentBlockAddress,Vector(),0)
          else FileBlock(parentBlockAddress, Vector(), 0)

        fileEntry = FileEntry(name, dir = dir, deleted = false, fileBlockAddress)

        (updatedBlockAddress, updatedBlock, updatedParent) <- findOrCreateBlockAndUpdateEntry(io, parentBlock, fileEntry)

      } yield {
        io.writeBlock(updatedBlockAddress, updatedBlock)
        io.writeBlock(fileBlockAddress, fileBlock)
        if (updatedParent != parentBlock) {
          // this block kind of YOLO but it's probably fine
          if (parentBlockAddress == 0) {
            header = updatedParent.asInstanceOf[HeaderBlock]
            io.writeBlock(parentBlockAddress,header)
          } else
            io.writeBlock(parentBlockAddress,updatedParent.asInstanceOf[DirectoryIndexBlock])
        }
        fileEntry
      }

      resultEntry.orElse {
        // release possibly taken block because there was error
        fileBlockAddressOpt.foreach(a => freeMap.freeBlocks(Vector(a)))
        None
      }

    }
  }


  /** Find a directory block among addresses that has a free or deleted entry slot
    * and update it with given entry.
    *
    * @return (updated block address, updated directory block, possibly updated parent)
    */
  private def findOrCreateBlockAndUpdateEntry(io: IO, parent: DirIndex, fileEntry: FileEntry): Option[(Int,DirectoryBlock,DirIndex)] = {
    parent.blockAddresses.iterator // iterator for lazy semantics
      .map { address => (address, DirectoryBlock(io.getBlock(address))) }
      .collectFirst {
        case (address, dirBlock) if dirBlock.files.exists(_.deleted) =>
          val index = dirBlock.files.indexWhere(_.deleted) // duplicated work with .exists
          val updatedFiles = dirBlock.files.updated(index, fileEntry)
          val updatedDirBlock = dirBlock.copy(files = updatedFiles)
          (address, updatedDirBlock, parent)

        case (address, dirBlock) if dirBlock.files.size < DirectoryBlock.MAX_ENTRIES =>
          val updatedDirBlock = dirBlock.copy(dirBlock.files :+ fileEntry)
          (address, updatedDirBlock, parent)
      }
      .orElse {
        // create a new dir block
        if (parent.blockCount < parent.maxBlockCount) {
          val newDirBlockAddressOpt = freeMap.takeBlocks(1).headOption
          newDirBlockAddressOpt.map { newDirBlockAddress =>
            val newDirBlock = DirectoryBlock(Vector(fileEntry))
            val updatedIndex = parent.addDirBlock(newDirBlockAddress)
            (newDirBlockAddress, newDirBlock, updatedIndex)
          }.orElse {
            // cleanup
            newDirBlockAddressOpt.foreach(a => freeMap.freeBlocks(Vector(a)))
            None
          }
        }
        else None
      }
  }


  private def fileFromRoot(io: IO, path: Path): Option[FileEntry] =
    fileForPath(io)(header.rootBlockAddresses, path.parts)

  private def readWithIO[A](f: IO => A): A = IO.withIO(physical)(f)

  /** Perform mutation operations with an IO, write the changed filesystem metadata, and close IO. */
  private def mutateWithIO[A](f: IO => A): A = IO.withIO(physical) { io =>
    val res = f(io)
    io.writeBlock(0,header) // write this regardless of what kind of change was performed because simplicity
    freeMap.flush(io)
    res
  }

}


object FFS {

  /**
    * Open an existing FFS or create a new one with given size.
    */
  def apply(physical: JFile, size: Int): FFS =
    if (physical.isFile) FFSCache(physical, open(physical))
    else FFSCache(physical, initialize(physical, size))

  /**
    * @param physical "physical" file system file
    * @param size size in bytes. Actual size will be rounded up to the next highest multiple of blockSize (512)
    * @return
    */
  private def initialize(physical: JFile, size: Int): FFS = {
    import constants.BLOCKSIZE, common.ceilingDiv

    require(!physical.isFile, s"file '$physical' already exists, will not initialize.")

    val nBlocks = ceilingDiv(size, BLOCKSIZE)

    physical.createNewFile()

    val freeMap = FreeMap(nBlocks)

    val rootBlockAddress = freeMap.takeBlocks(1).head
    val dummyFileBlockAddress = freeMap.takeBlocks(1).head

    val dummyFileEntry = FileEntry("foo", dir=false, deleted=false, dummyFileBlockAddress)
    val dummyFileBlock = FileBlock(rootBlockAddress, Vector.empty[Int], 0)
    val dummyFileInfo = (dummyFileBlockAddress, dummyFileBlock)

    val headerBlock = HeaderBlock(nBlocks,Vector(rootBlockAddress))
    val rootBlock = DirectoryBlock(Vector(dummyFileEntry))
    val rootBlockInfo = (rootBlockAddress, rootBlock)

    val blocks =
      Vector( (0,headerBlock) ) ++
      freeMap.addressedBlocks ++
      Vector(rootBlockInfo, dummyFileInfo)

    IO.withIO(physical) { io =>
      io.writeBlocks(blocks)
    }

    new FFS(physical, headerBlock, freeMap)
  }

  /**
    * Open an existing Fake File System
    *
    * @param physical underlying file for the FFS
    * @return
    */
  private def open(physical: JFile): FFS = {
    require(physical.isFile, s"'$physical' is not a regular file.")

    IO.withIO(physical) { io =>
      val header = HeaderBlock(io.getBlock(0))
      val freemap = FreeMap(io, header.blockCount)
      new FFS(physical, header, freemap)
    }
  }

}
