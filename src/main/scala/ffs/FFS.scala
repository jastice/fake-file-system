package ffs

import java.io.{File => JFile}

import ffs.impl._
import ffs.common.constants
import FFS._

/**
  * The Fake File System.
  *
  * Currently not thread-safe. Possible solution: make operations synchronized, check for / wait on file locks,
  * have only one instance per physical file.
  */
class FFS private(physical: JFile, private[ffs] var header: HeaderBlock, private[ffs] val freeMap: FreeMap) {


  /** List all files within `path`. If path is a file, list that. */
  def ls(path: String): Seq[FileNode] = {
    val myPath = Path(path)
    require(paths.valid(myPath), "not a valid path (filename too long?)")


    readWithIO { io =>
      def fileNodes(addresses: Vector[Int]) =
        fileEntries(io)(addresses)
          .map { e => if (e.dir) Directory(e.name) else File(e.name) }

      if (myPath.parts.isEmpty) // ls on root dir
        fileNodes(header.rootBlockAddresses)
      else {
        fileFromRoot(io,myPath)
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

  /** Recursively list all files below `path`, with their full path name. */
  def lsr(path: String): Seq[String] = ???

  /** Create directory in at path. Path must be directory itself. */
  def mkdir(path: String): Unit =
    createFile(Path(path), dir = true)

  /** Create empty file at path. */
  def touch(path: String): Unit =
    createFile(Path(path), dir = false)

  def cp(from: String, to: String): Unit = ???

  def mv(from: String, to: String): Unit = ???

  /** Delete a file. */
  def rm(path: String): Unit = {
    ???
  }

  /** Find an entry slot at `path` and create a file or directory for the name part of path. */
  private def createFile(path: Path, dir: Boolean): Unit = {

    val parentPath = path.parts.init
    val name = path.parts.last

    val fileBlockAddress = freeMap.takeBlocks(1).head // YOLO
    // TODO release block in error case

    // FIXME correct parent address pls

    mutateWithIO { io =>

      // TODO proper error on non-existent directory

      val (parentBlockAddress, parentBlock) =
        if (parentPath.isEmpty) (0,header)
        else {
          val parentEntry = fileFromRoot(io, Path(path.parts.init)).get // YOLO
          (parentEntry.address, DirectoryIndexBlock(io.getBlock(parentEntry.address)))
        }

      val fileBlock =
        if (dir) DirectoryIndexBlock(parentBlockAddress,Vector(),0)
        else FileBlock(parentBlockAddress, Vector(), 0)
      val fileEntry = FileEntry(name, dir = dir, deleted = false, fileBlockAddress)

      val updates = findOrCreateBlockAndUpdateEntry(io, parentBlock, fileEntry)

      updates.foreach { case (updatedBlockAddress, updatedBlock, updatedParent) =>
        io.writeBlock(updatedBlockAddress, updatedBlock)
        io.writeBlock(fileBlockAddress, fileBlock)
        if (updatedParent != parentBlock) {
          if (parentBlockAddress == 0) {
            header = updatedParent.asInstanceOf[HeaderBlock]
            io.writeBlock(parentBlockAddress,header)
          } else
            io.writeBlock(parentBlockAddress,updatedParent.asInstanceOf[DirectoryIndexBlock])
        }
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
        // create a new dirblock
        if (parent.blockCount < parent.maxBlockCount) {
          val newDirBlockAddress = freeMap.takeBlocks(1).head // YOLO
          val newDirBlock = DirectoryBlock(Vector(fileEntry))
          val updatedIndex = parent.addDirBlock(newDirBlockAddress)
          Some((newDirBlockAddress, newDirBlock, updatedIndex))
        }
        else None
      }
  }

  /** Find directory block containing file with `name`. */
  private def dirBlockForName(io: IO, addresses: Vector[Int], name: String): Option[(Int,DirectoryBlock)] = {
    addresses.iterator
      .map { a => (a, DirectoryBlock(io.getBlock(a))) }
      .find { case (address,dirBlock) => dirBlock.files.exists(_.name == name) }
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
    if (physical.isFile) open(physical)
    else initialize(physical, size)

  /**
    *
    * @param physical "physical" file system file
    * @param size size in bytes. Actual size will be rounded up to the next highest multiple of blockSize (512)
    * @return
    */
  def initialize(physical: JFile, size: Int): FFS = {
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
  def open(physical: JFile): FFS = {
    require(physical.isFile, s"'$physical' is not a regular file.")

    IO.withIO(physical) { io =>
      val header = HeaderBlock(io.getBlock(0))
      val freemap = FreeMap(io, header.blockCount)
      new FFS(physical, header, freemap)
    }
  }

  /** Non-deleted file entries in directory blocks. */
  private def fileEntries(io: IO)(addresses: Vector[Int]): Vector[FileEntry] = {
    addresses
      .flatMap { a => DirectoryBlock(io.getBlock(a)).files }
      .filter(!_.deleted)
  }


  /** Get the file entry describing the file referenced by `path` among `addresses`, recursively. */
  private def fileForPath(io: IO)(addresses: Vector[Int], path: Vector[String]): Option[FileEntry] = {
    if (path.isEmpty) None
    else {
      val name = path.head
      addresses
        .flatMap { a => DirectoryBlock(io.getBlock(a)).files }
        .find { case entry => !entry.deleted && entry.name == name }
        .flatMap { case entry =>
          val restPath = path.tail
          if (restPath.nonEmpty) {
            if (entry.dir) {
              val block = DirectoryIndexBlock(io.getBlock(entry.address))
              fileForPath(io)(block.blockAddresses, restPath)
            } else None // not a dir here, can't complete path recursion
          }
          else Some(entry)
        }
    }
  }

}
