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
class FFS private(physical: JFile, private[ffs] val header: HeaderBlock, private[ffs] val freeMap: FreeMap) {

  /** Set a block as blocked or free in the freeMap. */
  private def updateBlock(address: Int, blocked: Boolean): Unit = ???


  /** List all files within `path`. If path is a file, list that. */
  def ls(path: String): Seq[FileNode] = {
    val myPath = Path(path)
    require(paths.valid(myPath))


    withIO { io =>
      def fileNodes(addresses: Vector[Int]) =
        fileEntries(io)(addresses)
          .map { e => if (e.dir) Directory(e.name) else File(e.name) }

      if (myPath.parts.isEmpty) // ls on root dir
        fileNodes(header.rootBlockAddresses)
      else {
        fileFromRoot(io)(header,myPath)
          .map { case (entry,block) =>
            if (entry.deleted)
              Vector.empty[FileNode]
            else if (entry.dir)
              fileNodes(block.dataBlocks)
            else Vector(File(entry.name))
      }

      }.getOrElse(Vector.empty[FileNode])
    }
  }

  /** Recursively list all files below `path`, with their full path name. */
  def lsr(path: String): Seq[String] = ???

  /** Create directory in at path. Path must be directory itself. */
  def mkdir(path: String): Unit = ???

  /** Create empty file at path. */
  def touch(path: String): Unit = {
    val pathParts = Path(path)

    mutateWithIO { io =>
      header.rootBlockAddresses
    }
  }

  def cp(from: String, to: String): Unit = ???

  def mv(from: String, to: String): Unit = ???

  /** Delete a file. */
  def rm(path: String): Unit = {
    // mark file as deleted in index
    // mark all its blocks as free
    ???
  }

  private def withIO[A](f: IO => A): A = IO.withIO(physical)(f)

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


  private def fileFromRoot(io: IO)(header: HeaderBlock, path: Path): Option[(FileEntry,FileBlock)] =
    fileForPath(io)(header.rootBlockAddresses, path.parts)

  /** Get the file entry and block describing the file referenced by `path` below `parent`. */
  private def fileForPath(io: IO)(addresses: Vector[Int], path: Vector[String]): Option[(FileEntry,FileBlock)] = {
    if (path.isEmpty) None
    else {
      val name = path.head
      addresses
        .flatMap { a => DirectoryBlock(io.getBlock(a)).files }
        .find { entry => !entry.deleted && entry.name == name }
        .flatMap { entry =>
          val restPath = path.tail
          val block = FileBlock(io.getBlock(entry.address))
          if (restPath.isEmpty) Some((entry,block))
          else if (entry.dir) fileForPath(io)(block.dataBlocks, restPath)
          else None // not a dir here, can't complete path recursion
        }
    }
  }

}
