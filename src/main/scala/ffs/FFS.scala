package ffs

import java.io.{File => JFile}

import ffs.impl._
import ffs.common.constants

/**
  * The Fake File System.
  *
  * Currently not thread-safe. Possible solution: make operations synchronized, check for / wait on file locks,
  * have only one instance per physical file.
  */
class FFS private(physical: JFile, private[ffs] val header: HeaderBlock, private[ffs] val freeMap: FreeMap) {

  /** Set a block as blocked or free in the freeMap. */
  private def updateBlock(address: Int, blocked: Boolean): Unit = ???


  /** List all files within `path`. */
  def ls(path: String): Seq[FileNode] = {
    val pathParts = Path(path)

    // TODO recurse to given path

    readWithIO { io =>
      header.rootBlockAddresses
        .flatMap { a => DirectoryBlock(io.getBlock(a)).files }
        .filter(!_.deleted)
        .map { e => if (e.dir) Directory(e.name) else File(e.name) }
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

  /** Perform reading IO operations, and close IO afterward. */
  private[ffs] def readWithIO[A](f: IO => A): A = {
    // TODO readonly IO type
    val io = new IO(physical)
    val res = f(io)
    io.close()
    res
  }

  /** Perform mutation operations with an IO, write the changed filesystem metadata, and close IO. */
  private def mutateWithIO[A](f: IO => A): A = {
    val io = new IO(physical)
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

    val io = new IO(physical)
    io.writeBlocks(blocks)
    io.close()

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

    val io = new IO(physical)

    val header = HeaderBlock(io.getBlock(0))
    val freemap = FreeMap(io, header.blockCount)

    new FFS(physical, header, freemap)
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
