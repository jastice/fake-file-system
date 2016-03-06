package ffs

import java.io.{File => JFile}

import ffs.impl._
import ffs.common.constants

/**
  * The Fake File System.
  */
class FFS private(physical: JFile, private[ffs] val header: HeaderBlock, private[ffs] val freeMap: FreeMap) {

  /** Set a block as blocked or free in the freeMap. */
  private def updateBlock(address: Int, blocked: Boolean): Unit = ???


  /** List all files within `path`. */
  def ls(path: String): Seq[FileNode] = {
    val p = Path(path)

    // TODO recurse to given path

    readWithIO { io =>
      header.rootBlockAddresses
        .flatMap { a => DirectoryBlock(io.getBlock(a)).files }
        .filter(!_.deleted)
        .map { e => if (e.dir) Directory(e.name) else File(e.name, 0) }
    }
  }

  /** Recursively list all files below `path`, with their full path name. */
  def lsr(path: String): Seq[String] = ???

  /** Create directory in under `path`. Path must be directory itself. */
  def mkdir(path: String): FFS = ???

  /** Create empty file in directory `path`. */
  def touch(path: String): FFS = ???

  def cp(from: String, to: String): FFS = ???

  def mv(from: String, to: String): FFS = ???

  /** Delete a file. */
  def rm(path: String): FFS = {
    // mark file as deleted in index
    // mark all its blocks as free
    ???
  }

  /** Perform reading IO operations, and close IO afterward. */
  private def readWithIO[A](f: IO => A): A = {
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


//  def flush(): FFS = ???

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

    val nBlocks = ceilingDiv(size, BLOCKSIZE)
    val sizeRounded = nBlocks*BLOCKSIZE

    physical.createNewFile()


    val headerBlock = HeaderBlock(nBlocks,Vector(2))

    val freeMap = FreeMap(sizeRounded)

    val rootBlockAddress = freeMap.takeBlocks(1).head
    val dummyFileBlockAddress = freeMap.takeBlocks(1).head

    val dummyFileEntry = FileEntry("foo", dir=false, deleted=false, dummyFileBlockAddress)
    val dummyFileBlock = FileBlock(rootBlockAddress, Vector.empty[Int], 0)
    val dummyFileInfo = (dummyFileBlockAddress, dummyFileBlock)

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
    * @param physical
    * @return
    */
  def open(physical: JFile): FFS = {
    require(physical.isFile, s"'$physical' is not a regular file.")

    val io = new IO(physical)

    val header = HeaderBlock(io.getBlock(0))
    val freemap = FreeMap(io, header.blockCount)

    new FFS(physical, header, freemap)
  }

}
