package ffs.impl

import java.nio.ByteBuffer

import ffs.impl.blocks._
import ffs.common.constants._
import ffs.common.ceilingDiv

// low-level representation of files in the FFS

sealed abstract class Block {

  /** Byte representation of this block. */
  def toBytes: ByteBuffer
}

/**
  * The first block in the file.
  * Contains some information about the file format and addresses of "layout blocks".
  *
  * @param blockCount number of blocks in the fake file system
  * @param rootBlockAddresses block addresses of directory root
  */
case class HeaderBlock(blockCount: Int, rootBlockAddresses: Vector[Int]) extends Block {

  private val availableRootBlockAddresses = (BLOCKSIZE - 12) / 4

  override def toBytes = {
    val rbaSize = rootBlockAddresses.size
    require(rbaSize <= availableRootBlockAddresses,
      s"tried to create HeaderBlock with too many root directory blocks ($rbaSize). Maximum available: $availableRootBlockAddresses")

    returningBuffer { b =>
      b.putShort(MAGIC) // 2B
      b.putShort(VERSION) // 2B
      b.putInt(blockCount) // 4B
      // block count determines how many Bitmap Blocks are allocated in the FFS: blockCount/(8*blockSize)

      b.putInt(rbaSize) // 4B

      // leaves us 500 bytes, space for 125 4-byte block addresses
      rootBlockAddresses.foreach { a =>
        b.putInt(a) // 4B
      }
    }
  }
}

object HeaderBlock {

  def apply(bytes: ByteBuffer): HeaderBlock = {
    val myMagic = bytes.getShort
    require(myMagic == MAGIC, s"Unrecognized magic: $myMagic. Not a Fake File System file?")
    //noinspection ScalaUnusedSymbol
    val myVersion = bytes.getShort // we'll just ignore version for this implementation
    val myBlockCount = bytes.getInt
    val myRootBlockAddressesCount = bytes.getInt

    // using map somehow messes up the buffer state
    val myRootBlockAddresses = (0 until myRootBlockAddressesCount)
      .foldLeft(Vector.empty[Int]) { (acc,i) => acc :+ bytes.getInt }

    HeaderBlock(myBlockCount, myRootBlockAddresses)
  }
}


/** A file block represents either a regular file or a directory.
  *
  * @param parentBlock the parent directory of this file
  * @param dataBlocks addresses of data or directory blocks
  * */
case class FileBlock(parentBlock: Int, dataBlocks: Vector[Int], fileSize: Int) extends Block {
  override def toBytes = {
    require(dataBlocks.size <= FileBlock.maxFileBlocks)

    returningBuffer { b =>
      b.putInt(parentBlock)
      b.putInt(fileSize)

      // TODO check file size
      // effectively, files are limited to 126 blocks ~ 64k -- enough for this implementation :)
      // in real unix file system, we have a kind of tree of blocks to allow larger sizes
      dataBlocks.foreach(b.putInt)
    }
  }
}

object FileBlock {

  val maxFileBlocks = (BLOCKSIZE - 8) / 4 // 2 ints are reserved, 2 int per block address

  def apply(bytes: ByteBuffer): FileBlock = {
    val myParent = bytes.getInt
    val mySize = bytes.getInt
    val nBlocks = ceilingDiv(mySize,BLOCKSIZE)

    val myDataBlocks = (0 until nBlocks).foldLeft(Vector.empty[Int]) { (acc,i) => acc :+ bytes.getInt }
    FileBlock(myParent, myDataBlocks, mySize)
  }
}

/** A directory block contains data about files and directories within a directory. */
case class DirectoryBlock(files: Vector[FileEntry]) extends Block {
  override def toBytes = returningBuffer { b =>
    // TODO ensure this fits into the block
    files.foreach(e => b.put(e.toBytes))
    // the buffer should ensure this is 0-padded
  }
}

object DirectoryBlock {

  private def readEntries(collected: Vector[FileEntry], bytes: ByteBuffer): Vector[FileEntry] = {
    FileEntry(bytes) match {
      case Some(e: FileEntry) => readEntries(collected :+ e, bytes)
      case None => collected
    }
  }

  def apply(bytes: ByteBuffer): DirectoryBlock =
    DirectoryBlock(readEntries(Vector.empty[FileEntry], bytes))
}

/**
  * Holds raw file data, just that.
  *
  * @param data pure, raw, beautiful data
  */
case class DataBlock(data: Array[Byte]) extends Block {
  override def toBytes = ByteBuffer.wrap(data).asReadOnlyBuffer()
}

object DataBlock {
  def apply(bytes: ByteBuffer): DataBlock = {
    val arr = Array.ofDim[Byte](BLOCKSIZE)
    bytes.get(arr)
    DataBlock(arr)
  }
}

/**
  * A file or directory entry in a DirectoryBlock.
  *
  * @param name file name. Must be serializable as 8 bytes
  * @param dir is this file a directory?
  * @param deleted is this file deleted?
  * @param address address of file block
  */
case class FileEntry(name: String, dir: Boolean, deleted: Boolean, address: Int) {

  def toBytes: Array[Byte] = {
    val flags = flagBytes(dir, deleted)
    val nameBytes = name.getBytes(CHARSET)
    require(nameBytes.length <= FILENAME_BYTES, s"filename '$name' too long, can be at most $FILENAME_BYTES bytes")

    val b = entryBuffer()
    b.putInt(flags)
    b.putInt(address)
    b.put(nameBytes)

    b.array()
  }
}

object FileEntry {

  val ENTRY_BYTES = 4 + FILENAME_BYTES + 4 // flags + name + address

  def apply(bytes: ByteBuffer): Option[FileEntry] = {

    val myFlags = bytes.getInt
    val myAddress = bytes.getInt
    val nameArr = Array.ofDim[Byte](FILENAME_BYTES)
    bytes.get(nameArr)
    val truncatedName = nameArr.takeWhile(_ != 0) // assume string is 0-terminated

    if (myAddress != 0) {
      val myName = new String(truncatedName, CHARSET)
      val fDel = isDeleted(myFlags)
      val fDir = isDir(myFlags)
      Some(FileEntry(myName, fDir, fDel, myAddress))
    } else None
  }
}

object blocks {

  object flags {
    val dir: Int = 0x01
    val deleted: Int = 0x02
  }

  def flagBytes(fDir: Boolean, fDeleted: Boolean): Int = {
    import flags._

    (if (fDir) dir else 0) |
    (if (fDeleted) deleted else 0)
  }

  def isDir(f: Int): Boolean = (flags.dir & f) != 0
  def isDeleted(f: Int): Boolean = (flags.deleted & f) != 0


  /** An int as block address. */
  def address(a: Int) : Short = a.toShort

  /** Allocate a buffer with standard size for the FFS format. */
  def blockBuffer(): ByteBuffer = ByteBuffer.allocate(BLOCKSIZE)
  def entryBuffer(): ByteBuffer = ByteBuffer.allocate(FileEntry.ENTRY_BYTES)

  /**
    * Apply a series of operations on a ByteBuffer and return a read-only version of the buffer with position set to 0.
    *
    * @param f operations to apply
    * @return
    */
  def returningBuffer(f: ByteBuffer => Unit): ByteBuffer = {
    val b = blockBuffer()
    f(b)
    b.rewind()
    b.asReadOnlyBuffer()
  }

}
