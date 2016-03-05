package ffs

import java.nio.ByteBuffer
import blocks._
import constants._
import common.ceilingDiv

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

  override def toBytes = returningBuffer { b =>
    b.putShort(MAGIC) // 2B
    b.put(VERSION) // 1B
    b.putInt(blockCount) // 4B
    // block count determines how many Bitmap Blocks are allocated in the FFS: blockCount/(8*blockSize)

    // TODO handle too many root blocks errors
    b.put(rootBlockAddresses.size.toByte) // 1b

    // leaves us 508 bytes, space for 127 4-byte block addresses
    rootBlockAddresses.foreach { a =>
      b.putInt(a) // 4B
    }
  }
}

object HeaderBlock {

  def apply(bytes: ByteBuffer): HeaderBlock = {
    val myMagic = bytes.getShort
    require(myMagic == MAGIC, s"Unrecognized magic: $myMagic. Not a Fake File System file?")
    //noinspection ScalaUnusedSymbol
    val myVersion = bytes.get // we'll just ignore this for this implementation
    val myBlockCount = bytes.getInt
    val myRootBlockAddressesCount = bytes.get
    val myRootBlockAddresses = (0 until myRootBlockAddressesCount).map(bytes.getInt).toVector
    HeaderBlock(myBlockCount, myRootBlockAddresses)
  }
}

/** A file block represents either a regular file with data or a directory. */
case class FileBlock(parentBlock: Int, dataBlocks: Vector[Int], fileSize: Int) extends Block {
  override def toBytes = returningBuffer { b =>
    // parent directory block (4B)
    // file size in bytes (4B)
    // file block addresses (4B * size/512)

    b.putInt(parentBlock)
    b.putInt(fileSize)

    // TODO check file size
    // effectively, files are limited to 126 blocks ~ 64k -- enough for this implementation :)
    // in real unix file system, we have a kind of tree of blocks to allow larger sizes
    dataBlocks.foreach(b.putInt)
  }

}

object FileBlock {
  def apply(bytes: ByteBuffer): FileBlock = {
    val myParent = bytes.getInt
    val mySize = bytes.getInt
    val nBlocks = ceilingDiv(mySize,BLOCKSIZE)
    val myDataBlocks = (0 until nBlocks).map(bytes.getInt).toVector
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

    val b = entryBuffer()
    b.putInt(flags)
    b.put(nameBytes)
    b.putInt(address)

    b.array()
  }
}

object FileEntry {

  val ENTRY_BYTES = 4 + FILENAME_BYTES + 4 // flags + name + address

  def apply(bytes: ByteBuffer): Option[FileEntry] = {

    val myFlags = bytes.getInt
    val nameArr = Array.ofDim[Byte](FILENAME_BYTES)
    bytes.get(nameArr)
    val myAddress = bytes.getInt

    if (myAddress != 0) {
      val myName = new String(nameArr, CHARSET)
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
  def entryBuffer(): ByteBuffer = ByteBuffer.allocate(16)

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
