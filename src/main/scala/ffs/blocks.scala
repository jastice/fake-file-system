package ffs

import java.nio.ByteBuffer

// low-level representation of files in the FFS

sealed abstract class Block {

  /** Allocate a buffer with standard size for the FFS format. */
  def buffer(): ByteBuffer = ByteBuffer.allocate(blocks.blockSize)
  def toBytes: Array[Byte]
}

/**
  * The first block in the file.
  * Contains some information about the file format and addresses of "layout blocks".
  *
  * @param rootBlockAddresses block addresses of layout blocks
  */
case class HeaderBlock(rootBlockAddresses: Vector[Int]) extends Block {

  import blocks._

  override def toBytes = {
    val b = buffer()
    b.putShort(magic) // 2b
    b.put(version) // 1b

    // the root block gets up to 127 directory blocks
    // TODO handle too many root blocks errors
    b.put(rootBlockAddresses.size.toByte) // 1b

    // leaves us 508 bytes, space for 127 4-byte block addresses
    rootBlockAddresses.foreach { a =>
      b.putInt(a) // 4B
    }

    b.array()
  }
}

/** A file block represents either a regular file with data or a directory. */
case class FileBlock(dataBlocks: Vector[Block]) extends Block {
  override def toBytes = {
    // file size in bytes (4B)

    ???
  }
}

/** A directory block contains data about files and directories within a directory. */
case class DirectoryBlock(files: Vector[Entry]) extends Block {
  override def toBytes = {
    //
    // sequence of files and dirs
    // dirs:
      // flags (1B)
      // dir name (8B)
      // dir block address (4B)
    // files:
      // flags (1B)
      // file name (8B)
      // file block address (4B)
      // file data total size in bytes (4B)
      // file data block addresses (2B * fileSize/512)

    val b = buffer()

    b.array()
  }
}



case class DataBlock(data: Array[Byte]) extends Block {
  override def toBytes = data
}

sealed abstract class Entry {
  val name: String

  def toBytes: Array[Byte]
}

case class DirEntry(name: String, deleted: Boolean, blocks: Vector[Int]) extends Entry {
  override def toBytes = ???
}

case class FileEntry(name: String, deleted: Boolean, size: Int, addresses: Vector[Int]) extends Entry {
  override def toBytes: Array[Byte] = ???
}

object blocks {

  object flags {
    val dir: Int = 0x01
    val deleted: Int = 0x02
  }

  def flagByte(fDir: Boolean, fDeleted: Boolean): Byte = {
    import flags._
    (
      (if (fDir) dir else 0) |
      (if (fDeleted) deleted else 0)
    ).toByte
  }

  /** The block size in bytes. */
  val blockSize: Int = 512

  /** Marker for FFS format. */
  val magic: Short = 0x0FF5.toShort

  /** Version of FFS format */
  val version: Byte = 0.toByte

  /** An int as block address. */
  def address(a: Int) : Short = a.toShort
}