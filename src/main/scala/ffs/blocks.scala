package ffs

import java.nio.ByteBuffer
import blocks._
import constants._

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
    b.putShort(magic) // 2B
    b.put(version) // 1B
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
  // header block object from existing bytes
  def apply(bytes: Array[Byte]) = ???
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
    dataBlocks.foreach(b.putInt)

  }
}

object FileBlock {
  def apply(bytes: Array[Byte]) = ???
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
  def apply(bytes: Array[Byte]) = ???
}

/**
  * Holds raw file data, just that.
  * @param data pure, raw, beautiful data
  */
case class DataBlock(data: Array[Byte]) extends Block {
  override def toBytes = ByteBuffer.wrap(data).asReadOnlyBuffer()
}

/**
  * A file or directory entry in a DirectoryBlock.
  * @param name
  * @param dir
  * @param deleted
  * @param address
  */
case class FileEntry(name: String, dir: Boolean, deleted: Boolean, address: Int) {

  def toBytes: Array[Byte] = {
    val flags = flagBytes(dir, deleted)
    val nameBytes = name.getBytes(charset)

    val b = entryBuffer()
    b.putInt(flags)
    b.put(nameBytes)
    b.putInt(address)

    b.array()
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


  /** An int as block address. */
  def address(a: Int) : Short = a.toShort

  /** Allocate a buffer with standard size for the FFS format. */
  def blockBuffer(): ByteBuffer = ByteBuffer.allocate(blockSize)
  def entryBuffer(): ByteBuffer = ByteBuffer.allocate(16)

  /**
    * Apply a series of operations on a ByteBuffer and return a read-only version of the buffer with position set to 0.
    * @param f operations to apply
    * @return
    */
  def returningBuffer(f: ByteBuffer => Unit): ByteBuffer = {
    val b = blockBuffer()
    f(b)
    b.rewind()
    b.asReadOnlyBuffer()
  }


  def fromBytes(bytes: Array[Byte]): Block = ???
}
