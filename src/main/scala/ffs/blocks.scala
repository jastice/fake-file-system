package ffs

import java.nio.ByteBuffer

// low-level representation of files in the FFS

sealed abstract class Block {

  /** Allocate a buffer with standard size for the FFS format. */
  def buffer(): ByteBuffer = ByteBuffer.allocate(512)
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
    b.putShort(format) // 2b
    b.put(version) // 1b

    // assuming maximum size fits into a byte...
    // TODO handle too many root blocks errors
    b.put(rootBlockAddresses.size.toByte) // 1b

    // leaves us 509 bytes, space for 254 2-byte bock addresses

    rootBlockAddresses.foreach { a =>
      b.putShort(address(a)) // 2b quasi-unsigned short each
    }

    b.array()
  }
}

case class LayoutBlock(files: Vector[Node]) extends Block {
  override def toBytes = {
    // layout block marker
    // sequence of files and dirs
    // dirs:
      // dir marker (1B)
      // dir name (8B)
      // dir block count
      // dir layout blocks
    // files:
      // file marker (1B)
      // file name (8B)
      // file data total size in bytes (4B)
      // file data block addresses (2B * fileSize/512)

    val b = buffer()

    b.array()
  }
}

case class DataBlock(data: Array[Byte]) extends Block {
  override def toBytes = data
}

sealed abstract class Node {
  val name: String

  def marker: Byte // wasteful, I know :)
  def toBytes: Array[Byte]
}

case class DirNode(name: String, blocks: Vector[Int]) extends Node {
  override def marker = 0.toByte
  override def toBytes = ???
}

case class FileNode(name: String, size: Int, addresses: Vector[Int]) extends Node {
  override def marker = 1.toByte
  override def toBytes: Array[Byte] = ???
}

object blocks {

  /** Marker for FFS format. */
  val format = 0x0FF5.toShort

  /** Version of FFS format */
  val version = 0.toByte

  /** An int as block address. */
  def address(a: Int) : Short = a.toShort
}