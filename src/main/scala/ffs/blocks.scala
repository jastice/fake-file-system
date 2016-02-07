package ffs

import java.nio.ByteBuffer

// low-level representation of files in the FFS

sealed abstract class Block {
  lazy val buffer: ByteBuffer = ByteBuffer.allocate(512)
  def toBytes: Array[Byte]
}
case class HeaderBlock(layoutBlockAddresses: Vector[Int]) extends Block {

  val format = 0x0FF5.toShort
  val version = 0.toByte

  override def toBytes = {
    buffer.putShort(format)
    buffer.put(version)
    buffer.put(layoutBlockAddresses.size.toByte) // assuming maximum size fits into a byte...
    layoutBlockAddresses.foreach { addr =>
      buffer.putShort(addr.toShort)
    }
    // file format marker (2b)
    // file format version (1b)
    // layout block count (1b)
    // layout block addresses as 2b unsigned ints
    ???
  }
}

case class LayoutBlock(files: Vector[FileNode]) extends Block {
  override def toBytes = {
    // layout block marker
    // files:
      // file name (0-terminated)
      // file data total size in bytes
      // file data block addresses
    ???
  }
}

case class DataBlock(data: Array[Byte]) extends Block {
  override def toBytes = data
}

case class FileNode(name: String, size: Int, addresses: Vector[Int]) {
  def toBytes: Array[Byte] = ???
}

object bla {

  ByteBuffer.allocate(1).
}
