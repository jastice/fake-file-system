package ffs

import java.nio.charset.Charset

/**
  * Constants and magic values.
  */
object constants {

  /** The block size in bytes. */
  val blockSize: Int = 512

  /** Marker for FFS format. */
  val magic: Short = 0x0FF5.toShort

  /** Version of FFS format */
  val version: Byte = 0.toByte

  val filenameBytes: Int = 8

  val charset = Charset.forName("UTF-8")

}
