package ffs.common

import java.nio.charset.StandardCharsets

/**
  * Constants and magic values.
  */
object constants {

  /** The block size in bytes in the file system. */
  val BLOCKSIZE: Int = 512

  /** Marker for FFS format. */
  val MAGIC: Short = 0x0FF5.toShort

  /** Version of FFS format */
  val VERSION: Short = 0

  /** Number of bytes reserved for file names. */
  val FILENAME_BYTES: Int = 8

  /** The character set for string serialization. */
  val CHARSET = StandardCharsets.UTF_8

}
