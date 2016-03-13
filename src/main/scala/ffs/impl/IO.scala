package ffs.impl

import java.io.{File => JFile, RandomAccessFile}
import java.nio.ByteBuffer
import java.nio.channels.FileChannel.MapMode

import ffs.common.constants
import ffs.common.constants.BLOCKSIZE

/**
  * IO operations: writing and reading blocks by their address.
  * This is the only place actual writing and reading from file happens.
  *
  * @param file file to operate on
  */
class IO private(file: JFile) {

  val raf = new RandomAccessFile(file,"rw")
  val channel = raf.getChannel
  val lock = channel.lock() // note: locks won't help controlling file access within the JVM

  /** Write multiple blocks at respective block addresses. */
  def writeBlocks(locBlocks: Vector[(Int,Block)]) = {
    locBlocks.foreach { case (address,block) =>
      writeBlock(address, block)
    }
  }

  /** Write a Block to the given block address. */
  def writeBlock(address: Int, block: Block) = {
    val byteAddress = address * BLOCKSIZE
    val written = channel.write(block.toBytes, byteAddress)
    assert(written == BLOCKSIZE)
  }

  /** Get a read-only ByteBuffer corresponding to one block. */
  def getBlock(address: Int): ByteBuffer = {
    val byteAddress = address * BLOCKSIZE
    channel.map(MapMode.READ_ONLY, byteAddress, BLOCKSIZE)
  }

  private def close() = {
    lock.release()
    channel.close()
    raf.close()
  }

}


object IO {

  /** Perform IO operations, and close IO afterward.
    * IO can only be used with this function, so you don't forget to close it.
    */
  def withIO[A](file: java.io.File)(f: IO => A): A = {
    // TODO readonly IO type
    val io = new IO(file)
    try { f(io) }
    finally { io.close() }
  }

}
