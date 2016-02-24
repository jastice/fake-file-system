package ffs

import java.nio.file.{Path => NioPath}
import java.nio.file.Files
import java.nio.file.StandardOpenOption.{READ,WRITE}

class IO(file: NioPath) {

  val channel = Files.newByteChannel(file, READ, WRITE)

  def writeBlocks(locBlocks: Vector[(Int,Block)]) = {
    locBlocks.foreach { case (address,block) =>
      writeBlock(address, block)
    }
  }

  def writeBlock(address: Int, block: Block) = {
    val byteAddress = address * constants.blockSize
    val written = channel.position(byteAddress).write(block.toBytes)
    assert(written == constants.blockSize)
  }

  def readBlock = ???

  def close() = channel.close()

}
