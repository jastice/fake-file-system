package ffs

import ffs.constants._
import FreeMap._

class FreeMap(blocks: Vector[DataBlock], size: Int) {

  private val firstBlockAddress = 1
  private val blockBits = blockSize*8

  private var free: Int = countFree

  private def blockAddress(address: Int): (Int,Int) = (address / blockBits, address % blockBits)

  private def countFree: Int = {
    val oneBits = blocks.foldLeft(0) { (sum,block) =>
      sum + block.data.foldLeft(0) { (s,b) => s + num1Bits(b)}
    }
    size - oneBits
  }

  /**
    * @param block block to set bit in
    * @param bit bit to set
    * @return whether the bit was set in this operation (true) or already set (false)
    */
  private def setBit(block: DataBlock, bit: Int): Boolean = {
    val byteAddress = bit >> 3
    val b = 1 << (bit & 0x7)
    val byt = block.data(byteAddress)
    block.data.update(byteAddress, (b | byt).toByte)
    (byt & b) > 0
  }

  /**
    *
    * @param block
    * @param bit
    * @return whether bit was cleared (true) or already clear (false)
    */
  private def clearBit(block: DataBlock, bit: Int): Boolean = {
    val byteAddress = bit >> 3
    val b = ~(1 << (bit & 0x7))
    val byt = block.data(byteAddress)
    block.data.update(byteAddress, (b & byt).toByte)
    (byt & b) > 0
  }

  /** Find `blocks` free blocks and mark them as blocked.
    * If not enough free blocks are available, return an empty vector. */
  def takeBlocks(n: Int): Vector[Int] = {
    if (free < n) Vector.empty
    else {

      var result = Vector.empty[Int]
      var collected = 0
      var b = 0

      while(b < blocks.size) {
        val data = blocks(b).data
        val blockZero = blockBits*b
        var i = 0
        while (blockZero + i < size && collected < n) {
          val byt = data(i)
          val added = (if (byt != 0xFF.toByte) freeBits(byt) else Vector.empty).map{ k => blockZero + k }
          result ++= added
          collected += added.size
          i += 1
        }
        b += 1
      }

      result.take(n)
    }
  }

  /** Free bit indexes in a byte. */
  def freeBits(b: Byte): Vector[Int] = {
    (0 until 8).filter { i =>
      val m = 1 << i
      (m & b) > 1
    }.toVector
  }

  /**
    * Free the blocks with given block addresses. They are free for writing again.
    *
    * @return updated block index and block
    */
  def freeBlocks(freeUs: Vector[Int]): Vector[(Int, Block)] = {
    val freed = freeUs.flatMap { address =>
      val (blockAddr,addr) = blockAddress(address)
      val block = blocks(blockAddr)
      val cleared = clearBit(block, addr)
      if (cleared) Vector((firstBlockAddress+blockAddr, block)) else Vector.empty
    }

    free -= freed.size
    freed
  }

  def addressedBlocks: Vector[(Int,DataBlock)] =
    blocks.zipWithIndex.map{ case (block,addr) => (firstBlockAddress+addr, block)}
}


object FreeMap {

  /**
    * Initialize a FreeMap for given size of blocks
    */
  def apply(size: Int) = {

    val nFreeMapBlocks = common.ceilingDiv(size, blockSize * 8)

    val freeMapBlocks = Vector.fill(nFreeMapBlocks)(DataBlock(Array.ofDim(blockSize)))
    val reservedBlocks = 1 + freeMapBlocks.size // header block + FreeMap itself
    val freeMap = new FreeMap(freeMapBlocks, size)
    val firstBlock = freeMapBlocks(0)
    (0 until reservedBlocks).map { i => freeMap.setBit(firstBlock, i) }

    freeMap
  }


  /**
    * Count 1-bits in an int
    * via http://codegolf.stackexchange.com/questions/4588/compute-number-of-1-bits-in-a-binary-representation-of-a-number
    * @param x
    * @return
    */
  private def num1Bits(x: Int) = {
    var i = x
    i = (i & 0x55555555) + ((i>> 1) & 0x55555555)
    i = (i & 0x33333333) + ((i>> 2) & 0x33333333)
    i = (i & 0x0f0f0f0f) + ((i>> 4) & 0x0f0f0f0f)
    i = (i & 0x00ff00ff) + ((i>> 8) & 0x00ff00ff)
    (i & 0x0000ffff) + ((i>>16) & 0x0000ffff)
  }

}