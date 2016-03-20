package ffs.impl

import ffs.common.constants.BLOCKSIZE
import ffs.impl.FreeMap._
import ffs.common

/**
  * Map of free blocks in filesystem.
  *
  * @param blocks blocks comprising the FreeMap
  * @param size Size of file system in blocks
  */
class FreeMap(private[ffs] val blocks: Vector[DataBlock], size: Int) {

  private val firstBlockAddress = 1
  private val blockBits = BLOCKSIZE*8

  private[ffs] var free: Int = countFree

  private[ffs] def countFree: Int = {
    val oneBits = blocks.foldLeft(0) { (sum,block) =>
      sum + block.data.foldLeft(0) { (s,b) => s + num1Bits(b)}
    }
    size - oneBits
  }


  /** Find `blocks` free blocks and mark them as taken.
    * If not enough free blocks are available, return an empty vector.
    * Only this method should be used to reserve blocks.
    */
  def takeBlocks(n: Int): Vector[Int] = {
    if (free < n) Vector.empty
    else {

      var result = Vector.empty[Int]
      var collected = 0
      var b = 0

      while(b < blocks.size && collected < n) {
        val data = blocks(b).data
        val blockZero = blockBits*b
        // the last block may not be completely filled
        var i = 0
        while (i < BLOCKSIZE && collected < n) {
          val byte = data(i)
          val added = (if (byte != 0xFF.toByte) freeBits(byte) else Vector.empty).map{ k => blockZero + i*8 + k }
          result ++= added
          collected += added.size
          i += 1
        }
        b += 1
      }

      val resultBlocks = result.take(n)
      assert(resultBlocks.size == n, s"result size was ${result.size}, expected $n")

      resultBlocks.foreach { blk =>
        val (blockAddr,addrInBlock) = blockAddress(blk)
        setBit(blocks(blockAddr),addrInBlock)
      }
      free -= n

      resultBlocks
    }
  }


  /**
    * Free the blocks with given block addresses. They are free for writing again.
    * Only this method should be used to free blocks.
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

    free += freed.size
    freed
  }

  def addressedBlocks: Vector[(Int,DataBlock)] =
    blocks.zipWithIndex.map{ case (block,addr) => (firstBlockAddress+addr, block) }

  /** Write current state of FreeMap to IO. Just write all the blocks to keep it simple. */
  def flush(io: IO): Unit =
    addressedBlocks.foreach { case (addr, block) => io.writeBlock(addr, block) }
}


object FreeMap {

  private val BLOCK_BITS = BLOCKSIZE * 8

  /** Read FreeMap from IO.
    *
    * @param io IO
    * @param size size of file system in blocks
    */
  def apply(io: IO, size: Int): FreeMap = {
    // hard-coded assumption: freemap always begins at block 1
    val nFreeMapBlocks = freeMapBlocksFromSize(size)
    val blocks = (1 to nFreeMapBlocks).map { i =>
      DataBlock(io.getBlock(i))
    }.toVector

    new FreeMap(blocks, size)
  }

  /**
    * Initialize a FreeMap for given size of blocks
    *
    * @param size size of file system in blocks
    */
  def apply(size: Int) = {

    val nFreeMapBlocks = freeMapBlocksFromSize(size)

    val freeMapBlocks = Vector.fill(nFreeMapBlocks)(DataBlock(Array.ofDim[Byte](BLOCKSIZE)))
    val reservedBlocks = 1 + freeMapBlocks.size // header block + FreeMap itself
    val firstBlock = freeMapBlocks(0)
    (0 until reservedBlocks).map { i => setBit(firstBlock, i) }
    val freeMap = new FreeMap(freeMapBlocks, size)

    freeMap
  }

  def freeMapBlocksFromSize(size: Int) = common.ceilingDiv(size, BLOCK_BITS)

  def blockAddress(address: Int): (Int,Int) = (address / BLOCK_BITS, address % BLOCK_BITS)

  /** Free bit indexes in a byte.
    * index 0 is most significant digit.
    */
  def freeBits(b: Byte): Vector[Int] = {
    (0 until 8).filter { i =>
      ((0x80 >> i) & b) == 0
    }.toVector
  }

  /**
    * Count 1-bits in a byte.
    */
  def num1Bits(b: Byte): Int = {
    (1 & b)        + (1 & (b >> 1)) + (1 & (b >> 2)) + (1 & (b >> 3)) +
    (1 & (b >> 4)) + (1 & (b >> 5)) + (1 & (b >> 6)) + (1 & (b >> 7))
  }

  /**
    * @param block block to set bit in
    * @param bitAddress bit to set
    * @return whether the bit was set in this operation (true) or already set (false)
    */
  private[ffs] def setBit(block: DataBlock, bitAddress: Int): Boolean = {
    val byteAddress = bitAddress >> 3 // div 8 :)
    val bit = bitAddress & 0x7
    val mask = 0x80 >> bit
    val byte = block.data(byteAddress)
    val newByte = (mask | byte).toByte
    block.data.update(byteAddress, newByte)
    newByte != byte
  }

  /**
    *
    * @param block block to clear bit in
    * @param bitAddress bit within block to clear
    * @return whether bit was cleared (true) or already clear (false)
    */
  private[ffs] def clearBit(block: DataBlock, bitAddress: Int): Boolean = {
    val byteAddress = bitAddress >> 3
    val bit = bitAddress & 0x7
    val mask = ~(0x80 >> bit)
    val byte = block.data(byteAddress)
    val newByte = (mask & byte).toByte
    block.data.update(byteAddress, newByte)
    newByte != byte
  }

}
