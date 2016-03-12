package ffs

import ffs.impl.FreeMap
import org.scalatest.FunSpec

import FreeMap._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/**
  * Created by jast on 02/03/2016.
  */
class FreeMapSpec extends FunSpec with GeneratorDrivenPropertyChecks {

  describe("initialization") {
    it("countFree is size - reserved for empty freemap") {
      val size = 7096 // two freemap blocks, one not full
      val reserved = 3 // header + freemap
      val freemap = FreeMap(size)
      assert(freemap.countFree == size - reserved)
    }
  }

  describe("takeBlocks") {

    it("respects reserved blocks") {
      val freemap = FreeMap(324)
      val taken = freemap.takeBlocks(9)
      // block 0 is reserved by default for header, and block 1 is freemap itself
      assert(taken == Vector(2,3,4,5,6,7,8,9,10))
    }

    it("returns correct number of distinct block addresses") {
      val freemap = FreeMap(7732)
      val take = 171
      val taken = freemap.takeBlocks(take)
      assert( taken.size == take )
      assert( taken.distinct.size == take )
    }

    it("decreases free blocks") {
      val freemap = FreeMap(7732)
      val freeBefore = freemap.free
      val take = 171
      freemap.takeBlocks(take)
      val freeAfter = freemap.free
      assert(freeAfter == freeBefore - take)
    }

    it("marks taken blocks") {
      val freemap = FreeMap(7732)
      assert(
        freemap.takeBlocks(171).forall { b =>
          val (block,blockBit) = blockAddress(b)
          !freemap.setBit(freemap.blocks(block), blockBit)
        }
      )
    }


  }

  describe("setBit") {

    it("sets bits addressed by most significant digit as index 0") {
      val freemap = FreeMap(334)
      val block = freemap.blocks.head
      val byte = 73
      val bit = 0
      freemap.setBit(block, byte*8+bit)
      println(block.data.toVector)
      assert(block.data(byte) == 0x80.toByte)
    }

    it("sets bits addressed by most significant digit as index 7") {
      val freemap = FreeMap(7732)
      val blk = freemap.blocks.head
      val byte = 77
      val bit = 7
      freemap.setBit(blk, byte*8+bit)
      assert(blk.data(byte) == 1)
    }

    it("setting a clear bit returns true") {
      val freemap = FreeMap(7732)
      val block = freemap.blocks.head
      assert(freemap.setBit(block,3732))
    }

    it("setting a set bit returns false") {
      val freemap = FreeMap(7732)
      val block = freemap.blocks.head
      freemap.setBit(block,3732)
      assert(! freemap.setBit(block,3732))
    }
  }

  describe("clearBit") {

    it("clears bits addressed by most significant digit as index 0") {
      val freemap = FreeMap(334)
      val block = freemap.blocks.head
      val byte = 73
      val bit = 0
      block.data.update(byte,0xFF.toByte)
      freemap.clearBit(block, byte*8+bit)
      assert(block.data(byte) == 0x7F.toByte)
    }

    it("clears bits addressed by least significant digit as index 7") {
      val freemap = FreeMap(334)
      val block = freemap.blocks.head
      val byte = 73
      val bit = 7
      block.data.update(byte,0xFF.toByte)
      freemap.clearBit(block, byte*8+bit)
      assert(block.data(byte) == ~1.toByte)
    }


    it("clear set bit correctly") {
      val freemap = FreeMap(7732)
      val block = freemap.blocks.head
      val byte = 77
      val bit = 4
      freemap.setBit(block, byte*8+bit)
      assert(block.data(byte) == 8)
      freemap.clearBit(block, byte*8+bit)
      assert(block.data(byte) == 0)
    }

    it("clearing a clear bit returns false") {
      val freemap = FreeMap(7732)
      val block = freemap.blocks.head
      assert(! freemap.clearBit(block,3732))
    }

    it("clearing a set bit returns true") {
      val freemap = FreeMap(7732)
      val block = freemap.blocks.head
      freemap.setBit(block,3732)
      assert(freemap.clearBit(block,3732))
    }
  }

  describe("freeBits") {
    it("returns all indices for 0") {
      assert(freeBits(0) == Vector(0,1,2,3,4,5,6,7))
    }

    it("returns no indices for 0xFF") {
      assert(freeBits(-1).isEmpty)
    }

    it("returns correct indices for 42") {
      assert(freeBits(42) == Vector(0,1,3,5,7) )
    }

    it("number of indices is 8 - num1Bits") {
      forAll { b: Byte =>
        assert(freeBits(b).size == 8-num1Bits(b))
      }
    }
  }

  describe("num1bits") {
    it("num1Bits(0) == 0") {
      assert(num1Bits(0) == 0)
    }

    it("num1Bits(-1) == 8") {
      assert(num1Bits(-1) == 8)
    }

    it("num1Bits(42) == 3") {
      assert(num1Bits(42) == 3)
    }
  }

}
