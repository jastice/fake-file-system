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

  describe("FreeMap basic operations") {

  }

  describe("takeBlocks") {

    it("respects reserved blocks") {
      val freemap = FreeMap(324)
      val taken = freemap.takeBlocks(3)
      assert(taken == Vector(2,3,4))
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

    it("sets cleared bit correctly") {
      val freemap = FreeMap(7732)
      val blk = freemap.blocks.head
      val byte = 77
      val bit = 4
      freemap.setBit(blk, byte*8+bit)
      assert(blk.data(byte) == 16)
    }

    it("setting a clear bit returns true") {
      val freemap = FreeMap(7732)
      val blk = freemap.blocks.head
      assert(freemap.setBit(blk,3732))
    }

    it("setting a set bit returns false") {
      val freemap = FreeMap(7732)
      val blk = freemap.blocks.head
      freemap.setBit(blk,3732)
      assert(! freemap.setBit(blk,3732))
    }
  }

  describe("clearBit") {
    it("clear set bit correctly") {
      val freemap = FreeMap(7732)
      val blk = freemap.blocks.head
      val byte = 77
      val bit = 4
      freemap.setBit(blk, byte*8+bit)
      assert(blk.data(byte) == 16)
      freemap.clearBit(blk, byte*8+bit)
      assert(blk.data(byte) == 0)
    }

    it("clearing a clear bit returns false") {
      val freemap = FreeMap(7732)
      val blk = freemap.blocks.head
      assert(! freemap.clearBit(blk,3732))
    }

    it("clearing a set bit returns true") {
      val freemap = FreeMap(7732)
      val blk = freemap.blocks.head
      freemap.setBit(blk,3732)
      assert(freemap.clearBit(blk,3732))
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
