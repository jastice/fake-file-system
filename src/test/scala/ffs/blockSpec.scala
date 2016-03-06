package ffs

import java.nio.ByteBuffer

import ffs.impl.{FileEntry, DirectoryBlock, FileBlock, HeaderBlock}
import org.scalatest.FunSpec
import org.scalatest.prop.GeneratorDrivenPropertyChecks

/**
  * Created by jast on 05/03/2016.
  */
//noinspection NameBooleanParameters
class blockSpec extends FunSpec with GeneratorDrivenPropertyChecks{


  describe("serialization / deserialization reversibility") {

    it("HeaderBlock") {
      val block = HeaderBlock(3027,Vector(74,723,717,788))
      assert(HeaderBlock(block.toBytes) == block)
    }

    it("FileBlock") {
      val block = FileBlock(377,Vector(74,723,717,788,321),2400)
      assert(FileBlock(block.toBytes) == block)
    }

    it("FileEntry") {
      val entry = FileEntry("somedir", true, false, 779)
      assert(FileEntry(ByteBuffer.wrap(entry.toBytes)).get == entry)
    }

    it("DirectoryBlock") {
      val f1 = FileEntry("dir", true, false, 777)
      val f2 = FileEntry("f2", false, true, 321)
      val f3 = FileEntry("f3", false, false, 648)
      val block = DirectoryBlock(Vector(f1,f2,f3))

      assert(DirectoryBlock(block.toBytes) == block)
    }
  }

}
