package ffs

import java.io.File

import ffs.impl.{FileEntry, DirectoryBlock, IO, HeaderBlock}
import org.scalatest.{SequentialNestedSuiteExecution, FunSpec}

//noinspection NameBooleanParameters
class IOSpec extends FunSpec with SequentialNestedSuiteExecution {

  describe("reading restores written blocks") {
    it("HeaderBlock") {
      withFileIO { io =>

        val outblock = HeaderBlock(3027, Vector(74, 723, 717, 788))
        io.writeBlock(0, outblock)
        val inblock = HeaderBlock(io.getBlock(0))

        assert(outblock == inblock)
      }
    }

    it("DirectoryBlock") {
      withFileIO {io =>
        val f1 = FileEntry("dir", true, false, 777)
        val f2 = FileEntry("f2", false, true, 321)
        val f3 = FileEntry("f3", false, false, 648)
        val outblock = DirectoryBlock(Vector(f1,f2,f3))
        val addr = 5233
        io.writeBlock(addr,outblock)
        val inblock = DirectoryBlock(io.getBlock(addr))
        assert(outblock == inblock)
      }
    }
  }

}
