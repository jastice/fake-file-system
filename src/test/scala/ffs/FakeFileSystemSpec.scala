package ffs

import org.scalatest.FunSpec

class FakeFileSystemSpec extends FunSpec {
  describe("Creating a filesystem") {
    it("initializing empty fs"){
      val file = new java.io.File("dummy-fs")
      FFS.initialize(file, 1024*512)

      assert(file.exists())
      assert(file.isFile())

      file.delete()
    }
  }

  describe("opening an existing filesystem") {
    it("reads the same blocks that were written") {

      val file = new java.io.File("dummy-fs-write-open")
//      val ffs1 = FFS.initialize(file, 1024*512)

    }
  }
}

