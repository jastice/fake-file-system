package ffs

import org.scalatest.FunSpec

class FakeFileSystemSpec extends FunSpec {
  describe("Creating a filesystem") {
    it("initializing empty fs"){
      val file = new java.io.File("dummy-fs")
      FFS.initialize(file, 1024*1024)

      assert(file.exists())
      assert(file.isFile())

      file.delete()
    }
  }
}

