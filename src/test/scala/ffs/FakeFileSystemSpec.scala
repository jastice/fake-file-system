package ffs

import ffs.impl.File
import org.scalatest.FunSpec

class FakeFileSystemSpec extends FunSpec {
  describe("Creating a filesystem") {
    it("initializing empty fs") {
      val file = new java.io.File("dummy-fs")
      FFS.initialize(file, 1024*512)

      assert(file.exists())
      assert(file.isFile)

      file.delete()
    }

    it("contains the default 'foo' file") {
      val file = new java.io.File("dummy-fs")
      val ffs = FFS.initialize(file, 1024*512)
      val filesInRoot = ffs.ls("/")
      println(filesInRoot)
      assert(filesInRoot.contains(File("foo",0)))
      file.delete()
    }
  }

  describe("opening an existing filesystem") {
    it("reads the same blocks that were written") {

      val file = new java.io.File("dummy-fs-write-open")
      val ffs1 = FFS.initialize(file, 1024*512)

      val ffs2 = FFS.open(file)

      assert(ffs1.header == ffs2.header)
      file.delete()

      // TODO reading existent files
    }
  }
}

