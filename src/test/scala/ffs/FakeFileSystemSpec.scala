package ffs

import org.scalatest.{FunSpec, SequentialNestedSuiteExecution}

class FakeFileSystemSpec extends FunSpec with SequentialNestedSuiteExecution {
  describe("Creating a filesystem") {
    it("initializing empty fs") {
      withFile { file =>
        FFS.initialize(file, 1024*512)

        assert(file.exists())
        assert(file.isFile)
      }
    }

    it("contains the default 'foo' file") {
      withFile { file =>
        val ffs = FFS.initialize(file, 1024*512)
        val filesInRoot = ffs.ls("/")

        assert(filesInRoot.contains(File("foo")))
      }
    }
  }

  describe("opening an existing filesystem") {
    it("reads the same blocks that were written") {
      withFile { file =>
        val ffs1 = FFS.initialize(file, 1024 * 512)
        val ffs2 = FFS.open(file)

        assert(ffs1.header == ffs2.header)
      }
    }
    // TODO reading existent files
  }

}
