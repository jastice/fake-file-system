package ffs

import ffs.impl.{DirectoryIndexBlock, DirectoryBlock}
import org.scalatest.{FunSpec, SequentialNestedSuiteExecution}

class FakeFileSystemSpec extends FunSpec with SequentialNestedSuiteExecution {
  describe("Creating a filesystem") {
    it("initializing empty fs") {
      withFile { file =>
        FFS.initialize(file, 1024*512)

        assert(file.exists)
        assert(file.isFile)
      }
    }

    it("contains the default 'foo' file") {
      withFile { file =>
        val fs = FFS.initialize(file, 1024*512)
        val filesInRoot = fs.ls("/")

        assert(filesInRoot.contains(File("foo")))
      }
    }
  }

  describe("opening an existing filesystem") {
    it("reads the same blocks that were written") {
      withFile { file =>
        val fs1 = FFS.initialize(file, 1024 * 512)
        val fs2 = FFS.open(file)

        assert(fs1.header == fs2.header)
      }
    }
    // TODO reading existent files, writing new files
  }

  describe("touch") {
    it("can create an empty file in root") {
      val fileName = "bdauh"
      withFile { file =>
        val fs = FFS.initialize(file, 1024*512)
        fs.touch(s"/$fileName")
        assert(fs.ls("/") contains File(fileName))
      }
    }
  }

  describe("mkdir") {
    it("can create an empty directory in root") {
      val dirName = "augh"
      withFFS { fs =>
        fs.mkdir(s"/$dirName")
        assert(fs.ls("/") contains Directory(dirName))
      }
    }

    it("can create nested directories") {
      val dirName = "augh"
      withFFS { fs =>
        fs.mkdir("/nest1")
        fs.mkdir("/nest1/nest2")
        fs.mkdir("/nest1/nest2/nest3")
        fs.mkdir(s"/nest1/nest2/nest3/$dirName")
        assert(fs.ls("/nest1/nest2/nest3") contains Directory(dirName))
      }
    }

    it("can create maximum number of subdirs in a dir") {
      val maxFiles = DirectoryIndexBlock.MAX_DIRECTORY_BLOCKS * DirectoryBlock.MAX_ENTRIES
      val testDir = "/testy"

      withFFS { fs =>
        fs.mkdir(testDir)
        (0 until maxFiles).foreach { i => fs.mkdir(s"$testDir/d$i")}

        assert(fs.ls(testDir).size == maxFiles)
      }

    }
  }

}

