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
    it("creates an empty file in root") {
      val fileName = "bdauh"
      withFFS { fs =>
        assert(fs touch s"/$fileName")
        assert(fs ls "/" contains File(fileName))
      }
    }

    it("creates an empty file in nested dir") {
      withFFS { fs =>
        assert(fs mkdir "/augh")
        assert(fs touch "/augh/bdauh")
        assert(fs ls "/augh" contains File("bdauh"))
      }
    }
  }

  describe("mkdir") {
    it("can create an empty directory in root") {
      val dirName = "augh"
      withFFS { fs =>
        assert(fs mkdir s"/$dirName")
        assert(fs ls "/" contains Directory(dirName))
      }
    }

    it("creates nested directories") {
      val dirName = "augh"
      withFFS { fs =>
        fs.mkdir("/nest1")
        fs.mkdir("/nest1/nest2")
        fs.mkdir("/nest1/nest2/nest3")
        assert(fs mkdir s"/nest1/nest2/nest3/$dirName")
        assert(fs ls "/nest1/nest2/nest3" contains Directory(dirName))
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

  describe("rm") {

    it("does not delete root") {
      withFFS { fs =>
        assert(! fs.rm("/"))
      }
    }

    it("deletes an empty file in root") {
      withFFS { fs =>
        fs touch "/dummy" // to make sure a dir block exists
        val filesBefore = fs ls "/"
        val freeBefore = fs.freeMap.free
        fs touch "/boo"
        assert(fs rm "/boo")
        assert(filesBefore == (fs ls "/"))
        assert(freeBefore == fs.freeMap.free)
      }
    }

    it("deletes a non-empty file") {
      withFFS { fs =>
        val filesBefore = fs ls "/"
        val freeBefore = fs.freeMap.free
        fs touch "/boo"
        fail("TODO implement file writing")
        assert(fs rm "/boo")
        val filesAfter = fs ls "/"
        assert(filesBefore == filesAfter)
        assert(freeBefore == fs.freeMap.free)
      }
    }

    it("deletes a file in a nested dir") {
      withFFS { fs =>
        fs mkdir "/boo"
        fs mkdir "/boo/far"
        val filesBefore = fs ls "/boo/far"
        val freeBefore = fs.freeMap.free
        fs touch "/boo/far/toddles"
        assert(fs ls "/boo/far" contains File("toddles"))
        assert(fs rm "/boo/far/toddles")
        assert(filesBefore == (fs ls "/boo/far"))
        assert(freeBefore == fs.freeMap.free)
      }
    }

    it("deletes an empty dir") {
      withFFS { fs =>
        val filesBefore = fs ls "/"
        val freeBefore = fs.freeMap.free
        fs mkdir "/boo"
        assert(fs rm "/boo")
        assert(filesBefore == (fs ls "/"))
        assert(freeBefore == fs.freeMap.free)
      }
    }

    it("does not delete a non-empty dir") {
      withFFS { fs =>
        fs mkdir "/boo"
        fs touch "/boo/far"
        val freeBefore = fs.freeMap.free
        assert(! (fs rm "/boo"))
        val filesAfter = fs ls "/"
        assert(filesAfter contains Directory("boo"))
        assert(freeBefore == fs.freeMap.free)
      }
    }

    it("deletes an empty dir with a deleted file") {
      withFFS { fs =>
        fs mkdir "/boo"
        val freeBefore = fs.freeMap.free
        fs touch "/boo/far"
        assert(fs rm "/boo/far")
        assert(fs rm "/boo")
        val filesAfter = fs ls "/boo"
        assert(filesAfter.isEmpty)
        assert(freeBefore == fs.freeMap.free)
      }
    }
  }

}

