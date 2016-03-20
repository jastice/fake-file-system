package ffs

import ffs.common.constants
import ffs.impl.{DirectoryIndexBlock, DirectoryBlock}
import org.scalatest.{FunSpec, SequentialNestedSuiteExecution}

import scala.util.Random

class FakeFileSystemSpec extends FunSpec with SequentialNestedSuiteExecution {
  describe("Creating a filesystem") {
    it("initializing empty fs") {
      withFile { file =>
        FFS(file, 1024*512)

        assert(file.exists)
        assert(file.isFile)
      }
    }

    it("contains the default 'foo' file") {
      withFile { file =>
        val fs = FFS(file, 1024*512)
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

        val blockzip = fs1.freeMap.blocks.zip(fs2.freeMap.blocks)
        assert(blockzip.forall {case (b1,b2) => b1.data sameElements b2.data})
      }
    }

    it("allows modifications") {
      withFile { file =>
        FFS.initialize(file, 1024 * 512)
        val fs2 = FFS.open(file)
        assert(fs2 touch "/bozz")
        assert(fs2 ls "/" contains File("bozz"))
      }
    }
  }

  describe("concurrent use of filesystem") {
    it("is threadsafe") {
      fail
    }
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

    it("does not create a subdirectory to a file") {
      withFFS { fs =>
        assert(!(fs mkdir "/lassi/mango"))
      }
    }

    it("does not create a subdirectory to a nonexistent path") {
      withFFS { fs =>
        assert(!(fs mkdir "/lassi/mango"))
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
        fs.append("/boo", Array.fill[Byte](71)(23))
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
        fs touch "/boo/far/dummy" // to make sure dir block exists
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
        val freeBefore = fs.freeMap.free
        fs mkdir "/boo"
        fs touch "/boo/far"
        assert(fs rm "/boo/far")
        assert(fs rm "/boo")
        val filesAfter = fs ls "/"
        assert(! (filesAfter contains Directory("boo")))
        assert(freeBefore == fs.freeMap.free)
      }
    }
  }

  describe("append") {
    it("doesn't change anything appending empty array") {
      withFFS { fs =>
        fs touch "/solomon"
        val freeBefore = fs.freeMap.free
        fs.append("/solomon", Array.empty[Byte])
        assert(fs.freeMap.free == freeBefore)
        assert((fs size "/solomon") == 0)
      }
    }

    it("changes file size with non-empty array and reduces free blocks") {
      withFFS { fs =>
        val bytes = Array.iterate[Byte](1,77)(b => (b+13).toByte)
        fs touch "/silly"
        val sizeBefore = fs size "/silly"
        val freeBefore = fs.freeMap.free
        fs.append("/silly", bytes)
        assert((fs size "/silly") == sizeBefore + bytes.length)
        assert(fs.freeMap.free == freeBefore - 1)
      }
    }

    it("appends arrays larger than block size") {
      withFFS { fs =>
        val bytes = Array.iterate[Byte](1, constants.BLOCKSIZE*3 + 77)(b => (b+13).toByte)
        fs touch "/silly"
        val sizeBefore = fs size "/silly"
        val freeBefore = fs.freeMap.free
        fs.append("/silly", bytes)
        assert((fs size "/silly") == sizeBefore + bytes.length)
        assert(fs.freeMap.free == freeBefore - 4)
      }
    }

    it("appends smaller chunks multiple times") {
      withFFS{ fs =>
        val bytes = Array.fill(23)(17.toByte)
        fs touch "/silly"
        val sizeBefore = fs size "/silly"
        fs.append("/silly", bytes)
        fs.append("/silly", bytes)
        assert((fs size "/silly") == sizeBefore + bytes.length + bytes.length)
      }

    }

    it("appends larger chunks multiple times") {
      withFFS{ fs =>
        val bytes0 = Array.fill(513)(17.toByte)
        val bytes1 = Array.fill(713)(23.toByte)
        fs touch "/silly"
        val sizeBefore = fs size "/silly"
        fs.append("/silly", bytes0)
        fs.append("/silly", bytes1)
        assert((fs size "/silly") == sizeBefore + bytes0.length + bytes1.length)
      }
    }
  }

  describe("read") {

    it("reads nothing from an empty file") {
      withFFS { fs =>
        fs touch "/hula"
        val out = fs.read("/hula", 0, 0)
        assert(out.isEmpty)
      }
    }

    it("reads a few written bytes") {
      withFFS { fs =>
        fs touch "/hula"
        val bytes = Array.ofDim[Byte](23)
        Random.nextBytes(bytes)
        fs.append("/hula", bytes)

        val readAll = fs.read("/hula", 0, bytes.length)
        assert(readAll.toVector == bytes.toVector)
      }
    }

    it("reads chunk of written data") {
      withFFS { fs =>
        fs touch "/hula"
        val bytes = Array.ofDim[Byte](101)
        Random.nextBytes(bytes)
        fs.append("/hula", bytes)

        val readAll = fs.read("/hula", 0, bytes.length)
        assert(readAll sameElements bytes)
      }
    }

    it("reads chunk of data larger than a block") {
      withFFS { fs =>
        fs touch "/hula"
        val bytes = Array.ofDim[Byte](constants.BLOCKSIZE + 13)
        Random.nextBytes(bytes)
        fs.append("/hula", bytes)

        val readAll = fs.read("/hula", 0, bytes.length)
        assert(readAll.toVector == bytes.toVector)
      }
    }

    it("reads correct slices of written data") {
      withFFS { fs =>
        fs touch "/hula"
        val bytes = Array.ofDim[Byte](constants.BLOCKSIZE*4 + 101)
        Random.nextBytes(bytes)
        fs.append("/hula", bytes)

        val readAll = fs.read("/hula", 0, bytes.length)
        assert(readAll sameElements bytes)

        val readSlice = fs.read("/hula", 13, bytes.length - constants.BLOCKSIZE)
        assert(readSlice sameElements bytes.slice(13, bytes.length - constants.BLOCKSIZE))
      }
    }

    it("fails on out-of-bounds parameters") {
      withFFS { fs =>
        fs touch "/alog"
        fs.append("/alog", Array.fill(23)(7.toByte))

        intercept[IllegalArgumentException] {
          fs.read("/alog", -2, 11)
        }

        intercept[IllegalArgumentException] {
          fs.read("/alog", 13, 1)
        }

        intercept[IllegalArgumentException] {
          fs.read("/alog", 133, 198)
        }
      }
    }
  }

}
