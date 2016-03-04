package ffs

/**
  * The Fake File System.
  */
class FFS private(physical: java.nio.file.Path, header: HeaderBlock, freeMap: FreeMap) {

//  private[ffs] val freeMap: FreeMap = ???

  private val io = new IO(physical)

  /** Set a block as blocked or free in the freeMap. */
  private def updateBlock(address: Int, blocked: Boolean): Unit = ???


  /** List all files within `path`. */
  def ls(path: String): Seq[String] = {
    ???
  }

  /** Recursively list all files below `path`, with their full path name. */
  def lsr(path: String): Seq[String] = ???

  /** Create directory in under `path`. Path must be directory itself. */
  def mkdir(path: String): FFS = ???

  /** Create empty file in directory `path`. */
  def touch(path: String): FFS = ???

  def cp(from: String, to: String): FFS = ???

  def mv(from: String, to: String): FFS = ???

  /** Delete a file. */
  def rm(path: String): FFS = {
    // mark file as deleted in index
    // mark all its blocks as free
    ???
  }

//  def flush(): FFS = ???

}



object FFS {

  /**
    * Opens or creates a Fake File System from given physical file.
    * @param physical The actual file system file in which the fake file system will reside.
    * @return
    */
  def apply(physical: java.io.File): FFS = {
    // for now, just create it if it doesn't exist.
    if (physical.exists()) {
      // initialize from file
      new FFS(physical.toPath, ???, ???)
    } else {
      throw new RuntimeException(s"no FFS here: ${physical.getAbsolutePath}")
    }
  }

  /**
    *
    * @param physical
    * @param size size in bytes
    * @return
    */
  def initialize(physical: java.io.File, size: Int): FFS = {
    import constants.blockSize, common.ceilingDiv

    physical.createNewFile()

    val nBlocks = ceilingDiv(size, blockSize)
    val headerBlock = HeaderBlock(nBlocks,Vector(2))

    val freeMap = FreeMap(size)

    val rootBlockAddress = freeMap.takeBlocks(1).head
    val dummyFileBlockAddress = freeMap.takeBlocks(1).head

    val dummyFileEntry = FileEntry("foo", dir=false, deleted=false, dummyFileBlockAddress)
    val dummyFileBlock = FileBlock(rootBlockAddress, Vector.empty[Int], 0)
    val dummyFileInfo = (dummyFileBlockAddress, dummyFileBlock)

    val rootBlock = DirectoryBlock(Vector(dummyFileEntry))
    val rootBlockInfo = (rootBlockAddress, rootBlock)

    val blocks =
      Vector( (0,headerBlock) ) ++
      freeMap.addressedBlocks ++
      Vector(rootBlockInfo, dummyFileInfo)

    val path = physical.toPath

    val io = new IO(path)
    io.writeBlocks(blocks)
    io.close()

    new FFS(path, headerBlock, freeMap)
  }

}
