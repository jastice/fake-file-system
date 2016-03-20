package ffs.impl

/**
  * FFS manipulation operations.
  */
object manipulation {


  /** Non-deleted file entries in directory blocks. */
  def fileEntries(io: IO)(addresses: Vector[Int]): Vector[FileEntry] = {
    addresses
      .flatMap { a => DirectoryBlock(io.getBlock(a)).files }
      .filter(!_.deleted)
  }


  /** Get the file entry describing the file referenced by `path` among `addresses`, recursively. */
  def fileForPath(io: IO)(addresses: Vector[Int], path: Vector[String]): Option[FileEntry] = {
    if (path.isEmpty) None
    else {
      val name = path.head
      addresses
        .flatMap { a => DirectoryBlock(io.getBlock(a)).files }
        .find { case entry => !entry.deleted && entry.name == name }
        .flatMap { case entry =>
          val restPath = path.tail
          if (restPath.nonEmpty) {
            if (entry.dir) {
              val block = DirectoryIndexBlock(io.getBlock(entry.address))
              fileForPath(io)(block.blockAddresses, restPath)
            } else None // not a dir here, can't complete path recursion
          }
          else Some(entry)
        }
    }
  }

  /** Find directory block containing file with `name`. */
  def dirBlockForName(io: IO, addresses: Vector[Int], name: String): Option[(Int,DirectoryBlock)] = {
    addresses.iterator
      .map { a => (a, DirectoryBlock(io.getBlock(a))) }
      .find { case (address,dirBlock) => dirBlock.files.exists(_.name == name) }
  }

}
