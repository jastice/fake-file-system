package ffs

// high-level representation of files

sealed abstract class FileNode {

  val name: String

  /** Index block of this file. */
  val block: FileBlock
}

case class File(name: String, size: Int, block: FileBlock) extends FileNode

case class Directory(name: String, block: DirectoryBlock) extends FileNode

object files {

  def mkdir(inPath: Directory, name: String): Unit = ???

  def ls(dir: Directory): Seq[String] = ???

  def touch(inPath: Directory, name: String): Unit = ???

  def cp(from: File, to: File): Unit = ???

  def mv(from: File, to: File): Unit = ???

  def rm(path: File): Unit = ???


}
