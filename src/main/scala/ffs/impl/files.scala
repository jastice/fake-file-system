package ffs.impl

// high-level representation of files

sealed abstract class FileNode {

  val name: String

}

case class File(name: String, size: Int) extends FileNode

case class Directory(name: String) extends FileNode

object files {

  def mkdir(inPath: Directory, name: String): Unit = ???

  def ls(dir: Directory): Seq[String] = ???

  def touch(inPath: Directory, name: String): Unit = ???

  def cp(from: File, to: File): Unit = ???

  def mv(from: File, to: File): Unit = ???

  def rm(path: File): Unit = ???


}
