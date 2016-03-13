package ffs

// high-level representation of files

sealed abstract class FileNode {
  val name: String
}

case class File(name: String) extends FileNode

case class Directory(name: String) extends FileNode
