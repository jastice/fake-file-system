package ffs

/**
  * Created by jast on 07/02/2016.
  */
object FileOps {

  def mkdir(path: String): Unit = ???

  def ls(path: String): Seq[String] = ???

  def touch(path: String): Unit = ???

  def cp(from: String, to: String): Unit = ???

  def mv(from: String, to: String): Unit = ???

  def rm(path: String): Unit = ???

  def append(path: String, data: Array[Byte]): Unit = ???

  def read(path: String): Array[Byte] = ???
}
