package ffs.impl

import ffs.common.constants


case class Path(parts: Vector[String]) {
  def isRoot = parts.isEmpty
  def name = if (parts.isEmpty) "" else parts.last
  def parent = if (parts.isEmpty) this else Path(parts.init)
}

object Path {
  // TODO this is still subject to all kinds of wonky user abuse. let's just assume user only uses mostly valid paths.
  def apply(path: String): Path = Path(path.split('/').toVector.filter(_.nonEmpty))
}

object paths {

  import constants.CHARSET

  def valid(path: Path): Boolean = path.parts.forall { p => p.getBytes(CHARSET).size <= constants.FILENAME_BYTES }

  def valid(path: String): Boolean = valid(Path(path))

}
