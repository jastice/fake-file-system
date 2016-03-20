package ffs.impl

import ffs.common.constants
import constants.CHARSET


case class Path(parts: Vector[String]) {
  def isRoot = parts.isEmpty
  def name = if (parts.isEmpty) "" else parts.last
  def parent = if (parts.isEmpty) this else Path(parts.init)
}

object Path {
  // this is still subject to all kinds of wonky user abuse. let's just assume user only uses mostly valid paths.
  def apply(path: String): Path = {
    val parts = path.split('/').toVector.filter(_.nonEmpty)
    require(valid(parts), s"$path is not a valid path. filename too long?")
    Path(parts)
  }

  def valid(parts: Vector[String]): Boolean = parts.forall { p => p.getBytes(CHARSET).size <= constants.FILENAME_BYTES }
}
