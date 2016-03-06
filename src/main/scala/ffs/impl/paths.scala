package ffs.impl

import ffs.common.constants


case class Path(parts: Vector[String])

object Path {
  def apply(path: String): Path = Path(path.split('/').toVector)
}

object paths {

  import constants.CHARSET

  def valid(path: Path): Boolean = path.parts.forall { p => p.getBytes(CHARSET).size <= constants.FILENAME_BYTES }

  def valid(path: String): Boolean = valid(Path(path))

}
