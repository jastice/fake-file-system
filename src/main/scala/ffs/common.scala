package ffs

/**
  * Created by jast on 24/02/2016.
  */
object common {

  /** Calculate ceiling(a/b) as integer division. */
  def ceilingDiv(a: Int, b: Int): Int = 1 + ((a - 1) / b)
}
