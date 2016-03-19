package ffs

/**
  * Created by jast on 06/03/2016.
  */
package object common {

  /** Calculate ceiling(a/b) as integer division, only for non-negative `a`. */
  def ceilingDiv(a: Int, b: Int): Int =
    if (a>0) 1 + ((a - 1) / b)
    else 0

}
