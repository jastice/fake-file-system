package ffs

/**
  * Created by jast on 06/03/2016.
  */
package object common {

  /** Calculate ceiling(a/b) as integer division. */
  def ceilingDiv(a: Int, b: Int): Int = 1 + ((a - 1) / b)

}