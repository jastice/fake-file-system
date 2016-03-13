import java.io.File

import ffs.impl.IO

/**
  * Created by jast on 06/03/2016.
  */
package object ffs {

  /** Provide a temporary file to a function. */
  def withFile[A](f: File => A): A = {
    val file = new File("dummy-test")
    file.delete() // clean up in case it got left over
    file.deleteOnExit() // just in case :)

    try { f(file) }
    finally { file.delete() }
  }

  /** Provide IO object on temporary file. */
  def withFileIO[A](f: IO => A): A = {
    val file = new File("io-test")
    file.createNewFile()
    file.deleteOnExit()

    IO.withIO(file) { io =>
      try { f(io) }
      finally { file.delete() }
    }
  }

}
