import java.io.{File => JFile}

import ffs.impl.IO

/**
  * Created by jast on 06/03/2016.
  */
package object ffs {

  /** Provide a temporary file to a function. */
  def withFile[A](f: JFile => A): A = {
    val file = new JFile("dummy-test")
    file.delete() // clean up in case it got left over
    file.deleteOnExit() // just in case :)

    try { f(file) }
    finally { file.delete() }
  }

  /** Provide IO object on temporary file. */
  def withFileIO[A](f: IO => A): A = {
    val file = new JFile("io-test")
    file.createNewFile()
    file.deleteOnExit()

    IO.withIO(file) { io =>
      try { f(io) }
      finally { file.delete() }
    }
  }

  /** Provide a FFS on a temporary file. */
  def withFFS(f: FFS => Unit): Unit = withFile { file =>
    val fs = FFS.initialize(file, 1024*512)
    f(fs)
  }

}
