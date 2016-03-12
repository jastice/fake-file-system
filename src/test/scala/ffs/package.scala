import java.io.File

import ffs.impl.IO

/**
  * Created by jast on 06/03/2016.
  */
package object ffs {

  def withFileIO[A](f: IO => A): A = {
    val file = new File("io-test")
    file.createNewFile()
    val io = new IO(file)
    val ret = f(io)
    io.close()
    ret
  }

}
