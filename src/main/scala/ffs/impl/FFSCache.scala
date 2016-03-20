package ffs.impl

import java.io.{File => JFile}

import ffs.FFS

/**
  * Singleton cache for FFS instances.
  */
object FFSCache {

  private var cache = Map.empty[String,FFS]

  def apply(file: JFile, create: => FFS): FFS = this.synchronized {

    val key = file.getCanonicalPath

    // file was deleted outside, fresh create
    if (!file.exists() && cache.contains(key))
      cache -= key

    cache.getOrElse(key, {
      val newFFS = create
      cache = cache.updated(key, newFFS)
      newFFS
    })
  }
}
