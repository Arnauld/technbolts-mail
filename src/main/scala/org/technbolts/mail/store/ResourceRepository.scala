package org.technbolts.mail.store

import java.io._
import org.apache.commons.io.IOUtils

trait Resource {
  def copyFrom(in:InputStream):Unit
  def copyTo(out:OutputStream):Unit

  def append(bytes:Array[Byte]):Unit
}

trait ResourceRepository {
  def temporaryResource:Resource
}

class FileResourceRepository(val workingDir:File) {
  def temporaryResource = new FileResource(File.createTempFile("res",".eml~", workingDir))
}

class FileResource(val file:File) extends Resource {

  def append(bytes:Array[Byte]):Unit = {
    val out = new FileOutputStream(file, true);
    try{
      out.write(bytes)
    }
    finally{
      IOUtils.closeQuietly(out)
    }
  }

  def copyFrom(in:InputStream):Unit = {
    val out = new FileOutputStream(file);
    try{
      IOUtils.copy(in, out)
    }
    finally{
      IOUtils.closeQuietly(out)
    }
  }
  
  def copyTo(out:OutputStream):Unit = {
    val in = new FileInputStream(file);
    try{
      IOUtils.copy(in, out)
    }
    finally{
      IOUtils.closeQuietly(in)
    }
  }
}