package org.technbolts.mail.store

import java.io.OutputStream

trait Resource {
  def writeTo(out:OutputStream)
}

trait Store {
  def resourceAt(index:Int):Resource
  def resourceCount:Int
}