package org.technbolts

import java.net.URL
import org.springframework.util.DefaultPropertiesPersister
import java.util.Properties
import java.io.File

object TestSettings {
  def apply() = new TestSettings(loadProperties(classOf[TestSettings].getResource("/test.properties")))
  def apply(url:URL) = new TestSettings(loadProperties(url))

  def loadProperties(url:URL) = {
    val loader = new DefaultPropertiesPersister
    val props = new Properties
    loader.load(props, url.openStream)
    props
  }
}

class TestSettings(properties:Properties) {
  def workingDir:File = properties.getProperty("working-dir") match {
    case null => throw new IllegalStateException("No working dir defined")
    case path => new File(path)
  }
}