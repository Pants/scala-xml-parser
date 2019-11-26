package wtf.pants.scalaxml.impl

case class Attribute(name: String, value: String) extends Xml {
  override def toXml: String = s"""$name=\"${value.replace("\"", "\\\"")}\""""
}

object Attribute {
  def fromXml(string: String): Attribute = {
    val delimIndex = string.indexOf("=")
    val attributeName = string.substring(0, delimIndex).trim
    val valuePreParse = string.trim
    val attributeValue = valuePreParse.substring(delimIndex + 2, valuePreParse.length - 1)
    Attribute(attributeName, attributeValue)
  }
}