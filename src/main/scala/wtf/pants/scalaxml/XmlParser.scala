package wtf.pants.scalaxml

import wtf.pants.scalaxml.data.ContentFold
import wtf.pants.scalaxml.impl.{Attribute, Element}

object XmlParser {

  def main(args: Array[String]): Unit = {
    val fromFile = scala.io.Source.fromFile("test.xml")

    val read = XmlParser.readElement(fromFile.mkString)
    read.value.foreach(e => println(e.toXml))

    fromFile.close()
  }

  def getNextDelimIndex(contents: String): Int = {
    val chars = List(" ", ">", "/")
    val indexes = chars.map(contents.indexOf).filter(_ != -1)
    indexes.min
  }

  def findUpTo(charIndex: String, contents: String): ContentFold[String] = {
    val index = contents.indexOf(charIndex)

    if (index == -1) {
      println(s"[ERROR] Failed to find '$charIndex' in '${contents.substring(0, 10)}'")
    }

    ContentFold(contents.substring(0, index), contents.drop(index))
  }

  def getElementName(contents: String): ContentFold[String] = {
    val spaceIndex = getNextDelimIndex(contents)
    ContentFold(contents.substring(0, spaceIndex), contents.drop(spaceIndex))
  }

  def readElement(_contents: String): ContentFold[Option[Element]] = {
    val trimmed = _contents.trim

    if (!trimmed.startsWith("<")) return ContentFold(None, trimmed)

    val contents = trimmed.drop(1).trim

    val elementName = getElementName(contents)
    val attributes = getAttributes(elementName.contents)

    //>, />
    val element = impl.Element(elementName.value, None, attributes.value)

    if (attributes.contents.startsWith("/>")) {
      return ContentFold(Some(element), attributes.contents.drop(2))
    }

    val elementValue = getElementValue(element, attributes.contents.drop(1)) //drop 1 to get exit the start tag ('>')
    ContentFold(Some(elementValue.value), elementValue.contents)
  }

  def getElementValue(element: Element, contents: String): ContentFold[Element] = {
    val trimmed = contents.trim

    //If the contents doesn't start with a '<' it's a string based value
    if (!trimmed.startsWith("<")) {
      val valueBeforeEndTag = findUpTo("</", trimmed)
      val afterEndTag = findUpTo(">", valueBeforeEndTag.contents)
      val value = Some(Left(valueBeforeEndTag.value))

      return ContentFold(element.setValue(value), afterEndTag.contents)
    }

    val elements = readElements(element, trimmed)
    ContentFold(appendElements(element, elements.value), elements.contents)
  }

  def readElements(parent: Element, contents: String, elements: List[Element] = List.empty): ContentFold[List[Element]] = {
    val trimmed = contents.trim

    val endTag = s"</${parent.name}>"
    if (trimmed.startsWith(endTag)) {
      return ContentFold(elements, trimmed.drop(endTag.length))
    }

    val nextElement = readElement(trimmed)

    if (nextElement.value.isEmpty) {
      println("[ERROR] Invalid contents..")
      println(s"[ERROR/Contents] ${trimmed.substring(0, Math.min(20, trimmed.length))}")
    }

    readElements(parent, nextElement.contents.drop(1), elements :+ nextElement.value.get)
  }

  def appendElements(element: Element, elements: List[Element]): Element = {
    val elementWithValue = if (element.value.isEmpty) element.setValue(Some(Right(List.empty))) else element

    elementWithValue.updateValue {
      case Right(rightElement) =>
        Right(rightElement ++ elements)
      case either =>
        either
    }
  }

  def getAttributes(contents: String, attribute: List[Attribute] = List.empty): ContentFold[List[Attribute]] = {
    val trimmed = contents.trim
    //If it starts with either of these the element doesn't have any attributes.
    if (trimmed.startsWith(">") || trimmed.startsWith("/>")) return ContentFold(attribute, trimmed)

    val attrName = findUpTo("=", trimmed)
    val attrValue = findUpTo("\"", attrName.contents.drop(2)) //drop 2 to remove the =" after the attribute name

    getAttributes(attrValue.contents.drop(1), attribute :+ Attribute(attrName.value, attrValue.value))
  }

}
