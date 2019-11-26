package wtf.pants.scalaxml.impl

case class Element(
                    name: String,
                    value: Option[Either[String, List[Element]]] = None,
                    attributes: List[Attribute] = List.empty
                  ) extends Xml {
  def addAttribute(attribute: Attribute) = Element(name, value, attributes :+ attribute)

  def setValue(value: Option[Either[String, List[Element]]]) = Element(name, value, attributes)

  def updateValue(f: Either[String, List[Element]] => Either[String, List[Element]]): Element = {
    Element(name, value.map(f), attributes)
  }

  def toXml: String = {
    val prefix = "<"
    val suffix = ">"
    val elementBegin = prefix + name
    val elementClosingTag = prefix + "/" + name + suffix

    val elementAttributes = attributes.map(" " + _.toXml).mkString

    value match {
      case Some(either) =>
        either match {
          case Left(leftValue) =>
            elementBegin + elementAttributes + suffix + leftValue + elementClosingTag
          case Right(rightValue) =>
            elementBegin + elementAttributes + suffix + rightValue.map(_.toXml + "\n").mkString + elementClosingTag
        }
      case _ =>
        elementBegin + elementAttributes + "/" + suffix
    }
  }
}

object Element {


}
