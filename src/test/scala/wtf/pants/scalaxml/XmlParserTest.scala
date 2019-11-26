package wtf.pants.scalaxml

import org.scalatest.FunSuite

class XmlParserTest extends FunSuite {
  val elementBasic = """<element>Hello World!</element>"""
  val elementWithAttributes = """<element attr1="hello" attr2="world">Hello World!</element>"""
  val elementWithoutValue = """<element/>"""
  val elementWithoutValueWithAttributes = """<element attr1="hello" attr2="world"/>"""

  test("XmlParser.getElementName") {
    val nameBasic = XmlParser.getElementName(elementBasic.drop(1)).value
    assert(nameBasic.equals("element"), "elementBasic was parsed incorrectly.")

    val nameWithAttr = XmlParser.getElementName(elementWithAttributes.drop(1)).value
    assert(nameWithAttr.equals("element"), "elementWithAttributes was parsed incorrectly.")

    val nameWithoutValue = XmlParser.getElementName(elementWithoutValue.drop(1)).value
    assert(nameWithoutValue.equals("element"), "elementWithoutValue was parsed incorrectly.")

    val nameWithoutValueWithAttributes = XmlParser.getElementName(elementWithoutValueWithAttributes.drop(1)).value
    assert(nameWithoutValueWithAttributes.equals("element"), "elementWithoutValueWithAttributes was parsed incorrectly.")
  }

  test("XmlParser.findUpTo") {
    val findUpTo = XmlParser.findUpTo("Hello", elementBasic)

    val actualValue = findUpTo.value
    val expectedValue = "<element>"
    assert(actualValue.equals(expectedValue), s"Expected '$expectedValue' got '$actualValue'")

    val actualContents = findUpTo.contents
    val expectedContents = "Hello World!</element>"
    assert(actualContents.equals(expectedContents), s"Expected '$expectedContents' got '$actualContents'")
  }

  test("XmlParser.getAttributes") {
    val attributes = XmlParser.getAttributes("""attr1="hello" attr2="world">Hello World!</element>""")
    val attributeList = attributes.value

    val actualSize = attributeList.size
    val expectedSize = 2
    assert(actualSize == expectedSize, s"Expected attribute size of '$expectedSize' got '$actualSize'")

    val firstAttribute = attributeList.head

    val firstAttributeName = firstAttribute.name
    val expectedName = "attr1"
    assert(firstAttributeName.equals(expectedName), s"Expected attribute name '$expectedName' got '$firstAttributeName'")

    val firstAttributeValue = firstAttribute.value
    val expectedValue = "hello"
    assert(firstAttributeValue.equals(expectedValue), s"Expected attribute value '$expectedValue' got '$firstAttributeValue'")
  }

}
