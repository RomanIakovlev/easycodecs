package net.iakovlev.dynamo.generic

import org.specs2.mutable.Specification

class CodecParityTest extends Specification {
  "Encoder and Decoder should be on par in terms of" >> {
    "collections" >> {
      case class C(l: Vector[Int])
      val translation = Map("l" -> AttributeValueList(
        List(AttributeValueInt(1), AttributeValueInt(2), AttributeValueInt(3))))
      val original = C(Vector(1, 2, 3))
      val decoded = Decoder[AttributeValue, C].decode(translation)
      val encoded = Encoder[C, AttributeValue].encode(original)
      decoded must beRight(original)
      encoded must beRight(translation)
    }
    "simple case classes" >> {
      case class I(i: Int)
      val original = I(5)
      val translation = Map("i" -> AttributeValueInt(5))
      val encoded = Encoder[I, AttributeValue].encode(original)
      val decoded = Decoder[AttributeValue, I].decode(translation)
      decoded must beRight(original)
      encoded must beRight(translation)
    }
    "case classes" >> {
      case class Inner(j: String)
      case class Simple(i: Int, n: Inner)
      case class Outer(s: Simple)
      val original = Outer(Simple(123, Inner("hello")))
      val translation = Map("s" -> AttributeValueMap(
        Map("i" -> AttributeValue(123),
          "n" -> AttributeValueMap(Map("j" -> AttributeValue("hello"))))))
      val encoded = Encoder[Outer, AttributeValue].encode(original)
      val decoded = Decoder[AttributeValue, Outer].decode(translation)
      decoded must beRight(original)
      encoded must beRight(translation)
    }
    "ADTs" >> {
      sealed trait ADT
      case class A(a: String) extends ADT
      case class B(b: String) extends ADT
      case class Outer(a: ADT, b: ADT) extends ADT
      val original = Outer(A("AAA"), B("BBB"))
      val translation = Map("a" -> AttributeValueMap(Map("a" -> AttributeValueString("AAA"))),
        "b" -> AttributeValueMap(Map("b" -> AttributeValueString("BBB"))))
      val encoded = Encoder[Outer, AttributeValue].encode(original)
      val decoded = Decoder[AttributeValue, Outer].decode(translation)
      decoded must beRight(original)
      encoded must beRight(translation)
    }
  }
}
