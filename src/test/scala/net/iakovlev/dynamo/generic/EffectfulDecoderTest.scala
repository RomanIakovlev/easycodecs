package net.iakovlev.dynamo.generic

import org.specs2.mutable.Specification

class EffectfulDecoderTest extends Specification {
  "Decoder should" >> {
    "decode case classes" >> {
      case class Inner(j: String)
      case class Simple(i: Int, n: Inner)
      case class Outer(s: Simple)

      val d = Decoder[AttributeValue, Outer](
        Map(
          "s" -> AttributeValueMap(
            Map("i" -> AttributeValue(123),
                "n" -> AttributeValueMap(Map("j" -> AttributeValue("hello"))))))
      )
      d must beRight(Outer(Simple(123, Inner("hello"))))
    }
  }
  "Encoder should" >> {
    "encode case classes" >> {
      case class Inner(j: String)
      case class Simple(i: Int, n: Inner)
      case class Outer(s: Simple)

      val e = Encoder[Outer, AttributeValue].encode(
        Outer(Simple(123, Inner("hello"))))
      e must beRight(
        Map("s" -> AttributeValueMap(
          Map("i" -> AttributeValue(123),
              "n" -> AttributeValueMap(Map("j" -> AttributeValue("hello")))))))
    }
    "encode ADTs" >> {
      sealed trait ADT
      case class A(a: String) extends ADT
      case class B(b: String) extends ADT
      case class Outer(a: ADT, b: ADT) extends ADT
      val res = Encoder[Outer, AttributeValue].encode(Outer(A("AAA"), B("BBB")))
      res must beRight(
        Map("a" -> AttributeValueMap(Map("a" -> AttributeValueString("AAA"))),
            "b" -> AttributeValueMap(Map("b" -> AttributeValueString("BBB")))))
    }
  }
}
