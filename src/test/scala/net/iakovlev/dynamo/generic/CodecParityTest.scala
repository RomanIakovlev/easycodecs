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
      println("!!! decoded " + decoded)
      decoded must beRight(original)
      val encoded = Encoder[C, AttributeValue].encode(original)
      println("!!! encoded " + encoded)
      encoded must beRight(translation)
    }
  }
  /*"Decoder should" >> {
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
    "decode collections" >> {

    }
  }
  "Encoder should" >> {
    "encode simple case classes" >> {
      case class I(i: Int)
      Encoder[I, AttributeValue].encode(I(5)) should beRight(
        Map("i" -> AttributeValueInt(5)))
    }
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
    "encode primitives collections" >> {
      case class C(l: Vector[Int])
      val res = Encoder[C, AttributeValue].encode(C(Vector(1, 2, 3)))
      println("!!!!!!!!!!!!" + res)
      res must beRight(
        Map(
          "l" -> AttributeValueList(
            List(AttributeValueInt(1),
                 AttributeValueInt(2),
                 AttributeValueInt(3))))
      )
    }
  }*/
}
