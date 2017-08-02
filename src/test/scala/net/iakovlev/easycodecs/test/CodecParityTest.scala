package net.iakovlev.easycodecs.test

import net.iakovlev.easycodecs.decoder.Decoder
import net.iakovlev.easycodecs.encoder.Encoder
import org.specs2.mutable.Specification

class CodecParityTest extends Specification {
  "Encoder and Decoder should be on par in terms of" >> {
    def checkParity[A](original: A, translation: Map[String, AttributeValue])(
        implicit e: Encoder[A, AttributeValue],
        d: Decoder[AttributeValue, A]) = {
      val decoded = d.decode(translation)
      val encoded = e.encode(original)
      decoded must beRight(original)
      encoded must beRight(translation)
    }
    "collections" >> {
      case class C(l: Vector[Int])
      val translation = Map("l" -> AttributeValueList(
        List(AttributeValueInt(1), AttributeValueInt(2), AttributeValueInt(3))))
      val original = C(Vector(1, 2, 3))
      checkParity(original, translation)
    }
    "maps" >> {
      case class M(m: Map[String, Long])
      val original = M(Map("hello" -> 555l))
      val translation =
        Map("m" -> AttributeValueMap(Map("hello" -> AttributeValueLong(555l))))
      checkParity(original, translation)
    }
    "simple case classes" >> {
      case class I(i: Int)
      val original = I(5)
      val translation = Map("i" -> AttributeValueInt(5))
      checkParity(original, translation)
    }
    "case classes" >> {
      case class Inner(j: String)
      case class Simple(i: Int, n: Inner)
      case class Outer(s: Simple)
      val original = Outer(Simple(123, Inner("hello")))
      val translation = Map(
        "s" -> AttributeValueMap(
          Map("i" -> AttributeValue(123),
              "n" -> AttributeValueMap(Map("j" -> AttributeValue("hello"))))))
      checkParity(original, translation)
    }
    "ADTs" >> {
      sealed trait ADT
      case class A(a: String) extends ADT
      case class B(b: String) extends ADT
      case class Outer(a: ADT, b: ADT) extends ADT
      val original = Outer(A("AAA"), B("BBB"))
      val translation =
        Map("a" -> AttributeValueMap(Map("a" -> AttributeValueString("AAA"))),
            "b" -> AttributeValueMap(Map("b" -> AttributeValueString("BBB"))))
      checkParity(original, translation)
    }
    "Options - None" >> {
      case class Maybe(i: Option[Int])
      val original = Maybe(None)
      val translation = Map[String, AttributeValue]()
      checkParity(original, translation)
    }
    "Options - Some" >> {
      case class Maybe(i: Option[Int])
      val original = Maybe(Some(4))
      val translation = Map("i" -> AttributeValueInt(4))
      checkParity(original, translation)
    }
    "Options - case classes None" >> {
      case class Thing(i: Int)
      case class Maybe(i: Option[Thing])
      val original = Maybe(None)
      val translation = Map[String, AttributeValue]()
      checkParity(original, translation)
    }
    "Options - case classes Some" >> {
      case class Thing(i: Int)
      case class Maybe(i: Option[Thing])
      val original = Maybe(Some(Thing(4)))
      val translation =
        Map("i" -> AttributeValueMap(Map("i" -> AttributeValueInt(4))))
      checkParity(original, translation)
    }
    "ADT-based enums as strings" >> {
      sealed abstract class ADT
      case object A extends ADT
      case object B extends ADT
      case class I(a: ADT, b: ADT)
      val original = I(A, B)
      val translation =
        Map("a" -> AttributeValueString("A"), "b" -> AttributeValueString("B"))
      checkParity(original, translation)
    }
  }
}
