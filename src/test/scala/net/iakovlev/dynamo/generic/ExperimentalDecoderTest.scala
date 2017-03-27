package net.iakovlev.dynamo.generic

import org.specs2.mutable.Specification

class ExperimentalDecoderTest extends Specification {
  "Experimental generic decoder derivation facility should" >> {
    "Use custom decoder" >> {
      case class Test(s: List[Int])
      val d = new OptionalDecoder[Test] {
        override def decode(
            attributes: Map[String, AttributeValue]): Option[Test] = {
          attributes.get("s").flatMap {
            case AttributeValueString(value) =>
              Some(Test(value.split(",").map(_.toInt).toList))
            case _ => None
          }
        }
      }
      val res = d.decode(Map("s" -> AttributeValue("1,2,3")))
      res must not be empty
      res.get must_== Test(List(1, 2, 3))
    }
    "Decode scalar lists" >> {
      case class Parent(c: List[String])
      val res = OptionalDecoder[Parent](
        Map("c" -> AttributeValueList(List(AttributeValue("bla"))))
      )
      res must not be empty
      res.get must_== Parent(List("bla"))
    }
    "Decode optional fields" >> {
      case class Optional(o: Option[String])
      val res1 = OptionalDecoder[Optional](Map("o" -> AttributeValue("123")))
      res1 must not be empty
      res1.get must_== Optional(Some("123"))
      val res2 = OptionalDecoder[Optional](Map())
      res2 must not be empty
      res2.get must_== Optional(None)
    }
    "Fail to decode optional fields if attribute has wrong type" >> {
      case class Optional(o: Option[String])
      val res = OptionalDecoder[Optional](Map("o" -> AttributeValue(123)))
      res must beEmpty
    }
    "Decode map field as a simple map, not nested class" >> {
      case class MapHostString(m: Map[String, String])
      val res = OptionalDecoder[MapHostString](
        Map("m" -> AttributeValue("hello" -> AttributeValue("world"))))
      res must not be empty
      res.get must_== MapHostString(Map("hello" -> "world"))
      case class MapHostInt(m: Map[String, Int])
      val res1 = OptionalDecoder[MapHostInt](
        Map("m" -> AttributeValue("hello" -> AttributeValue(123))))
      res1 must not be empty
      res1.get must_== MapHostInt(Map("hello" -> 123))
    }
    "Decode all numeric attributes correctly" >> {
      case class AllNumerals(i: Int,
                             l: Long,
                             f: Float,
                             d: Double,
                             b: BigDecimal,
                             n: BigDecimal)
      val res =
        OptionalDecoder[AllNumerals](
          Map("i" -> AttributeValue(1),
              "l" -> AttributeValue(2l),
              "f" -> AttributeValue(3.0f),
              "d" -> AttributeValue(4.0d),
              "b" -> AttributeValue(BigDecimal(1000l)),
              "n" -> AttributeValueNumeric("2000")))
      res must not be empty
      res.get must_== AllNumerals(1, 2l, 3.0f, 4.0d, BigDecimal(1000l), BigDecimal(2000l))
    }
    "Decode into ADT" >> {
      sealed trait ADT
      case class A(a: String) extends ADT
      case class B(b: String) extends ADT
      case class O(a: ADT, b: ADT)
      val res = OptionalDecoder[O](
        Map("a" -> AttributeValue("a" -> AttributeValue("AAA")),
            "b" -> AttributeValue("b" -> AttributeValue("BBB"))))
      res must not be empty
      res.get must_== O(A("AAA"), B("BBB"))
    }
    "Decode case objects as strings" >> {
      sealed trait ADT
      case object A extends ADT
      case object B extends ADT
      case class O(a: ADT, b: ADT)
      val res = OptionalDecoder[O](Map("a" -> AttributeValue("A"), "b" -> AttributeValue("B")))
      res must not be empty
      res.get must_== O(A, B)
    }
  }
}
