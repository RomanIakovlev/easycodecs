package net.iakovlev.dynamo.generic.experimental

import org.specs2.mutable.Specification

class ExperimentalEncoderTest extends Specification {
  "Experimental generic encoder derivation facility should" >> {
    "Encode simple case classes" >> {
      case class Simple(s: String, i: Int)
      val res = Encoder[Simple](Simple("123", 456))
      res must_== Map("s" -> AttributeValue("123"), "i" -> AttributeValue(456))
    }
    "Encode case class with map" >> {
      case class MapHost(m: Map[String, Int])
      val res = Encoder[MapHost](MapHost(Map("h" -> 1)))
      res must_== Map("m" -> AttributeValue("h" -> AttributeValue(1)))
    }
    "Encode nested case class as map" >> {
      case class Nested(s: String)
      case class Parent(s: String, n: Nested)
      val res = Encoder[Parent](Parent("hello", Nested("world")))
      res must_== Map("n" -> AttributeValue("s" -> AttributeValue("world")),
                      "s" -> AttributeValue("hello"))
    }
    "Encode scalar list fields" >> {
      case class ListHost(l: List[String])
      val res = Encoder[ListHost](ListHost(List("hello", "world")))
      res must_== Map(
        "l" -> AttributeValue(AttributeValue("hello"),
                              AttributeValue("world")))
    }
    "Encode document list fields" >> {
      case class Nested(s: String)
      case class Parent(l: List[Nested])
      val res = Encoder[Parent](Parent(List(Nested("hello"), Nested("world"))))
      res must_== Map(
        "l" -> AttributeValue(AttributeValue("s" -> AttributeValue("hello")),
                              AttributeValue("s" -> AttributeValue("world"))))
    }
    "Encode optional fields" >> {
      case class Optional(o: Option[String])
      val res = Encoder[Optional](Optional(Some("s")))
      res must_== Map("o" -> AttributeValue("s"))
      val res1 = Encoder[Optional](Optional(None))
      res1 must_== Map.empty
    }
  }
}
