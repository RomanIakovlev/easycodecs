package net.iakovlev.dynamo.generic.ordered

import awscala.dynamodbv2.{Attribute, AttributeValue}
import org.specs2.mutable.Specification

class OrderedDecoderTest extends Specification {
  "Ordered generic decoder derivation facility should" >> {
    "Derive decoder for a simple case class" >> {
      case class Simple(s: String, t: String, n: Int, l: Long)
      val res = Decoder[Simple](
        Seq(
          Attribute("sss", AttributeValue(s = Some("value"))),
          Attribute("ttt", AttributeValue(s = Some("value_t"))),
          Attribute("ttt", AttributeValue(n = Some("0"))),
          Attribute("ttt", AttributeValue(n = Some("11111222121212")))
        ))
      res must not be empty
      res.get must_== Simple("value", "value_t", 0, 11111222121212L)
    }
    "Derive decoder for nested case classes with >1 complex fields" >> {
      case class Child(s: String)
      case class Parent(c1: Child, c2: Child)
      val res = Decoder[Parent](
        Seq(
          Attribute("sss1", AttributeValue(s = Some("value1"))),
          Attribute("sss2", AttributeValue(s = Some("value2")))
        ))
      res must not be empty
      res.get must_== Parent(Child("value1"), Child("value2"))
    }
    "Derive decoder for nested case classes with primitive and complex fields" >> {
      case class Child(s: String)
      case class Parent(s: String, i: Int, c1: Child)
      val res = Decoder[Parent](
        Seq(
          Attribute("sss1", AttributeValue(s = Some("value1"))),
          Attribute("sss1", AttributeValue(n = Some("1"))),
          Attribute("sss2", AttributeValue(s = Some("value2")))
        ))
      res must not be empty
      res.get must_== Parent("value1", 1, Child("value2"))
    }
    "Derive decoder for nested case classes with complex and primitive fields" >> {
      case class Child(s: String)
      case class Parent(c1: Child, s: String)
      val res = Decoder[Parent](
        Seq(
          Attribute("sss1", AttributeValue(s = Some("value1"))),
          Attribute("sss2", AttributeValue(s = Some("value2")))
        ))
      res must not be empty
      res.get must_== Parent(Child("value1"), "value2")
    }
    "Derive decoder for deeply nested case classes structures" >> {
      case class L1(s: String)
      case class L2(s: String, l1: L1)
      case class L3(s: String, l2: L2)
      val res = Decoder[L3](
        Seq(
          Attribute("sss1", AttributeValue(s = Some("value1"))),
          Attribute("sss2", AttributeValue(s = Some("value2"))),
          Attribute("sss2", AttributeValue(s = Some("value3")))
        ))
      res must not be empty
      res.get must_== L3("value1", L2("value2", L1("value3")))
    }
    "Derive decoder for map" >> {
      case class MapValue(s: String)
      case class Parent(m: Map[String, MapValue])
      val res = Decoder[Parent](
        Seq(
          Attribute("",
                    AttributeValue(
                      AttributeValue.toJavaValue(Map("hello" -> "world"))))
        )
      )
      res must not be empty
      res.get must_== Parent(Map("hello" -> MapValue("world")))
    }
    "Use custom decoders" >> {
      case class Custom(p: List[Int])
      case class Parent(s: String, c: Custom)
      implicit val customDecoder = new Decoder[Custom] {
        override def decode(s: Seq[AttributeValue]): Option[Custom] = {
          s.headOption
            .flatMap(_.s)
            .map(_.split(",").map(_.toInt).toList)
            .map(Custom)
        }
      }
      val res = Decoder[Parent](
        Seq(
          Attribute("sss1", AttributeValue(s = Some("value1"))),
          Attribute("sss2", AttributeValue(s = Some("1,2,3")))))
      res must not be empty
      res.get must_== Parent("value1", Custom(List(1,2,3)))
    }
  }
}
