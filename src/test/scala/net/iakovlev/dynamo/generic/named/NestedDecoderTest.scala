package net.iakovlev.dynamo.generic.named

import awscala.dynamodbv2.{Attribute, AttributeValue}
import org.specs2.mutable.Specification
import net.iakovlev.dynamo.generic.named._

class NestedDecoderTest extends Specification {
  sequential
  "Name-based generic decoder derivation facility should" >> {
    "Derive decoder for a simple case class" >> {
      case class Simple(s: String, t: String, n: Int, l: Long)
      val res = Decoder[Simple](
        Seq(
          Attribute("s", AttributeValue(s = Some("value"))),
          Attribute("t", AttributeValue(s = Some("value_t"))),
          Attribute("n", AttributeValue(n = Some("0"))),
          Attribute("l", AttributeValue(n = Some("11111222121212")))
        ))
      res must not be empty
      res.get must_== Simple("value", "value_t", 0, 11111222121212L)
    }
    "Report error for non-matching fields" >> {
      case class Simple(s: String)
      val res = Decoder[Simple](
        Seq(
          Attribute("t", AttributeValue(s = Some("value")))
        ))
      res must be empty
    }
    "Derive decoder for nested case classes" >> {
      case class Simple(simpleName: String)
      case class Another(anotherName: String, anotherNumber: Int)
      case class Parent(childField: Simple,
                        parentName: String,
                        anotherField: Another)
      val res = Decoder[Parent](
        Seq(
          Attribute("simpleName", AttributeValue(s = Some("value_s"))),
          Attribute("child", AttributeValue(s = Some("!!!!"))),
          Attribute("parentName", AttributeValue(s = Some("value_p"))),
          Attribute("anotherName", AttributeValue(s = Some("!!!!"))),
          Attribute("anotherNumber", AttributeValue(n = Some("42")))
        )
      )
      res must not be empty
      res.get must_== Parent(Simple("value_s"), "value_p", Another("!!!!", 42))
    }
    "Use custom decoders" >> {
      case class Custom(p: List[Int])
      case class Parent(c: Custom, s: String)
      implicit val customDecoder = new FieldDecoder[Custom] {
        override def decode(s: Seq[Attribute], n: String): Option[Custom] = {
          println(s"custom decoder for $n")
          s.find(_.name == n)
            .flatMap(_.value.s)
            .map(_.split(",").map(_.toInt).toList)
            .map(Custom)
        }
      }
      val res = Decoder[Parent](
        Seq(
          Attribute("s", AttributeValue(s = Some("value1"))),
          Attribute("c", AttributeValue(s = Some("1,2,3")))))
      res must not be empty
      res.get must_== Parent(Custom(List(1,2,3)), "value1")
    }
  }
}
