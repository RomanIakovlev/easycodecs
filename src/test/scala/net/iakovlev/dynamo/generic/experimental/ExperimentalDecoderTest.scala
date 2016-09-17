package net.iakovlev.dynamo.generic.experimental

import org.specs2.mutable.Specification

class ExperimentalDecoderTest extends Specification {
  sequential
  "Experimental generic decoder derivation facility should" >> {
    "Use custom decoder" >> {
      case class Test(s: List[Int])
      val d = new Decoder[Test] {
        override def decode(attributes: Map[String, AttributeValue]): Option[Test] = {
          attributes.get("s").flatMap {
            case AttributeValueString(value) => Some(Test(value.split(",").map(_.toInt).toList))
            case _ => None
          }
        }
      }
      val res = d.decode(Map("s" -> AttributeValue("1,2,3")))
      res must not be empty
      res.get must_== Test(List(1,2,3))
    }
    "Use map based decoder for nested classes" >> {
      case class Child(s: String)
      case class Custom(c: Child, s: String)
      case class Parent(s: String, c: Custom)
      val res = Decoder[Parent](
        Map("s" -> AttributeValue("hello"),
            "c" -> AttributeValueMap(
              Map("s" -> AttributeValue("world"),
                  "c" -> AttributeValueMap(
                    Map("s" -> AttributeValue("lol")))))))
      res must not be empty
      res.get must_== Parent("hello", Custom(Child("lol"), "world"))
    }
    "Decode case classes lists" >> {
      case class Child(s: String)
      case class Parent(c: List[Child])
      val res = Decoder[Parent](
        Map("c" -> AttributeValueList(List(AttributeValueMap(Map("s" -> AttributeValue("bla"))))))
      )
      res must not be empty
      res.get must_== Parent(List(Child("bla")))
    }
    "Decode scalar lists" >> {
      case class Parent(c: List[String])
      val res = Decoder[Parent](
        Map("c" -> AttributeValueList(List(AttributeValue("bla"))))
      )
      res must not be empty
      res.get must_== Parent(List("bla"))
    }
    "Decode optional fields" >> {
      case class Optional(o: Option[String])
      val res1 = Decoder[Optional](Map("o" -> AttributeValue("123")))
      res1 must not be empty
      res1.get must_== Optional(Some("123"))
      val res2 = Decoder[Optional](Map())
      res2 must not be empty
      res2.get must_== Optional(None)
    }
  }
}
