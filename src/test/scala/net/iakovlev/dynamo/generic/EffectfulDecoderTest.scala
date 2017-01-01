package net.iakovlev.dynamo.generic

import cats.data.Kleisli
import cats.implicits._
import org.specs2.mutable.Specification

import scala.util.{Failure, Success, Try}

//import scala.language.implicitConversions

/*class EffectfulDecoderTest extends Specification {
  "Monadic decoder should" >> {
    "decode simple case class" >> {
      case class Inner(j: String)
      case class Simple(i: Int, n: Inner)
      case class Outer(s: Simple)
      implicit def intAwsDecoder: SingleFieldEffectfulDecoder[Try,
                                                            AttributeValue,
                                                            Int] =
        new SingleFieldEffectfulDecoder[Try, AttributeValue, Int] {
          override def decode(a: AttributeValue): Try[Int] =
            a match {
              case AttributeValueInt(value) => Success(value)
              case _ => Failure(new Exception("Wrong attribute type"))
            }
        }
      implicit def attrMapAsMap(
          attributeValue: AttributeValue): Map[String, AttributeValue] =
        attributeValue match {
          case AttributeValueMap(value) => value
        }

      val d = EffectfulDecoder[AttributeValue, Outer](
        Map("s" -> AttributeValueMap(
          Map("i" -> AttributeValue(123),
              "n" -> AttributeValueMap(Map("j" -> AttributeValue("hello"))))))
      )
      d must_== Success(Outer(Simple(123, Inner("hello"))))
    }
  }
}*/
