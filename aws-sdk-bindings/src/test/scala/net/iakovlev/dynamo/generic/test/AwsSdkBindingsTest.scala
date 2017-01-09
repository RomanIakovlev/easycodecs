package net.iakovlev.dynamo.generic.test

import net.iakovlev.dynamo.generic.{AwsAttributeValueDecoder, EffectfulDecoder}
import org.specs2.mutable.Specification
import com.amazonaws.services.dynamodbv2.{AmazonDynamoDBClient, model => aws}
import com.amazonaws.services.dynamodbv2.document.DynamoDB
import com.amazonaws.services.dynamodbv2.model._
import cats.implicits._

import scala.collection.JavaConverters._
import scala.collection.generic.CanBuildFrom
import scala.util.{Failure, Success, Try}

class AwsSdkBindingsTest extends Specification with AwsAttributeValueDecoder {
  def awsDecoder[A](m: Map[String, aws.AttributeValue])(
      implicit d: EffectfulDecoder[Try, aws.AttributeValue, A]) =
    d.decode(m)
  case class Test(s: List[Int])
  object Test {
    implicit val d = new EffectfulDecoder[Try, aws.AttributeValue, Test] {
      override def decode(
                           attributes: Map[String, aws.AttributeValue]): Try[Test] = {
        for {
          attrValue <- Try(attributes("s"))
          value <- Try(attrValue.getS.split(",").map(_.toInt).toList)
        } yield Test(value)
      }
    }
  }
  "AWS bindings for generic effectful decoder derivation facility should" >> {
    "Use custom decoder" >> {
      // Class Test has to be defined outside of this spec, because specs2 breaks companion objects
      val res = awsDecoder[Test](Map("s" -> new aws.AttributeValue().withS("1,2,3")))
      res must_== Success(Test(List(1, 2, 3)))
    }
    "Use map based decoder for nested classes" >> {
      case class Child(s: String)
      case class Custom(c: Child, s: String)
      case class Parent(s: String, c: Custom)
      val res = awsDecoder[Parent](
        Map(
          "s" -> new aws.AttributeValue("hello"),
          "c" -> new aws.AttributeValue()
            .addMEntry("s", new aws.AttributeValue("world"))
            .addMEntry("c",
                       new aws.AttributeValue()
                         .addMEntry("s", new aws.AttributeValue("lol")))
        ))
      res must_== Success(Parent("hello", Custom(Child("lol"), "world")))
    }
    "Decode case classes lists" >> {
      case class Child(s: String)
      case class Parent(c: Vector[Child])
      val res = awsDecoder[Parent](
        Map(
          "c" -> new aws.AttributeValue()
            .withL(new aws.AttributeValue()
              .addMEntry("s", new aws.AttributeValue("bla")))))

      res must_== Success(Parent(Vector(Child("bla"))))
    }
    "Decode all supported collections" >> {
      //only collections with instances of cats.Traverse are supported
      case class Child(s: String)
      case class ListParent(
          l: List[Child],
          v: Vector[Child]
      )
      val c = new aws.AttributeValue()
        .withL(new aws.AttributeValue()
          .addMEntry("s", new aws.AttributeValue("bla")))
      val child = Child("bla")
      val res = awsDecoder[ListParent](
        Map(
          "l" -> c,
          "v" -> c
        )
      )
      res must_== Success(
        ListParent(
          List(child),
          Vector(child)
        ))
    }
    "Decode scalar lists" >> {
      case class Parent(c: List[String])
      val res = awsDecoder[Parent](
        Map(
          "c" -> new aws.AttributeValue()
            .withL(List(new aws.AttributeValue("bla")).asJava))
      )
      res must_== Success(Parent(List("bla")))
    }
    "Decode optional fields" >> {
      case class Optional(o: Option[String])
      val res1 =
        awsDecoder[Optional](Map("o" -> new aws.AttributeValue("123")))
      res1 must_== Success(Optional(Some("123")))
      val res2 = awsDecoder[Optional](Map())
      res2 must_== Success(Optional(None))
    }
    "Decode map field as a simple map, not nested class" >> {
      // TODO has to be Map[String, F[_]], because of lack of Optional type class in Cats
      case class MapHostString(m: Map[String, Try[String]])
      val res = awsDecoder[MapHostString](
        Map("m" -> new aws.AttributeValue()
          .addMEntry("hello", new aws.AttributeValue("world"))))
      res must_== Success(MapHostString(Map("hello" -> Success("world"))))
      case class MapHostInt(m: Map[String, Try[Int]])
      val res1 = awsDecoder[MapHostInt](
        Map("m" -> new aws.AttributeValue()
          .addMEntry("hello", new aws.AttributeValue().withN("123"))))
      res1 must_== Success(MapHostInt(Map("hello" -> Success(123))))
    }
    "Decode all numeric attributes correctly" >> {
      case class AllNumerals(i: Int,
                             l: Long,
                             f: Float,
                             d: Double,
                             b: BigDecimal)
      val res =
        awsDecoder[AllNumerals](
          Map(
            "i" -> new aws.AttributeValue().withN("1"),
            "l" -> new aws.AttributeValue().withN("2"),
            "f" -> new aws.AttributeValue().withN("3.0"),
            "d" -> new aws.AttributeValue().withN("4.0"),
            "b" -> new aws.AttributeValue().withN("1000")
          ))
      res must_== Success(AllNumerals(1, 2l, 3.0f, 4.0d, BigDecimal(1000l)))
    }
    "Decode into ADT" >> {
      sealed trait ADT
      case class A(a: String) extends ADT
      case class B(b: String) extends ADT
      case class O(a: ADT, b: ADT)
      val res = awsDecoder[O](
        Map("a" -> new aws.AttributeValue()
              .addMEntry("a", new aws.AttributeValue("AAA")),
            "b" -> new aws.AttributeValue()
              .addMEntry("b", new aws.AttributeValue("BBB"))))
      res must_== Success(O(A("AAA"), B("BBB")))
    }
    "Decode case objects as strings" >> {
      sealed trait ADT
      case object A extends ADT
      case object B extends ADT
      case class O(a: ADT, b: ADT)
      val res =
        awsDecoder[O](
          Map("a" -> new aws.AttributeValue("A"),
              "b" -> new aws.AttributeValue("B")))
      res must_== Success(O(A, B))
    }
  }
}
