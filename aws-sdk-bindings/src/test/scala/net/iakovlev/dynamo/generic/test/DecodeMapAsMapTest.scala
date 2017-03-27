package net.iakovlev.dynamo.generic.test

import com.amazonaws.services.dynamodbv2.{model => aws}
import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import org.specs2.mutable.Specification

class DecodeMapAsMapTest
    extends Specification
    with AwsAttributeValueDecoder {

  "Decode map field as a simple map, not nested class" >> {
    // TODO has to be Map[String, F[_]], because of lack of Optional type class in Cats
    case class MapHostString(m: Map[String, String])
    val res = awsDecoder[MapHostString](
      Map("m" -> new aws.AttributeValue()
        .addMEntry("hello", new aws.AttributeValue("world"))))
    res must beRight(MapHostString(Map("hello" -> "world")))
    case class MapHostInt(m: Map[String, Int])
    val res1 = awsDecoder[MapHostInt](
      Map("m" -> new aws.AttributeValue()
        .addMEntry("hello", new aws.AttributeValue().withN("123"))))
    res1 must beRight(MapHostInt(Map("hello" -> 123)))
  }
}
