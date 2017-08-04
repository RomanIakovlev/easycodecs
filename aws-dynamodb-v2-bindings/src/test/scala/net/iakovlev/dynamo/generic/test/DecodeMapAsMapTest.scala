package net.iakovlev.dynamo.generic.test

import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import org.specs2.mutable.Specification
import scala.collection.JavaConverters._

class DecodeMapAsMapTest
    extends Specification
    with AwsAttributeValueDecoder
    with TestBase {

  "Decode map field as a simple map, not nested class" >> {
    // TODO has to be Map[String, F[_]], because of lack of Optional type class in Cats
    case class MapHostString(m: Map[String, String])
    val res = awsDecoder[MapHostString](
      Map("m" -> attr().m(Map("hello" -> attr("world")).asJava).build()))
    res must beRight(MapHostString(Map("hello" -> "world")))
    case class MapHostInt(m: Map[String, Int])
    val res1 = awsDecoder[MapHostInt](
      Map("m" -> attr().m(Map("hello" -> attr().n("123").build()).asJava).build()))
    res1 must beRight(MapHostInt(Map("hello" -> 123)))
  }
}
