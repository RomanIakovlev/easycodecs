package net.iakovlev.dynamo.generic.test

import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import org.specs2.mutable.Specification
import scala.collection.JavaConverters._

class DecodeADTTest
    extends Specification
    with AwsAttributeValueDecoder
    with TestBase {

  "Decode into ADT" >> {
    sealed trait ADT
    case class A(a: String) extends ADT
    case class B(b: String) extends ADT
    case class O(a: ADT, b: ADT) extends ADT
    val res = awsDecoder[O](
      Map(
        "a" -> attr().m(Map("a" -> attr("AAA")).asJava).build(),
        "b" -> attr().m(Map("b" -> attr("BBB")).asJava).build()))
    res must beRight(O(A("AAA"), B("BBB")))
  }
}
