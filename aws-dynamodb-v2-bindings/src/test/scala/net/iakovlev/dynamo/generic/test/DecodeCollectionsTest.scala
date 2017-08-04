package net.iakovlev.dynamo.generic.test

import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import org.specs2.mutable.Specification
import scala.collection.JavaConverters._

class DecodeCollectionsTest
    extends Specification
    with AwsAttributeValueDecoder
    with TestBase {

  "Decode all supported collections" >> {
    case class Child(s: String)
    case class ListParent(
        l: List[Child],
        v: Vector[Child]
    )
    val c = attr().l(attr().m(Map("s" -> attr("bla")).asJava).build()).build()
    val child = Child("bla")
    val res = awsDecoder[ListParent](
      Map(
        "l" -> c,
        "v" -> c
      )
    )
    res must beRight(
      ListParent(
        List(child),
        Vector(child)
      ))
  }
}
