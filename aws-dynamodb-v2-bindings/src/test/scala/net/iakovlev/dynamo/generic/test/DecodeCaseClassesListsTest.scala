package net.iakovlev.dynamo.generic.test

import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import org.specs2.mutable.Specification
import scala.collection.JavaConverters._

class DecodeCaseClassesListsTest
    extends Specification
    with AwsAttributeValueDecoder
    with TestBase {

  "Decode case classes lists" >> {
    case class Child(s: String)
    case class Parent(c: Vector[Child])
    val res = awsDecoder[Parent](
      Map(
        "c" -> attr()
          .l(attr().m(Map("s" -> attr("bla")).asJava).build())
          .build()))

    res must beRight(Parent(Vector(Child("bla"))))
  }
}
