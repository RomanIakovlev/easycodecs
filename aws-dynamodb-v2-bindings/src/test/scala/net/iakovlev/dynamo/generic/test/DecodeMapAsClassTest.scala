package net.iakovlev.dynamo.generic.test

import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import org.specs2.mutable.Specification
import scala.collection.JavaConverters._

class DecodeMapAsClassTest
    extends Specification
    with AwsAttributeValueDecoder
    with TestBase {

  "Use map based decoder for nested classes" >> {
    case class Child(s: String)
    case class Custom(c: Child, s: String)
    case class Parent(s: String, c: Custom)
    val res = awsDecoder[Parent](
      Map(
        "s" -> attr("hello"),
        "c" -> attr()
          .m(
            Map("s" -> attr("world"),
                "c" -> attr().m(Map("s" -> attr("lol")).asJava).build()).asJava)
          .build()
      ))
    res must beRight(Parent("hello", Custom(Child("lol"), "world")))
  }
}
