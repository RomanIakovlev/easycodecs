package net.iakovlev.dynamo.generic.test

import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import org.specs2.mutable.Specification
import com.amazonaws.services.dynamodbv2.{model => aws}

class DecodeMapAsClassTest
    extends Specification
    with AwsAttributeValueDecoder {

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
    res must beRight(Parent("hello", Custom(Child("lol"), "world")))
  }
}
