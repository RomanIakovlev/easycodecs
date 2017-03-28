package net.iakovlev.dynamo.generic.test

import com.amazonaws.services.dynamodbv2.{model => aws}
import net.iakovlev.dynamo.generic.{AwsAttributeValueDecoder, Decoder}
import org.specs2.mutable.Specification

class DecodeADTTest
    extends Specification
    with AwsAttributeValueDecoder {

  "Decode into ADT" >> {
    sealed trait ADT
    case class A(a: String) extends ADT
    case class B(b: String) extends ADT
    case class O(a: ADT, b: ADT) extends ADT
    val res = awsDecoder[O](
      Map("a" -> new aws.AttributeValue()
        .addMEntry("a", new aws.AttributeValue("AAA")),
        "b" -> new aws.AttributeValue()
          .addMEntry("b", new aws.AttributeValue("BBB"))))
    res must beRight(O(A("AAA"), B("BBB")))
  }
}
