package net.iakovlev.dynamo.generic.test

import com.amazonaws.services.dynamodbv2.{model => aws}
import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import org.specs2.mutable.Specification

class DecodeCaseObjectsTest
    extends Specification
    with AwsAttributeValueDecoder {

  "Decode case objects as strings" >> {
    sealed trait ADT
    case object A extends ADT
    case object B extends ADT
    case class O(a: ADT, b: ADT)
    val res =
      awsDecoder[O](
        Map("a" -> new aws.AttributeValue("A"),
          "b" -> new aws.AttributeValue("B")))
    res must beRight(O(A, B))
  }
}
