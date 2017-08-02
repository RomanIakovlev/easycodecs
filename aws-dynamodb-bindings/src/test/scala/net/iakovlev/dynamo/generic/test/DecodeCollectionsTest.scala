package net.iakovlev.dynamo.generic.test

import com.amazonaws.services.dynamodbv2.{model => aws}
import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import org.specs2.mutable.Specification

class DecodeCollectionsTest
    extends Specification
    with AwsAttributeValueDecoder {

  "Decode all supported collections" >> {
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
    res must beRight(ListParent(
      List(child),
      Vector(child)
    ))
  }
}