package net.iakovlev.dynamo.generic.test

import com.amazonaws.services.dynamodbv2.{model => aws}
import net.iakovlev.dynamo.generic._
import org.specs2.mutable.Specification

class DecodeScalarListsTest
    extends Specification
    with AwsAttributeValueDecoder {

  "Decode scalar lists" >> {
    case class Parent(cc: Seq[Int])
    val res = awsDecoder[Parent](
      Map("cc" -> new aws.AttributeValue().withL(new aws.AttributeValue().withN("1")))
    )
    res must beRight(Parent(Seq(1)))
  }
}
