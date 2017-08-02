package net.iakovlev.dynamo.generic.test

import com.amazonaws.services.dynamodbv2.{model => aws}
import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import net.iakovlev.easycodecs.decoder.ReadingError
import org.specs2.mutable.Specification

class DecodeOptionalFieldsTest
    extends Specification
    with AwsAttributeValueDecoder {

  "Decode optional fields" >> {
    case class Optional(o: Option[String])
    case class Optional1(o: Option[Int])
    val res1 =
      awsDecoder[Optional](Map("o" -> new aws.AttributeValue("123")))
    res1 must beRight(Optional(Some("123")))
    val res2 = awsDecoder[Optional](Map())
    res2 must beRight(Optional(None))
    val res3 =
      awsDecoder[Optional1](Map("o" -> new aws.AttributeValue("oeuo")))
    res3 must beLike {
      case Left(_: ReadingError) => ok
      case _ => ko("wrong exception type")
    }
  }
}
