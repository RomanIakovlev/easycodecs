package net.iakovlev.dynamo.generic.test

import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import net.iakovlev.easycodecs.decoder.ReadingError
import org.specs2.mutable.Specification

class DecodeOptionalFieldsTest
    extends Specification
    with AwsAttributeValueDecoder
with TestBase {

  "Decode optional fields" >> {
    case class Optional(o: Option[String])
    case class Optional1(o: Option[Int])
    val res1 =
      awsDecoder[Optional](Map("o" -> attr("123")))
    res1 must beRight(Optional(Some("123")))
    val res2 = awsDecoder[Optional](Map())
    res2 must beRight(Optional(None))
    val res3 =
      awsDecoder[Optional1](Map("o" -> attr("oeuo")))
    res3 must beLike {
      case Left(_: ReadingError) => ok
      case _ => ko("wrong exception type")
    }
  }
}
