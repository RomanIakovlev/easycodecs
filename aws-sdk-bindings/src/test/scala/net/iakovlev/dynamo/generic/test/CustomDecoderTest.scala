package net.iakovlev.dynamo.generic.test

import cats.implicits._
import com.amazonaws.services.dynamodbv2.{model => aws}
import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import net.iakovlev.easycodecs._
import net.iakovlev.easycodecs.decoder.{Decoder, DecodingError}
import org.specs2.mutable.Specification

class CustomDecoderTest extends Specification with AwsAttributeValueDecoder {
  case class Test(s: List[Int])
  object Test {
    implicit val d = Decoder.instance[aws.AttributeValue, Test] { attributes =>
      for {
        attrValue <- Right[DecodingError, aws.AttributeValue](attributes("s"))
        value <- Right[DecodingError, List[Int]](
          attrValue.getS.split(",").map(_.toInt).toList)
      } yield Test(value)
    }
  }

  "Use custom decoder" >> {
    // Class Test has to be defined outside of this spec, because specs2 breaks companion objects
    val res =
      awsDecoder[Test](Map("s" -> new aws.AttributeValue().withS("1,2,3")))
    res must beRight(Test(List(1, 2, 3)))
  }

}
