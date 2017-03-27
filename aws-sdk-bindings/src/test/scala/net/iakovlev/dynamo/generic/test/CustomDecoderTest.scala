package net.iakovlev.dynamo.generic.test

import net.iakovlev.dynamo.generic.{AwsAttributeValueDecoder, Decoder}
import org.specs2.mutable.Specification
import com.amazonaws.services.dynamodbv2.{AmazonDynamoDBClient, model => aws}
import com.amazonaws.services.dynamodbv2.document.DynamoDB
import com.amazonaws.services.dynamodbv2.model._
import cats.implicits._
import org.specs2.matcher.ValueCheck

import scala.collection.JavaConverters._
import scala.collection.generic.CanBuildFrom
import scala.util.{Failure, Success, Try}
import net.iakovlev.dynamo.generic.DecodingError

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
