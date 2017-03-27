package net.iakovlev.dynamo.generic.test

import com.amazonaws.services.dynamodbv2.{model => aws}
import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import org.specs2.mutable.Specification

class DecodeNumericTest
    extends Specification
    with AwsAttributeValueDecoder {

  "Decode all numeric attributes correctly" >> {
    case class AllNumerals(i: Int,
                           l: Long,
                           f: Float,
                           d: Double,
                           b: BigDecimal)
    val res =
      awsDecoder[AllNumerals](
        Map(
          "i" -> new aws.AttributeValue().withN("1"),
          "l" -> new aws.AttributeValue().withN("2"),
          "f" -> new aws.AttributeValue().withN("3.0"),
          "d" -> new aws.AttributeValue().withN("4.0"),
          "b" -> new aws.AttributeValue().withN("1000")
        ))
    res must beRight(AllNumerals(1, 2l, 3.0f, 4.0d, BigDecimal(1000l)))
  }
}
