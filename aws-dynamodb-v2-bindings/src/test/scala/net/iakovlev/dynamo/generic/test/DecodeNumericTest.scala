package net.iakovlev.dynamo.generic.test

import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import org.specs2.mutable.Specification

class DecodeNumericTest
    extends Specification
    with AwsAttributeValueDecoder
with TestBase{

  "Decode all numeric attributes correctly" >> {
    case class AllNumerals(i: Int,
                           l: Long,
                           f: Float,
                           d: Double,
                           b: BigDecimal)
    val res =
      awsDecoder[AllNumerals](
        Map(
          "i" -> attr().n("1").build(),
          "l" -> attr().n("2").build(),
          "f" -> attr().n("3.0").build(),
          "d" -> attr().n("4.0").build(),
          "b" -> attr().n("1000").build()
        ))
    res must beRight(AllNumerals(1, 2l, 3.0f, 4.0d, BigDecimal(1000l)))
  }
}
