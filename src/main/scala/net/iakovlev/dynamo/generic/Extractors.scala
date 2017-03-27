package net.iakovlev.dynamo.generic

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait Extractors[S] {
  implicit def extractInt: PrimitivesExtractor[S, Int]
  implicit def extractLong: PrimitivesExtractor[S, Long]
  implicit def extractFloat: PrimitivesExtractor[S, Float]
  implicit def extractDouble: PrimitivesExtractor[S, Double]
  implicit def extractBigDecimal: PrimitivesExtractor[S, BigDecimal]
  implicit def extractBoolean: PrimitivesExtractor[S, Boolean]
  implicit def extractString: PrimitivesExtractor[S, String]
  implicit def extractSeq[C[X] <: Seq[X]](
      implicit canBuildFrom: CanBuildFrom[C[Either[DecodingError, S]],
                                          Either[DecodingError, S],
                                          C[Either[DecodingError, S]]])
    : PrimitivesExtractor[S, C[Either[DecodingError, S]]]
  def extractMap: PrimitivesExtractor[S, Map[String, S]]
}
