package net.iakovlev.dynamo.generic

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait Extractors[F[_], S] {
  implicit def extractInt: PrimitivesExtractor[F, S, Int]
  implicit def extractLong: PrimitivesExtractor[F, S, Long]
  implicit def extractFloat: PrimitivesExtractor[F, S, Float]
  implicit def extractDouble: PrimitivesExtractor[F, S, Double]
  implicit def extractBigDecimal: PrimitivesExtractor[F, S, BigDecimal]
  implicit def extractBoolean: PrimitivesExtractor[F, S, Boolean]
  implicit def extractString: PrimitivesExtractor[F, S, String]
  implicit def extractSeq[C[X] <: Seq[X]](implicit canBuildFrom: CanBuildFrom[C[F[S]], F[S], C[F[S]]])
    : PrimitivesExtractor[F, S, C[F[S]]]
   def extractMap: PrimitivesExtractor[F, S, Map[String, S]]
}