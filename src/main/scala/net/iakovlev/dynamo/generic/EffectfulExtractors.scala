package net.iakovlev.dynamo.generic

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.control.NoStackTrace

abstract class Error extends Exception with NoStackTrace

trait Extractors[S] {
  implicit def extractInt: PrimitivesExtractor[S, Int]
  implicit def extractLong: PrimitivesExtractor[S, Long]
  implicit def extractFloat: PrimitivesExtractor[S, Float]
  implicit def extractDouble: PrimitivesExtractor[S, Double]
  implicit def extractBigDecimal: PrimitivesExtractor[S, BigDecimal]
  implicit def extractBoolean: PrimitivesExtractor[S, Boolean]
  implicit def extractString: PrimitivesExtractor[S, String]
  implicit def extractSeq[C[X] <: Seq[X]](implicit canBuildFrom: CanBuildFrom[C[S], S, C[S]])
    : PrimitivesExtractor[S, C[S]]
   def extractMap: PrimitivesExtractor[S, Map[String, S]]
}