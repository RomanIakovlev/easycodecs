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
      implicit canBuildFrom: CanBuildFrom[C[S], S, C[S]])
    : PrimitivesExtractor[S, C[S]]
  implicit def extractMap: PrimitivesExtractor[S, Map[String, S]]
}

trait Writers[S] {
  implicit def writeInt: PrimitivesWriter[Int, S]
  implicit def writeLong: PrimitivesWriter[Long, S]
  implicit def writeFloat: PrimitivesWriter[Float, S]
  implicit def writeDouble: PrimitivesWriter[Double, S]
  implicit def writeBigDecimal: PrimitivesWriter[BigDecimal, S]
  implicit def writeBoolean: PrimitivesWriter[Boolean, S]
  implicit def writeString: PrimitivesWriter[String, S]
  implicit def writeSeq[C[X] <: Seq[X]](
      implicit canBuildFrom: CanBuildFrom[C[S], S, C[S]])
    : PrimitivesWriter[C[S], S]
  implicit def writeMap: PrimitivesWriter[Map[String, S], S]
}
