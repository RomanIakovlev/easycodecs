package net.iakovlev.easycodecs.decoder

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait Readers[A] {
  implicit def readInt: PrimitivesReader[A, Int]
  implicit def readLong: PrimitivesReader[A, Long]
  implicit def readFloat: PrimitivesReader[A, Float]
  implicit def readDouble: PrimitivesReader[A, Double]
  implicit def readBigDecimal: PrimitivesReader[A, BigDecimal]
  implicit def readBoolean: PrimitivesReader[A, Boolean]
  implicit def readString: PrimitivesReader[A, String]
  implicit def readIterable[C[X] <: Iterable[X]](
      implicit canBuildFrom: CanBuildFrom[C[A], A, C[A]])
    : PrimitivesReader[A, C[A]]
  implicit def readMap: PrimitivesReader[A, Map[String, A]]
}
