package net.iakovlev.dynamo.generic

import cats.syntax.either._
import cats.instances.either._
import cats.syntax.cartesian._
import shapeless.{:+:, _}
import shapeless.labelled.{FieldType, _}

import scala.language.higherKinds
import scala.util.Left
import scala.util.control.NoStackTrace

abstract sealed class EncodingError extends Throwable with NoStackTrace

class OtherEncodingError extends EncodingError

trait PrimitivesWriter[A, B] {
  def write(a: A): B
}

trait CoproductEncoder[A, B] {
  def encode(a: A): Either[EncodingError, B]
}

object CoproductEncoder {

  implicit def cNilEncoder[A]: CoproductEncoder[CNil, A] =
    new CoproductEncoder[CNil, A] {
      override def encode(a: CNil): Either[EncodingError, A] =
        Left(new OtherEncodingError)
    }

  implicit def cConsEncoder[A, H, T <: Coproduct](
      implicit eh: SingleFieldEncoder[H, A],
      et: SingleFieldEncoder[T, A]): CoproductEncoder[H :+: T, A] =
    new CoproductEncoder[H :+: T, A] {
      override def encode(a: H :+: T): Either[EncodingError, A] = {
        a match {
          case Inl(head) => eh.encode(head)
          case Inr(tail) => et.encode(tail)
        }
      }
    }

  implicit def coproductEncoder[A, B, Repr <: Coproduct](
      implicit lg: Generic.Aux[A, Repr],
      e: Lazy[CoproductEncoder[Repr, B]]): CoproductEncoder[A, B] =
    new CoproductEncoder[A, B] {
      override def encode(a: A): Either[EncodingError, B] = {
        e.value.encode(lg.to(a))
      }
    }
}

trait SingleFieldEncoder[A, B] {
  def encode(a: A): Either[EncodingError, B]
}

object SingleFieldEncoder {
  def instance[A, B](
      f: A => Either[EncodingError, B]): SingleFieldEncoder[A, B] =
    new SingleFieldEncoder[A, B] {
      override def encode(a: A): Either[EncodingError, B] = {
        f(a)
      }
    }
  implicit def coproductEncoder[A, B](
      implicit e: CoproductEncoder[A, B],
      lp: LowPriority): SingleFieldEncoder[A, B] =
    instance { a =>
      e.encode(a)
    }
  implicit def classAsMapEncoder[A, B](
      implicit e: Encoder[A, B],
      w: PrimitivesWriter[Map[String, B], B]): SingleFieldEncoder[A, B] =
    instance { a =>
      for {
        b <- e.encode(a)
      } yield w.write(b)
    }
  implicit def primitivesEncoder[A, B](
      implicit primitivesWriter: PrimitivesWriter[A, B])
    : SingleFieldEncoder[A, B] =
    instance { a =>
      Right(primitivesWriter.write(a))
    }

}

trait Encoder[A, B] {
  def encode(a: A): Either[EncodingError, Map[String, B]]
}

object Encoder {

  type Result[A] = Either[EncodingError, Map[String, A]]

  implicit def encodeHNil[A]: Encoder[HNil, A] =
    new Encoder[HNil, A] {
      override def encode(a: HNil): Result[A] = Right(Map.empty)
    }
  implicit def encodeHCons[A, K <: Symbol, H, T <: HList](
      implicit eh: SingleFieldEncoder[H, A],
      et: Lazy[Encoder[T, A]],
      k: Witness.Aux[K]): Encoder[FieldType[K, H] :: T, A] =
    new Encoder[FieldType[K, H] :: T, A] {
      override def encode(a: FieldType[K, H] :: T): Result[A] = {
        (et.value.encode(a.tail) |@| eh.encode(a.head)).map { (t, h) =>
          t + (k.value.name -> h)
        }
      }
    }
  // LowPriority and Strict to allow for SingleFieldEncoder#classAsMapEncoder to take over
  implicit def hlistEncoder[A, B, LabelledRepr <: HList](
      implicit lg: LabelledGeneric.Aux[A, LabelledRepr],
      e: Strict[Encoder[LabelledRepr, B]],
      lp: LowPriority): Encoder[A, B] =
    new Encoder[A, B] {
      override def encode(a: A): Result[B] = {
        e.value.encode(lg.to(a))
      }
    }

  def apply[A, B](implicit e: Encoder[A, B]): Encoder[A, B] = e
}
