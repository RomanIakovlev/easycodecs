package net.iakovlev.dynamo.generic

import cats._
import cats.implicits._
import shapeless._
import shapeless.labelled._

import scala.language.higherKinds

trait PrimitivesIntaker[Member, Host] {
  def intake(m: Member, h: Host): Host
}

trait SingleFieldEffectfulEncoder[F[_], A, B] {
  def encode(a: A): F[B]
}

object SingleFieldEffectfulEncoder {
  implicit def primivitesEncoder[R[_], A, B](
      implicit primitivesIntaker: PrimitivesIntaker[A, B],
      R: MonadReader[R, B])
    : SingleFieldEffectfulEncoder[R, A, B] =
    new SingleFieldEffectfulEncoder[R, A, B] {
      override def encode(a: A): R[B] = {
        R.ask.map(b => primitivesIntaker.intake(a, b))
      }
    }
}

trait EffectfulEncoder[F[_], A, B] {
  def encode(a: A): F[Map[String, B]]
}

object EffectfulEncoder {

  implicit def encodeHNil[F[_], A](
      implicit f: Applicative[F]): EffectfulEncoder[F, HNil, A] =
    new EffectfulEncoder[F, HNil, A] {
      override def encode(a: HNil): F[Map[String, A]] = f.pure(Map.empty)
    }
  implicit def encodeHCons[F[_]: Applicative, A, K <: Symbol, H, T <: HList](
      implicit eh: SingleFieldEffectfulEncoder[F, FieldType[K, H], A],
      et: EffectfulEncoder[F, T, A],
      k: Witness.Aux[K]): EffectfulEncoder[F, FieldType[K, H] :: T, A] =
    new EffectfulEncoder[F, FieldType[K, H] :: T, A] {
      override def encode(a: FieldType[K, H] :: T): F[Map[String, A]] = {
        (et.encode(a.tail) |@| eh.encode(a.head)).map { (t, h) =>
          t + (k.value.name -> h)
        }
      }
    }
  implicit def encodeGeneric[F[_], A, LabelledRepr <: HList, B](
      implicit lg: LabelledGeneric.Aux[A, LabelledRepr],
      e: EffectfulEncoder[F, LabelledRepr, B]): EffectfulEncoder[F, A, B] =
    new EffectfulEncoder[F, A, B] {
      override def encode(a: A): F[Map[String, B]] = {
        e.encode(lg.to(a))
      }
    }
}
