package net.iakovlev.easycodecs.encoder

import cats.instances.either._
import cats.syntax.cartesian._
import cats.syntax.either._
import net.iakovlev.easycodecs._
import shapeless._
import shapeless.labelled.FieldType

import scala.collection.generic.CanBuildFrom
import scala.language.{existentials, higherKinds}
import scala.util.control.NoStackTrace
import scala.util.{Left, Right}

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
  implicit def encodeEnum[A, ARepr <: Coproduct, B](
      implicit e: PrimitivesWriter[String, B],
      lg: LabelledGeneric.Aux[A, ARepr],
      isEnum: IsEnum[ARepr]): SingleFieldEncoder[A, B] =
    instance { a =>
      Right(e.write(isEnum.to(lg.to(a))))
    }
  implicit def traversableWriterEncoder[A, B, C[X] <: Traversable[X]](
      implicit cbf: CanBuildFrom[C[B], B, C[B]],
      ae: SingleFieldEncoder[A, B],
      w: PrimitivesWriter[C[B], B]): SingleFieldEncoder[C[A], B] =
    new SingleFieldEncoder[C[A], B] {
      override def encode(a: C[A]): Either[EncodingError, B] = {
        val c = cbf()
        a.foldLeft(None: Option[EncodingError]) { (_, elem) =>
          ae.encode(elem) match {
            case Left(e) =>
              Some(e)
            case Right(r) =>
              c += r
              None
          }
        } match {
          case Some(x) => Left(x)
          case None    => Right(w.write(c.result()))
        }
      }
    }
  implicit def optionEncoder[A, B](
      implicit e: SingleFieldEncoder[A, B]): SingleFieldEncoder[Option[A], B] =
    instance {
      case Some(x) => e.encode(x)
      case None    => Left(new OtherEncodingError)
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
  implicit def mapAsMapEncoder[A, B](
      implicit e: SingleFieldEncoder[A, B],
      w: PrimitivesWriter[Map[String, B], B]
  ): SingleFieldEncoder[Map[String, A], B] =
    instance { a =>
      import cats.instances.map._
      import cats.syntax.traverse._
      val o: Either[EncodingError, B] = for {
        r <- a.traverseU(e.encode)
      } yield w.write(r)
      o
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

  def instance[A, B](f: A => Result[B]): Encoder[A, B] =
    new Encoder[A, B] {
      override def encode(a: A): Either[EncodingError, Map[String, B]] = f(a)
    }

  implicit def encodeHNil[A]: Encoder[HNil, A] =
    instance { _ =>
      Right(Map.empty)
    }

  // "H =:= Option[X] forSome { type X }" and "LowPriority" in Encoder#encodeHCons seems to be
  // the only way to guide implicit solver correctly :(
  implicit def encodeHConsOpt[A, K <: Symbol, H, T <: HList](
      implicit eh: SingleFieldEncoder[H, A],
      et: Lazy[Encoder[T, A]],
      ev: H =:= Option[X] forSome { type X },
      k: Witness.Aux[K]): Encoder[FieldType[K, H] :: T, A] =
    instance { a =>
      ev(a.head) match {
        case Some(_) =>
          (et.value.encode(a.tail) |@| eh.encode(a.head)).map { (t, h) =>
            t + (k.value.name -> h)
          }
        case None =>
          et.value.encode(a.tail)
      }
    }
  // LowPriority to allow for Encoder#encodeHConsOpt to take over when H is Option
  implicit def encodeHCons[A, K <: Symbol, H, T <: HList](
      implicit eh: SingleFieldEncoder[H, A],
      et: Lazy[Encoder[T, A]],
      k: Witness.Aux[K],
      lp: LowPriority): Encoder[FieldType[K, H] :: T, A] =
    instance { a =>
      (et.value.encode(a.tail) |@| eh.encode(a.head)).map { (t, h) =>
        t + (k.value.name -> h)
      }
    }
  // LowPriority and Strict to allow for SingleFieldEncoder#classAsMapEncoder to take over
  implicit def hlistEncoder[A, B, LabelledRepr <: HList](
      implicit lg: LabelledGeneric.Aux[A, LabelledRepr],
      e: Strict[Encoder[LabelledRepr, B]],
      lp: LowPriority): Encoder[A, B] =
    instance { a =>
      e.value.encode(lg.to(a))
    }

  def apply[A, B](implicit e: Encoder[A, B]): Encoder[A, B] = e
}
