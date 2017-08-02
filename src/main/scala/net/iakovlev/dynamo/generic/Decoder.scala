package net.iakovlev.dynamo.generic
import cats.implicits._
import shapeless._
import shapeless.labelled.{FieldType, field}

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.{Left, Right}

abstract sealed class DecodingError extends Throwable {
  override def fillInStackTrace(): Throwable = this
}

class MissingFieldError extends DecodingError
class ExtractionError(val t: Throwable) extends DecodingError
class OtherError extends DecodingError

import Decoder.Result

trait PrimitivesReader[A, B] {
  def extract(a: A): Either[DecodingError, B]
}

trait SingleFieldEffectfulDecoder[S, A] {
  def decode(a: Either[DecodingError, S]): Either[DecodingError, A]
}

object SingleFieldEffectfulDecoder {

  def instance[S, A](f: Either[DecodingError, S] => Result[A])
    : SingleFieldEffectfulDecoder[S, A] =
    new SingleFieldEffectfulDecoder[S, A] {
      override def decode(a: Either[DecodingError, S]): Result[A] = {
        f(a)
      }
    }

  implicit def mapAsClassDecoder[S, A](
      implicit d: Decoder[S, A],
      ext: PrimitivesReader[S, Map[String, S]])
    : SingleFieldEffectfulDecoder[S, A] =
    instance { a =>
      for {
        b <- a
        c <- ext.extract(b)
        d <- d.decode(c)
      } yield d
    }

  implicit def mapAsMapDecoder[S, A](
      implicit d: SingleFieldEffectfulDecoder[S, A],
      ext: PrimitivesReader[S, Map[String, S]])
    : SingleFieldEffectfulDecoder[S, Map[String, A]] =
    instance { a =>
      val z: Either[DecodingError, Either[DecodingError, Map[String, A]]] =
        for {
          b <- a
          c <- ext.extract(b)
        } yield {
          c.traverseU { v =>
            d.decode(Right(v))
          }
        }
      z.flatten
    }

  implicit def traversableExtractorDecoder[S, A, C[X] <: Traversable[X]](
      implicit cbf: CanBuildFrom[C[A], A, C[A]],
      ad: SingleFieldEffectfulDecoder[S, A],
      ext: PrimitivesReader[S, C[S]]): SingleFieldEffectfulDecoder[S, C[A]] =
    new SingleFieldEffectfulDecoder[S, C[A]] {
      override def decode(
          a: Either[DecodingError, S]): Either[DecodingError, C[A]] = {
        (for {
          s <- a
          cs <- ext.extract(s)
        } yield {
          val builder = cbf()
          cs.foldLeft(None: Option[DecodingError]) { (_, a) =>
            ad.decode(Right(a)) match {
              case Left(e) => Some(e)
              case Right(r) =>
                builder += r
                None
            }
          } match {
            case Some(e) => Left(e)
            case None    => Right(builder.result())
          }
        }).flatten
      }
    }

  implicit def extractorDecoder[S, A](implicit ext: PrimitivesReader[S, A])
    : SingleFieldEffectfulDecoder[S, A] =
    new SingleFieldEffectfulDecoder[S, A] {
      override def decode(
          a: Either[DecodingError, S]): Either[DecodingError, A] =
        a.flatMap(aa => ext.extract(aa))
    }
  implicit def optionalDecoder[S, A](
      implicit d: SingleFieldEffectfulDecoder[S, A])
    : SingleFieldEffectfulDecoder[S, Option[A]] =
    new SingleFieldEffectfulDecoder[S, Option[A]] {
      override def decode(
          a: Either[DecodingError, S]): Either[DecodingError, Option[A]] = {
        d.decode(a).map(Option.apply).recoverWith {
          case _: MissingFieldError =>
            Right(None)
          case e =>
            Left(e)
        }
      }
    }
  implicit def coproductAsClassDecoder[S, A](
      implicit d: Strict[CoproductEffectfulDecoder[S, A]],
      lp: LowPriority) =
    new SingleFieldEffectfulDecoder[S, A] {
      override def decode(
          a: Either[DecodingError, S]): Either[DecodingError, A] =
        d.value.decode(a)
    }
  implicit def decodeEnum[S, A, C <: Coproduct](
      implicit gen: LabelledGeneric.Aux[A, C],
      ds: SingleFieldEffectfulDecoder[S, String],
      rie: IsEnum[C]) =
    new SingleFieldEffectfulDecoder[S, A] {
      override def decode(
          a: Either[DecodingError, S]): Either[DecodingError, A] = {
        ds.decode(a)
          .flatMap(
            s =>
              rie
                .from(s)
                .map(gen.from)
                .map(v => Right(v))
                .getOrElse(Left(new MissingFieldError)))
      }
    }
}

trait Decoder[A, B] {
  def decode(a: Map[String, A]): Either[DecodingError, B]
}

object Decoder {

  type Result[A] = Either[DecodingError, A]

  def instance[S, A](
      f: Map[String, S] => Either[DecodingError, A]): Decoder[S, A] =
    new Decoder[S, A] {
      override def decode(a: Map[String, S]): Either[DecodingError, A] = f(a)
    }
  implicit def hNilDecoder[A]: Decoder[A, HNil] =
    instance(_ => Right(HNil))

  implicit def hConsDecoder[A, K <: Symbol, H, T <: HList](
      implicit k: Witness.Aux[K],
      d: SingleFieldEffectfulDecoder[A, H],
      dt: Decoder[A, T]): Decoder[A, FieldType[K, H] :: T] =
    instance { attributes =>
      val x =
        Either.fromOption(attributes.get(k.value.name), new MissingFieldError)
      (d.decode(x) |@| dt.decode(attributes)).map { field[K](_) :: _ }
    }

  // LowPriority to allow the companion object-defined instances to take priority
  implicit def caseClassDecoder[A, B, R](implicit lg: LabelledGeneric.Aux[B, R],
                                         dr: Strict[Decoder[A, R]],
                                         lp: LowPriority): Decoder[A, B] =
    instance { attributes =>
      dr.value.decode(attributes).map(lg.from)
    }

  def apply[A, B](implicit da: Decoder[A, B]): Decoder[A, B] = da
}

trait CoproductEffectfulDecoder[A, B] {
  def decode(a: Either[DecodingError, A]): Either[DecodingError, B]
}

object CoproductEffectfulDecoder {
  implicit def cNilDecoder[A, B]: CoproductEffectfulDecoder[A, CNil] =
    new CoproductEffectfulDecoder[A, CNil] {
      override def decode(
          a: Either[DecodingError, A]): Either[DecodingError, CNil] =
        Left(new OtherError)
    }

  implicit def coproductDecoder[A, K <: Symbol, H, T <: Coproduct](
      implicit dh: Lazy[SingleFieldEffectfulDecoder[A, H]],
      dt: SingleFieldEffectfulDecoder[A, T]) =
    new CoproductEffectfulDecoder[A, FieldType[K, H] :+: T] {
      override def decode(a: Either[DecodingError, A])
        : Either[DecodingError, FieldType[K, H] :+: T] = {
        dh.value
          .decode(a)
          .map(aa => Inl(field[K](aa)): FieldType[K, H] :+: T)
          .recoverWith {
            case _ =>
              val t: Either[DecodingError, T] = dt.decode(a)
              t.map(Inr(_))
          }
      }
    }
  implicit def genericDecoder[A, B, R <: Coproduct](
      implicit lg: LabelledGeneric.Aux[B, R],
      d: CoproductEffectfulDecoder[A, R]) =
    new CoproductEffectfulDecoder[A, B] {
      override def decode(
          a: Either[DecodingError, A]): Either[DecodingError, B] = {
        d.decode(a).map(lg.from)
      }
    }
}
