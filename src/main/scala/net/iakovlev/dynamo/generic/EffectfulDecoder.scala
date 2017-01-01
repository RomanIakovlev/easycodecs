package net.iakovlev.dynamo.generic
import cats.implicits._
import cats.{ApplicativeError, Monad}
import shapeless._
import shapeless.labelled.{FieldType, field}

import scala.language.higherKinds
import scala.util.{Failure, Try}

trait SingleFieldEffectfulDecoder[F[_], A, B] {
  def decode(a: F[A]): F[B]
}

object SingleFieldEffectfulDecoder {
  /*implicit def mapAsClassAwsDecoder[F[_], S, A](
      implicit d: Lazy[EffectfulDecoder[F, S, A]],
      ev: Lazy[(S) => Map[String, S]]): SingleFieldEffectfulDecoder[F, S, A] =
    new SingleFieldEffectfulDecoder[F, S, A] {
      override def decode(a: S): F[A] = {
        d.value.decode(ev.value(a))
      }
    }*/
  implicit def coproductAsClassDecoder[F[_], S, A](
      implicit d: Lazy[CoproductEffectfulDecoder[F, S, A]],
      lp: LowPriority) =
    new SingleFieldEffectfulDecoder[F, S, A] {
      override def decode(a: F[S]): F[A] =
        d.value.decode(a)
    }
}

trait EffectfulDecoder[F[_], A, B] {
  def decode(a: Map[String, A]): F[B]
}

object EffectfulDecoder {

  implicit def hNilDecoder[F[_], A](implicit f: Monad[F]) =
    new EffectfulDecoder[F, A, HNil] {
      override def decode(a: Map[String, A]): F[HNil] = f.pure(HNil)
    }

  implicit def hConsDecoder[F[_],
                            E >: Throwable,
                            A,
                            K <: Symbol,
                            H,
                            T <: HList](
      implicit f: ApplicativeError[F, E],
      k: Witness.Aux[K],
      d: SingleFieldEffectfulDecoder[F, A, H],
      dt: EffectfulDecoder[F, A, T],
      t: Typeable[H]) =
    new EffectfulDecoder[F, A, FieldType[K, H] :: T] {
      override def decode(
          attributes: Map[String, A]): F[FieldType[K, H] :: T] = {
        println("hcons decoder " + t.describe)
        val a = f.catchNonFatal(attributes(k.value.name))
        (d.decode(a) |@| dt.decode(attributes)).map { field[K](_) :: _ }
      }
    }

  implicit def caseClassDecoder[F[_], A, B, E, R](
      implicit f: ApplicativeError[F, E],
      lg: LabelledGeneric.Aux[B, R],
      dr: EffectfulDecoder[F, A, R],
      t: Typeable[B]) =
    new EffectfulDecoder[F, A, B] {
      override def decode(attributes: Map[String, A]): F[B] = {
        println("case class decoder " + t.describe)
        dr.decode(attributes).map(lg.from)
      }
    }

  def apply[A, B](attributes: Map[String, A])(
      implicit da: EffectfulDecoder[Try, A, B]): Try[B] = {
    da.decode(attributes)
  }
}

trait CoproductEffectfulDecoder[F[_], A, B] {
  def decode(a: F[A]): F[B]
}

object CoproductEffectfulDecoder {
  implicit def cNilDecoder[F[_], A, E >: Throwable, B](
      implicit f: ApplicativeError[F, E])
    : CoproductEffectfulDecoder[F, A, CNil] =
    new CoproductEffectfulDecoder[F, A, CNil] {
      override def decode(a: F[A]): F[CNil] =
        f.fromTry[CNil](Failure(new Exception))
    }

  implicit def coproductDecoder[F[_], A, E, K <: Symbol, H, T <: Coproduct](
      implicit f: ApplicativeError[F, E],
      dh: SingleFieldEffectfulDecoder[F, A, H],
      dt: SingleFieldEffectfulDecoder[F, A, T]) =
    new CoproductEffectfulDecoder[F, A, FieldType[K, H] :+: T] {
      override def decode(a: F[A]): F[FieldType[K, H] :+: T] = {
        val r = dh
          .decode(a)
          .map(aa => Inl(field[K](aa)): FieldType[K, H] :+: T)

        r.recoverWith {
          case _ => dt.decode(a).map(Inr(_))
        }
      }
    }
  implicit def genericDecoder[F[_], A, B, E >: Throwable, R <: Coproduct](
      implicit f: ApplicativeError[F, E],
      lg: LabelledGeneric.Aux[B, R],
      d: CoproductEffectfulDecoder[F, A, R]) =
    new CoproductEffectfulDecoder[F, A, B] {
      override def decode(a: F[A]): F[B] = {
        d.decode(a).map(lg.from)
      }
    }
}
