package net.iakovlev.dynamo.generic
import cats.implicits._
import cats.{ApplicativeError, Monad, MonadError, Traverse}
import shapeless._
import shapeless.labelled.{FieldType, field}

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import scala.util.Failure

trait PrimitivesExtractor[F[_], A, B] {
  def extract(a: A): F[B]
}

trait SingleFieldEffectfulDecoder[F[_], A, B] {
  def decode(a: F[A]): F[B]
}

object SingleFieldEffectfulDecoder {
  implicit def mapAsClassDecoder[F[_]: Monad, S, A](
      implicit d: EffectfulDecoder[F, S, A],
      ext: PrimitivesExtractor[F, S, Map[String, S]])
    : SingleFieldEffectfulDecoder[F, S, A] =
    new SingleFieldEffectfulDecoder[F, S, A] {
      override def decode(a: F[S]): F[A] = {
        for {
          b <- a
          c <- ext.extract(b)
          d <- d.decode(c)
        } yield d
      }
    }
  implicit def mapAsMapDecoder[F[_], S, A](
      implicit f: Monad[F],
      d: SingleFieldEffectfulDecoder[F, S, A],
      ext: PrimitivesExtractor[F, S, Map[String, S]])
    : SingleFieldEffectfulDecoder[F, S, Map[String, F[A]]] =
    new SingleFieldEffectfulDecoder[F, S, Map[String, F[A]]] {
      override def decode(a: F[S]): F[Map[String, F[A]]] = {
        for {
          b <- a
          c <- ext.extract(b)
        } yield c.mapValues(value => d.decode(f.pure(value)))
      }
    }
  implicit def traversableExtractorDecoder[F[_]: Monad,
                                           S,
                                           A,
                                           C[X] <: Seq[X]: Traverse](
      implicit cbf: CanBuildFrom[C[A], A, C[A]],
      ad: SingleFieldEffectfulDecoder[F, S, A],
      ext: PrimitivesExtractor[F, S, C[F[S]]])
    : SingleFieldEffectfulDecoder[F, S, C[A]] =
    new SingleFieldEffectfulDecoder[F, S, C[A]] {
      override def decode(a: F[S]): F[C[A]] = {
        val builder = cbf()
        for {
          s <- a
          cfs <- ext.extract(s)
          r <- cfs.traverse(ad.decode)
        } yield {
          r.foreach(builder += _)
          builder.result()
        }
      }
    }

  implicit def extractorDecoder[F[_], S, A](implicit f: Monad[F],
                                            ext: PrimitivesExtractor[F, S, A])
    : SingleFieldEffectfulDecoder[F, S, A] =
    new SingleFieldEffectfulDecoder[F, S, A] {
      override def decode(a: F[S]): F[A] =
        a.flatMap(aa => ext.extract(aa))
    }
  implicit def optionalDecoder[F[_], S, E >: Throwable, A](
      implicit f: ApplicativeError[F, E],
      d: SingleFieldEffectfulDecoder[F, S, A])
    : SingleFieldEffectfulDecoder[F, S, Option[A]] =
    new SingleFieldEffectfulDecoder[F, S, Option[A]] {
      override def decode(a: F[S]): F[Option[A]] = {
        d.decode(a).map(Option.apply).recoverWith {
          case _: NoSuchElementException =>
            f.pure(None)
          case e =>
            f.raiseError(e)
        }
      }
    }
  implicit def coproductAsClassDecoder[F[_], S, A](
      implicit d: Lazy[CoproductEffectfulDecoder[F, S, A]],
      lp: LowPriority) =
    new SingleFieldEffectfulDecoder[F, S, A] {
      override def decode(a: F[S]): F[A] =
        d.value.decode(a)
    }
  implicit def decodeEnum[F[_], S, E >: Throwable, A, C <: Coproduct](
      implicit f: MonadError[F, E],
      gen: LabelledGeneric.Aux[A, C],
      ds: SingleFieldEffectfulDecoder[F, S, String],
      rie: IsEnum[C]) =
    new SingleFieldEffectfulDecoder[F, S, A] {
      override def decode(a: F[S]): F[A] = {
        ds.decode(a)
          .flatMap(
            s =>
              rie
                .from(s)
                .map(gen.from)
                .map(v => f.pure(v))
                .getOrElse(f.raiseError(new NoSuchElementException)))
      }
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
      dt: EffectfulDecoder[F, A, T]) =
    new EffectfulDecoder[F, A, FieldType[K, H] :: T] {
      override def decode(
          attributes: Map[String, A]): F[FieldType[K, H] :: T] = {
        val a = f.catchNonFatal(attributes(k.value.name))
        (d.decode(a) |@| dt.decode(attributes)).map { field[K](_) :: _ }
      }
    }

  // LowPriority to allow for companion object-defined instances to take priority
  implicit def caseClassDecoder[F[_], A, B, E, R](
      implicit f: ApplicativeError[F, E],
      lg: LabelledGeneric.Aux[B, R],
      dr: Lazy[EffectfulDecoder[F, A, R]],
      lp: LowPriority) =
    new EffectfulDecoder[F, A, B] {
      override def decode(attributes: Map[String, A]): F[B] = {
        dr.value.decode(attributes).map(lg.from)
      }
    }

  def apply[F[_], A, B](attributes: Map[String, A])(
      implicit da: EffectfulDecoder[F, A, B]): F[B] = {
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
        f.fromTry[CNil](Failure(new Exception("CNil decoder!")))
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
