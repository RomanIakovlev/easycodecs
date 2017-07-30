package net.iakovlev.dynamo.generic

import cats.implicits._
import shapeless.labelled.FieldType
import shapeless.{
  :+:,
  ::,
  CNil,
  Coproduct,
  HList,
  HNil,
  Inl,
  Inr,
  LabelledGeneric,
  Lazy,
  LowPriority,
  Typeable,
  Witness
}

import scala.language.higherKinds

trait SingleFieldOptionalEncoder[A] {
  def encode(a: A): Option[AttributeValue]
}

object SingleFieldOptionalEncoder {

  implicit def numericEncoder[A: Numeric] = new SingleFieldOptionalEncoder[A] {
    override def encode(a: A): Option[AttributeValue] = {
      Some(a).map {
        case i: Int        => AttributeValue(i)
        case l: Long       => AttributeValue(l)
        case f: Float      => AttributeValue(f)
        case d: Double     => AttributeValue(d)
        case b: BigDecimal => AttributeValue(b)
      }
    }
  }

  implicit val stringEncoder = new SingleFieldOptionalEncoder[String] {
    override def encode(a: String): Option[AttributeValue] =
      Some(AttributeValue(a))
  }

  implicit def mapAsMapEncoder[A](implicit e: SingleFieldOptionalEncoder[A]) =
    new SingleFieldOptionalEncoder[Map[String, A]] {
      override def encode(a: Map[String, A]): Option[AttributeValue] = {
        val res1: Option[Map[String, AttributeValue]] =
          a.mapValues(e.encode).sequence
        res1.map(AttributeValueMap.apply)
      }
    }

  implicit def mapAsClassEncoder[A](implicit e: MapEncoder[A]) =
    new SingleFieldOptionalEncoder[A] {
      override def encode(a: A): Option[AttributeValue] = {
        e.encode(a)
      }
    }

  implicit def coproductAsClassEncoder[A](
      implicit e: CoproductOptionalEncoder[A],
      lp: LowPriority) =
    new SingleFieldOptionalEncoder[A] {
      override def encode(a: A): Option[AttributeValue] = {
        e.encode(a)
      }
    }

  implicit def listEncoder[A](implicit e: SingleFieldOptionalEncoder[A]) =
    new SingleFieldOptionalEncoder[List[A]] {
      override def encode(a: List[A]): Option[AttributeValue] = {
        a.traverse(e.encode).map(AttributeValueList.apply)
      }
    }

  implicit def optionEncoder[A](implicit e: SingleFieldOptionalEncoder[A]) =
    new SingleFieldOptionalEncoder[Option[A]] {
      override def encode(a: Option[A]): Option[AttributeValue] =
        a.flatMap(e.encode)
    }

  implicit def encodeEnum[A, C <: Coproduct](
      implicit lg: LabelledGeneric.Aux[A, C],
      rie: IsEnum[C],
      es: SingleFieldOptionalEncoder[String]) =
    new SingleFieldOptionalEncoder[A] {
      override def encode(a: A): Option[AttributeValue] = {
        es.encode(rie.to(lg.to(a)))
      }
    }
}

trait OptionalEncoder[A] {
  def encode(a: A): Map[String, Option[AttributeValue]]
}

object OptionalEncoder {
  implicit def hNilEncoder[H <: HNil] = new OptionalEncoder[HNil] {
    override def encode(a: HNil): Map[String, Option[AttributeValue]] =
      Map.empty
  }
  implicit def hConsEncoder[K <: Symbol, H, T <: HList](
      implicit eh: SingleFieldOptionalEncoder[H],
      et: Lazy[OptionalEncoder[T]],
      k: Witness.Aux[K]) =
    new OptionalEncoder[FieldType[K, H] :: T] {
      override def encode(
          a: FieldType[K, H] :: T): Map[String, Option[AttributeValue]] = {
        println("encode hCons")
        et.value.encode(a.tail) + (k.value.name -> eh.encode(a.head))
      }
    }

  implicit def caseClassEncoder[A, R](implicit lg: LabelledGeneric.Aux[A, R],
                                      e: Lazy[OptionalEncoder[R]],
                                      t: Typeable[A]) = new OptionalEncoder[A] {
    override def encode(a: A): Map[String, Option[AttributeValue]] = {
      println("case class encode " + t.describe)
      e.value.encode(lg.to(a))
    }
  }

  private def flattenMapValues[A, B](m: Map[A, Option[B]]): Map[A, B] =
    for ((k, mv) <- m; v <- mv) yield k -> v

  def apply[A](a: A)(
      implicit e: OptionalEncoder[A]): Map[String, AttributeValue] =
    flattenMapValues(e.encode(a))
}

trait CoproductOptionalEncoder[A] {
  def encode(a: A): Option[AttributeValue]
}

object CoproductOptionalEncoder {
  implicit def cNilEncoder = new CoproductOptionalEncoder[CNil] {
    override def encode(a: CNil): Option[AttributeValue] =
      None
  }

  implicit def coproductEncoder[K <: Symbol, H, T <: Coproduct](
      implicit eh: SingleFieldOptionalEncoder[H],
      et: SingleFieldOptionalEncoder[T],
      k: Witness.Aux[K]) =
    new CoproductOptionalEncoder[FieldType[K, H] :+: T] {
      override def encode(a: FieldType[K, H] :+: T): Option[AttributeValue] = {
        println("encode coproduct")
        a match {
          case Inl(head) => eh.encode(head)
          case Inr(tail) => et.encode(tail)
        }
      }
    }

  implicit def genericEncoder[A, R <: Coproduct](
      implicit lg: LabelledGeneric.Aux[A, R],
      e: Lazy[CoproductOptionalEncoder[R]]) = new CoproductOptionalEncoder[A] {
    override def encode(a: A): Option[AttributeValue] = {
      println("coproduct generic encoder")
      e.value.encode(lg.to(a))
    }
  }
}

trait MapEncoder[A] {
  def encode(a: A): Option[AttributeValueMap]
}

object MapEncoder {

  implicit def hNilEncode[H <: HNil] = new MapEncoder[HNil] {
    override def encode(a: HNil): Option[AttributeValueMap] =
      Some(AttributeValueMap(Map.empty))
  }

  implicit def hConsEncode[K <: Symbol, H, T <: HList](
      implicit eh: SingleFieldOptionalEncoder[H],
      et: OptionalEncoder[T],
      k: Witness.Aux[K]) =
    new MapEncoder[FieldType[K, H] :: T] {
      override def encode(a: FieldType[K, H] :: T): Option[AttributeValueMap] = {
        println("encode map hCons")
        val res: Map[String, Option[AttributeValue]] =
          et.encode(a.tail) + (k.value.name -> eh.encode(a.head))
        val res1: Option[Map[String, AttributeValue]] = res.sequence
        res1.map(AttributeValueMap.apply)
      }
    }

  implicit def caseClassEncoder[A, R](implicit lg: LabelledGeneric.Aux[A, R],
                                      e: OptionalEncoder[R],
                                      t: Typeable[A]) = new MapEncoder[A] {
    override def encode(a: A): Option[AttributeValueMap] = {
      println("case class map encode " + t.describe)
      val res: Option[Map[String, AttributeValue]] =
        e.encode(lg.to(a)).sequence
      res.map(AttributeValueMap.apply)
    }
  }

}
