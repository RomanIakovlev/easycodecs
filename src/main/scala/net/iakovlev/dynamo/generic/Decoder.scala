package net.iakovlev.dynamo.generic

import cats.implicits._
import shapeless._
import shapeless.labelled.{FieldType, field}

import scala.language.higherKinds
import scala.util.Try
trait SingleFieldDecoder[A] {
  def decode(attributeValue: Option[AttributeValue]): Option[A]
}

object SingleFieldDecoder {
  implicit def optionDecoder[A](implicit d: SingleFieldDecoder[A]) =
    new SingleFieldDecoder[Option[A]] {
      override def decode(
          attributeValue: Option[AttributeValue]): Option[Option[A]] = {
        println("option decoder")
        if (attributeValue.isEmpty) Some(None)
        else d.decode(attributeValue).map(Some.apply)
      }
    }

  implicit def intDecoder = new SingleFieldDecoder[Int] {
    override def decode(attributeValue: Option[AttributeValue]): Option[Int] = {
      println("int decoder")
      attributeValue match {
        case Some(AttributeValueInt(value)) => Some(value)
        case Some(AttributeValueNumeric(value)) => Try(value.toInt).toOption
        case _ => None
      }
    }
  }

  implicit def longDecoder = new SingleFieldDecoder[Long] {
    override def decode(attributeValue: Option[AttributeValue]): Option[Long] = {
      println("long decoder")
      attributeValue match {
        case Some(AttributeValueLong(value)) => Some(value)
        case Some(AttributeValueNumeric(value)) => Try(value.toLong).toOption
        case _ => None
      }
    }
  }

  implicit def floatDecoder = new SingleFieldDecoder[Float] {
    override def decode(
        attributeValue: Option[AttributeValue]): Option[Float] = {
      println("float decoder")
      attributeValue match {
        case Some(AttributeValueFloat(value)) => Some(value)
        case Some(AttributeValueNumeric(value)) => Try(value.toFloat).toOption
        case _ => None
      }
    }
  }

  implicit def doubleDecoder = new SingleFieldDecoder[Double] {
    override def decode(
        attributeValue: Option[AttributeValue]): Option[Double] = {
      println("double decoder")
      attributeValue match {
        case Some(AttributeValueDouble(value)) => Some(value)
        case Some(AttributeValueNumeric(value)) => Try(value.toDouble).toOption
        case _ => None
      }
    }
  }

  implicit def bigDecimalDecoder = new SingleFieldDecoder[BigDecimal] {
    override def decode(
        attributeValue: Option[AttributeValue]): Option[BigDecimal] = {
      println("big decimal decoder")
      attributeValue match {
        case Some(AttributeValueBigDecimal(value)) => Some(value)
        case Some(AttributeValueNumeric(value)) =>
          Try(BigDecimal(value)).toOption
        case _ => None
      }
    }
  }
  implicit def stringDecoder = new SingleFieldDecoder[String] {
    override def decode(
        attributeValue: Option[AttributeValue]): Option[String] = {
      println("string decoder")
      attributeValue match {
        case Some(AttributeValueString(value)) => Some(value)
        case _ => None
      }
    }
  }
  implicit def mapAsMapDecoder[A](implicit d: SingleFieldDecoder[A],
                                  t: Typeable[A]) =
    new SingleFieldDecoder[Map[String, A]] {
      override def decode(
          attributeValue: Option[AttributeValue]): Option[Map[String, A]] = {
        attributeValue match {
          case Some(AttributeValueMap(value)) =>
            value.mapValues(Some.apply).mapValues(d.decode).sequence
          case _ => None
        }
      }
    }
  implicit def mapAsClassDecoder[A](implicit dm: Decoder[A],
                                    t: Typeable[A]) =
    new SingleFieldDecoder[A] {
      override def decode(attributeValue: Option[AttributeValue]): Option[A] = {
        println("map decoder " + t.describe)
        attributeValue match {
          case Some(AttributeValueMap(value)) =>
            dm.decode(value)
          case _ => None
        }
      }
    }
  implicit def listDecoder[A](implicit d: SingleFieldDecoder[A],
                              t: Typeable[A]) =
    new SingleFieldDecoder[List[A]] {
      override def decode(
          attributeValue: Option[AttributeValue]): Option[List[A]] = {
        println("list decoder " + t.describe)
        attributeValue match {
          case Some(AttributeValueList(value)) =>
            value.toList.map(Some.apply).traverse(d.decode)
          case _ => None
        }
      }
    }
  implicit def coproductAsClassDecoder[A](
      implicit d: Lazy[CoproductDecoder[A]],
      lp: LowPriority) =
    new SingleFieldDecoder[A] {
      override def decode(attributeValue: Option[AttributeValue]): Option[A] = {
        d.value.decode(attributeValue)
      }
    }

  implicit def decodeEnum[A, C <: Coproduct](
      implicit gen: LabelledGeneric.Aux[A, C],
      ds: SingleFieldDecoder[String],
      rie: IsEnum[C]) = new SingleFieldDecoder[A] {
    override def decode(attributeValue: Option[AttributeValue]): Option[A] = {
      ds.decode(attributeValue).flatMap(s => rie.from(s).map(gen.from))
    }
  }
}

trait Decoder[A] {
  def decode(attributes: Map[String, AttributeValue]): Option[A]
}

object Decoder {

  implicit def hNilDecoder = new Decoder[HNil] {
    override def decode(
        attributes: Map[String, AttributeValue]): Option[HNil] = Some(HNil)
  }
  implicit def hConsDecoder[K <: Symbol, H, T <: HList](
      implicit k: Witness.Aux[K],
      d: SingleFieldDecoder[H],
      dt: Decoder[T],
      t: Typeable[H]) = new Decoder[FieldType[K, H] :: T] {
    override def decode(attributes: Map[String, AttributeValue])
      : Option[FieldType[K, H] :: T] = {
      println("hcons decoder " + t.describe)
      val attr = attributes.get(k.value.name)
      for {
        h <- d.decode(attr)
        t <- dt.decode(attributes)
      } yield field[K](h) :: t
    }
  }

  implicit def caseClassDecoder[A, R](implicit lg: LabelledGeneric.Aux[A, R],
                                      dr: Decoder[R],
                                      t: Typeable[A]) = new Decoder[A] {
    override def decode(attributes: Map[String, AttributeValue]): Option[A] = {
      println("case class decoder " + t.describe)
      dr.decode(attributes).map(lg.from)
    }
  }

  def apply[A](attributes: Map[String, AttributeValue])(
      implicit da: Decoder[A]): Option[A] = {
    da.decode(attributes)
  }
}

trait CoproductDecoder[A] {
  def decode(a: Option[AttributeValue]): Option[A]
}

object CoproductDecoder {
  implicit val cNilDecoder = new CoproductDecoder[CNil] {
    override def decode(a: Option[AttributeValue]): Option[CNil] = None
  }

  implicit def coproductDecoder[K <: Symbol, H, T <: Coproduct](
      implicit dh: SingleFieldDecoder[H],
      dt: SingleFieldDecoder[T]) =
    new CoproductDecoder[FieldType[K, H] :+: T] {
      override def decode(
          a: Option[AttributeValue]): Option[FieldType[K, H] :+: T] = {
        dh.decode(a)
          .map(aa => Inl(field[K](aa)))
          .orElse(dt.decode(a).map(Inr(_)))
      }
    }
  implicit def genericDecoder[A, R <: Coproduct](
      implicit lg: LabelledGeneric.Aux[A, R],
      d: CoproductDecoder[R]) = new CoproductDecoder[A] {
    override def decode(a: Option[AttributeValue]): Option[A] = {
      d.decode(a).map(lg.from)
    }
  }
}