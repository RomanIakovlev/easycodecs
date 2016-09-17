package net.iakovlev.dynamo.generic.experimental

import shapeless._
import shapeless.labelled.{FieldType, field}
import cats.implicits._

import scala.language.higherKinds

sealed trait AttributeValue
sealed trait AttributeValueScalar extends AttributeValue
sealed trait AttributeValueDocument extends AttributeValue
sealed trait AttributeValueSet extends AttributeValue

case class AttributeValueNumeric(value: Int) extends AttributeValueScalar
case class AttributeValueString(value: String) extends AttributeValueScalar
case class AttributeValueMap(value: Map[String, AttributeValue])
    extends AttributeValueDocument
case class AttributeValueList(value: List[AttributeValue]) extends AttributeValueDocument

object AttributeValue {
  def apply(i: Int) = AttributeValueNumeric(i)
  def apply(s: String) = AttributeValueString(s)
}

trait SingleFieldDecoder[A] {
  def decode(attributeValue: Option[AttributeValue]): Option[A]
}

object SingleFieldDecoder {
  implicit def optionDecoder[A](implicit d: SingleFieldDecoder[A]) = new SingleFieldDecoder[Option[A]] {
    override def decode(attributeValue: Option[AttributeValue]): Option[Option[A]] = {
      println("option decoder")
      Some(d.decode(attributeValue))
    }
  }

  implicit def intDecoder = new SingleFieldDecoder[Int] {
    override def decode(attributeValue: Option[AttributeValue]): Option[Int] = {
      println("int decoder")
      attributeValue match {
        case Some(AttributeValueNumeric(value)) => Some(value)
        case _ => None
      }
    }
  }
  implicit def stringDecoder = new SingleFieldDecoder[String] {
    override def decode(attributeValue: Option[AttributeValue]): Option[String] = {
      println("string decoder")
      attributeValue match {
        case Some(AttributeValueString(value)) => Some(value)
        case _ => None
      }
    }
  }
  implicit def mapDecoder[A](implicit dm: MapDecoder[A], t: Typeable[A]) =
    new SingleFieldDecoder[A] {
      override def decode(attributeValue: Option[AttributeValue])
        : Option[A] = {
        println("map decoder " + t.describe)
        attributeValue match {
          case Some(AttributeValueMap(value)) => dm.decode(AttributeValueMap(value))
          case _ => None
        }
      }
    }
  implicit def listDecoder[A](implicit d: SingleFieldDecoder[A], t: Typeable[A]) =
    new SingleFieldDecoder[List[A]] {
      override def decode(attributeValue: Option[AttributeValue])
        : Option[List[A]] = {
        println("list decoder " + t.describe)
        attributeValue match {
          case Some(AttributeValueList(value)) => value.map(Some.apply).traverse(d.decode)
          case _ => None
        }
      }
    }
}

trait Decoder[A] {
  def decode(attributes: Map[String, AttributeValue]): Option[A]
}

object Decoder {
  implicit def hNilDecoder = new Decoder[HNil] {
    override def decode(attributes: Map[String, AttributeValue]): Option[HNil] = Some(HNil)
  }
  implicit def hConsDecoder[K <: Symbol, H, T <: HList](
      implicit k: Witness.Aux[K],
      d: SingleFieldDecoder[H],
      dt: Decoder[T],
      t: Typeable[H]) = new Decoder[FieldType[K, H] :: T] {
    override def decode(
        attributes: Map[String, AttributeValue]): Option[FieldType[K, H] :: T] = {
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

  def apply[A](attributes: Map[String, AttributeValue])(implicit da: Decoder[A]): Option[A] = {
    da.decode(attributes)
  }
}

trait MapDecoder[A] {
  def decode(map: AttributeValueMap): Option[A]
}

object MapDecoder {
  implicit def hNilMapDecoder = new MapDecoder[HNil] {
    override def decode(map: AttributeValueMap): Option[HNil] =
      Some(HNil)
  }

  implicit def hConsMapDecoder[K <: Symbol, H, T <: HList](
      implicit k: Witness.Aux[K],
      d: SingleFieldDecoder[H],
      td: MapDecoder[T],
      t: Typeable[H]) = new MapDecoder[FieldType[K, H] :: T] {
    override def decode(map: AttributeValueMap): Option[FieldType[K, H] :: T] = {
      println("hcons map decoder " + t.describe)
      val attrValue = map.value.get(k.value.name)
      for {
        decoded <- d.decode(attrValue)
        tail <- td.decode(map)
      } yield field[K](decoded) :: tail
    }
  }

  implicit def caseClassDecoder[A, R](implicit lg: LabelledGeneric.Aux[A, R],
                                      dr: MapDecoder[R],
                                      t: Typeable[A]) = new MapDecoder[A] {
    override def decode(map: AttributeValueMap): Option[A] = {
      println("case class map decoder " + t.describe)
      dr.decode(map).map(lg.from)
    }
  }
}
