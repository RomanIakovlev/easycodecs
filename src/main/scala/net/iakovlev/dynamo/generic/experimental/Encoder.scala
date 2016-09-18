package net.iakovlev.dynamo.generic.experimental

import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Typeable, Witness}

trait SingleFieldEncoder[A] {
  def encode(a: A): AttributeValue
}

object SingleFieldEncoder {
  implicit val intEncoder = new SingleFieldEncoder[Int] {
    override def encode(a: Int): AttributeValue = AttributeValue(a)
  }
  implicit val stringEncoder = new SingleFieldEncoder[String] {
    override def encode(a: String): AttributeValue = AttributeValue(a)
  }
  implicit def mapAsMapEncoder[A](implicit e: SingleFieldEncoder[A]) =
    new SingleFieldEncoder[Map[String, A]] {
      override def encode(a: Map[String, A]): AttributeValue = {
        AttributeValueMap(a.mapValues(e.encode))
      }
    }
  implicit def mapAsClassEncoder[A](implicit e: MapEncoder[A]) = new SingleFieldEncoder[A] {
    override def encode(a: A): AttributeValue = {
      e.encode(a)
    }
  }
  implicit def listEncoder[A](implicit e: SingleFieldEncoder[A]) = new SingleFieldEncoder[List[A]] {
    override def encode(a: List[A]): AttributeValue = {
      AttributeValueList(a.map(e.encode))
    }
  }
}

trait Encoder[A] {
  def encode(a: A): Map[String, AttributeValue]
}

object Encoder {
  implicit def hNilEncode[H <: HNil] = new Encoder[HNil] {
    override def encode(a: HNil): Map[String, AttributeValue] = Map.empty
  }
  implicit def hConsEncode[K <: Symbol, H, T <: HList](
      implicit eh: SingleFieldEncoder[H],
      et: Encoder[T],
      k: Witness.Aux[K]) =
    new Encoder[FieldType[K, H] :: T] {
      override def encode(
          a: FieldType[K, H] :: T): Map[String, AttributeValue] = {
        println("encode hcons")
        et.encode(a.tail) + (k.value.name -> eh.encode(a.head))
      }
    }

  implicit def caseClassEncoder[A, R](implicit lg: LabelledGeneric.Aux[A, R],
                                      e: Encoder[R],
                                      t: Typeable[A]) = new Encoder[A] {
    override def encode(a: A): Map[String, AttributeValue] = {
      println("case class encode " + t.describe)
      e.encode(lg.to(a))
    }
  }

  def apply[A](a: A)(implicit e: Encoder[A]) = e.encode(a)
}

trait MapEncoder[A] {
  def encode(a: A): AttributeValueMap
}

object MapEncoder {
  implicit def hNilEncode[H <: HNil] = new MapEncoder[HNil] {
    override def encode(a: HNil): AttributeValueMap = AttributeValueMap(Map.empty)
  }
  implicit def hConsEncode[K <: Symbol, H, T <: HList](
                                                        implicit eh: SingleFieldEncoder[H],
                                                        et: Encoder[T],
                                                        k: Witness.Aux[K]) =
    new MapEncoder[FieldType[K, H] :: T] {
      override def encode(
                           a: FieldType[K, H] :: T): AttributeValueMap = {
        println("encode map hcons")
        AttributeValueMap(et.encode(a.tail) + (k.value.name -> eh.encode(a.head)))
      }
    }

  implicit def caseClassEncoder[A, R](implicit lg: LabelledGeneric.Aux[A, R],
                                      e: Encoder[R],
                                      t: Typeable[A]) = new MapEncoder[A] {
    override def encode(a: A): AttributeValueMap = {
      println("case class map encode " + t.describe)
      AttributeValueMap(e.encode(lg.to(a)))
    }
  }

}