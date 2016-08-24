package net.iakovlev.dynamo.generic.named

import awscala.dynamodbv2.{Attribute, AttributeValue}
import shapeless.labelled.FieldType
import shapeless.ops.hlist.{IsHCons, Mapper}
import shapeless.{
  ::,
  HList,
  HNil,
  LabelledGeneric,
  Lazy,
  Poly1,
  Witness,
  labelled
}
import shapeless.ops.record._

import scala.reflect.ClassTag
import scala.util.Try

trait FieldDecoder[A] {
  def decode(s: Seq[Attribute], n: String): Option[A]
}

object FieldDecoder {
  implicit val hnilDecoder = new FieldDecoder[HNil] {
    override def decode(s: Seq[Attribute], n: String): Option[HNil] = {
      println(s"decode hnil for $n")
      Some(HNil)
    }
  }

  implicit def keyedHConsDecoder[K <: Symbol, H, T <: HList](
      implicit key: Witness.Aux[K],
      head: FieldDecoder[H],
      tail: Decoder[T]
  ): FieldDecoder[FieldType[K, H] :: T] =
    new FieldDecoder[FieldType[K, H] :: T] {
      def decode(s: Seq[Attribute], n: String) = {
        println(s"decode hcons for $n")
        val fieldName = key.value.name
        println(s"field keyed hcons decoder for $fieldName")
        for {
          head <- head.decode(s, fieldName)
          tail <- tail.decode(s)
        } yield labelled.field[K](head) :: tail
      }
    }

  implicit def caseClassDecoder[A, R <: HList](
      implicit gen: LabelledGeneric.Aux[A, R],
      reprDecoder: Lazy[FieldDecoder[R]],
      ct: ClassTag[A]): FieldDecoder[A] =
    new FieldDecoder[A] {
      override def decode(s: Seq[Attribute], n: String): Option[A] = {
        println(s"field decode case class ${ct.runtimeClass.getSimpleName}")
        reprDecoder.value.decode(s, n).map(gen.from)
      }
    }

  implicit def decodeString = new FieldDecoder[String] {
    override def decode(s: Seq[Attribute], n: String): Option[String] = {
      val r = s.find(_.name == n).flatMap(_.value.s)
      println(s"decode string $r for $n")
      r
    }
  }
  implicit def decodeInt = new FieldDecoder[Int] {
    override def decode(s: Seq[Attribute], n: String): Option[Int] = {
      val r = s
        .find(_.name == n)
        .flatMap(_.value.n)
        .flatMap(i => Try(i.toInt).toOption)
      println(s"decode int $r for $n")
      r
    }
  }
  implicit def decodeLong = new FieldDecoder[Long] {
    override def decode(s: Seq[Attribute], n: String): Option[Long] = {
      val r = s
        .find(_.name == n)
        .flatMap(_.value.n)
        .flatMap(i => Try(i.toLong).toOption)
      println(s"decode long $r for $n")
      r
    }
  }
}

trait Decoder[A] {
  def decode(s: Seq[Attribute]): Option[A]
}

object Decoder {

  implicit val hnilDecoder = new Decoder[HNil] {
    override def decode(s: Seq[Attribute]): Option[HNil] = {
      Some(HNil)
    }
  }

  object toName extends Poly1 {
    implicit def keyToName[A] = at[Symbol with A](_.name)
  }

  implicit def keyedHconsDecoder[K <: Symbol, H, T <: HList](
      implicit key: Witness.Aux[K],
      head: FieldDecoder[H],
      tail: Decoder[T]
  ): Decoder[FieldType[K, H] :: T] =
    new Decoder[FieldType[K, H] :: T] {
      def decode(s: Seq[Attribute]) = {
        val fieldName = key.value.name
        println(s"record keyed hcons decoder for $fieldName")
        for {
          head <- head.decode(s, fieldName)
          tail <- tail.decode(s)
        } yield labelled.field[K](head) :: tail
      }
    }
  /*
  implicit def hconsDecoder[H: Decoder,
                            T <: HList: Decoder,
                            O <: HList,
                            P <: HList](implicit kk: Keys.Aux[H :: HNil, O],
                                        m: Mapper.Aux[toName.type, O, P],
                                        ihs: IsHCons[P]) =
    new Decoder[H :: T] {
      override def decode(s: Seq[Attribute]): Option[::[H, T]] = {

        val attrName = (kk.apply() map toName).head.asInstanceOf[String]

        for {
          h <- implicitly[Decoder[H]].decode(s.filter(_.name == attrName))
          t <- implicitly[Decoder[T]].decode(s.filterNot(_.name == attrName))
        } yield h :: t

      }
    }
   */
  implicit def caseClassDecoder[A, R <: HList](
      implicit gen: LabelledGeneric.Aux[A, R],
      reprDecoder: Lazy[Decoder[R]],
      ct: ClassTag[A]): Decoder[A] =
    new Decoder[A] {
      override def decode(s: Seq[Attribute]): Option[A] = {
        println(s"record decode case class ${ct.runtimeClass.getSimpleName}")
        reprDecoder.value.decode(s).map(gen.from)
      }
    }

  def apply[A](s: Seq[Attribute])(implicit decoder: Decoder[A],
                                  ct: ClassTag[A]): Option[A] = {
    println(s"start decoding for ${ct.runtimeClass}")
    decoder.decode(s)
  }
}
