package net.iakovlev.dynamo.generic.ordered

import awscala.ByteBuffer
import awscala.dynamodbv2.{Attribute, AttributeValue}
import shapeless.{::, Generic, HList, HNil, Lazy}

import scala.reflect.ClassTag
import scala.util.Try

trait LowPriorityGenericDecoderInstances {
  implicit def caseClassDecoder[A, R <: HList](implicit gen: Generic.Aux[A, R],
                                               reprDecoder: Lazy[Decoder[R]],
                                               ct: ClassTag[A]): Decoder[A] =
    new Decoder[A] {
      override def decode(s: Seq[AttributeValue]): Option[A] = {
        println(s"decode case class ${ct.runtimeClass}")
        reprDecoder.value.decode(s).map(gen.from)
      }
    }
}

trait Decoder[A] {
  def decode(s: Seq[AttributeValue]): Option[A]
}

object Decoder extends LowPriorityGenericDecoderInstances {
  implicit def decodeString = new Decoder[String] {
    override def decode(s: Seq[AttributeValue]): Option[String] = {
      val r = s.headOption.flatMap(_.s)
      println(s"decode string $r")
      r
    }
  }

  implicit def decodeInt = new Decoder[Int] {
    override def decode(s: Seq[AttributeValue]): Option[Int] = {
      s.headOption.flatMap(_.n).flatMap(i => Try(i.toInt).toOption)
    }
  }

  implicit def decodeLong = new Decoder[Long] {
    override def decode(s: Seq[AttributeValue]): Option[Long] = {
      s.headOption.flatMap(_.n).flatMap(l => Try(l.toLong).toOption)
    }
  }

  implicit def decodeBoolean = new Decoder[Boolean] {
    override def decode(s: Seq[AttributeValue]): Option[Boolean] = {
      s.headOption.flatMap(_.bl)
    }
  }

  implicit def decodeByteBuffer = new Decoder[ByteBuffer] {
    override def decode(s: Seq[AttributeValue]): Option[ByteBuffer] = {
      s.headOption.flatMap(_.b)
    }
  }

  implicit def decodeIntSeq = new Decoder[Seq[Int]] {
    override def decode(s: Seq[AttributeValue]): Option[Seq[Int]] = {
      s.headOption.map(_.ns.flatMap(s => Try(s.toInt).toOption))
    }
  }

  implicit def decodeLongSeq = new Decoder[Seq[Long]] {
    override def decode(s: Seq[AttributeValue]): Option[Seq[Long]] = {
      s.headOption.map(_.ns.flatMap(s => Try(s.toLong).toOption))
    }
  }

  implicit def decodeStringSeq = new Decoder[Seq[String]] {
    override def decode(s: Seq[AttributeValue]): Option[Seq[String]] = {
      s.headOption.map(_.ss)
    }
  }

  implicit def decodeByteBufferSeq = new Decoder[Seq[ByteBuffer]] {
    override def decode(s: Seq[AttributeValue]): Option[Seq[ByteBuffer]] = {
      s.headOption.map(_.bs)
    }
  }

  implicit def decodeMap[T](implicit dm: Decoder[T]) =
    new Decoder[Map[String, T]] {
      override def decode(s: Seq[AttributeValue]): Option[Map[String, T]] = {
        import scala.collection.JavaConverters._
        s.headOption
          .flatMap(a => a.m)
          .map(
            _.asScala
              .mapValues(v => dm.decode(Seq(AttributeValue(v))))
              .collect {
                case (key, Some(value)) => key -> value
              }
              .toMap)
      }
    }

  implicit def decodeHNil = new Decoder[HNil] {
    override def decode(s: Seq[AttributeValue]): Option[HNil] = {
      println("decoding HNil")
      //if (s.isEmpty) Some(HNil) else None
      Some(HNil)
    }
  }

  implicit def decodeHCons[H: Decoder, T <: HList: Decoder](
      implicit ct: ClassTag[H]) =
    new Decoder[H :: T] {
      override def decode(s: Seq[AttributeValue]): Option[H :: T] = {
        println(s"decode hcons for ${ct.runtimeClass}, attributes: ${s.size}")
        for {
          head <- implicitly[Decoder[H]].decode(s)
          tail <- implicitly[Decoder[T]].decode(s.tail)
        } yield head :: tail
      }
    }

  implicit def caseClass1CellDecoder[A, R, H](implicit gen: Generic.Aux[A, R],
                                              ev: (H :: HNil) =:= R,
                                              c: Lazy[Decoder[H]],
                                              ct: ClassTag[A]): Decoder[A] =
    new Decoder[A] {
      override def decode(s: Seq[AttributeValue]): Option[A] = {
        println(s"decode single field case class ${ct.runtimeClass}")
        c.value.decode(s).map(h => gen.from(ev(h :: HNil)))
      }
    }

  def apply[A](s: Seq[Attribute])(implicit decoder: Decoder[A],
                                  ct: ClassTag[A]): Option[A] = {
    println(s"start decoding for ${ct.runtimeClass}")
    decoder.decode(s.map(_.value))
  }
}
