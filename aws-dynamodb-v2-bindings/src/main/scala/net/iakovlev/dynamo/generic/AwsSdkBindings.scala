package net.iakovlev.dynamo.generic

import software.amazon.awssdk.services.dynamodb.model.{GetItemResponse, PutItemRequest}
import software.amazon.awssdk.services.dynamodb.{model => aws}

import scala.collection.JavaConverters._
import scala.collection.generic._
import scala.language.higherKinds
import cats.implicits._
import net.iakovlev.easycodecs.decoder._
import net.iakovlev.easycodecs.encoder._

trait AwsAttributeValueDecoder
    extends Readers[aws.AttributeValue]
    with Writers[aws.AttributeValue] {

  def awsDecoder[A](m: Map[String, aws.AttributeValue])(
      implicit d: Decoder[aws.AttributeValue, A]): Either[DecodingError, A] = {
    d.decode(m)
  }

  def instance[A](f: (aws.AttributeValue) => A)
    : PrimitivesReader[aws.AttributeValue, A] =
    new PrimitivesReader[aws.AttributeValue, A] {
      override def extract(a: aws.AttributeValue): Either[DecodingError, A] =
        Either.catchNonFatal(f(a)).leftMap(e => new ReadingError(e))
    }

  implicit def readInt: PrimitivesReader[aws.AttributeValue, Int] =
    instance(_.n().toInt)

  implicit def readLong: PrimitivesReader[aws.AttributeValue, Long] =
    instance(_.n().toLong)

  implicit def readBoolean
    : PrimitivesReader[aws.AttributeValue, Boolean] =
    instance(_.bool())

  implicit def readFloat: PrimitivesReader[aws.AttributeValue, Float] =
    instance(_.n().toFloat)

  implicit def readDouble: PrimitivesReader[aws.AttributeValue, Double] =
    instance(_.n().toDouble)

  implicit def readBigDecimal
    : PrimitivesReader[aws.AttributeValue, BigDecimal] =
    instance(a => BigDecimal(a.n()))

  implicit def readString: PrimitivesReader[aws.AttributeValue, String] =
    instance(a => Option(a.s()).get)

  implicit def readIterable[C[X] <: Iterable[X]](
      implicit cbf: CanBuildFrom[C[aws.AttributeValue],
                                 aws.AttributeValue,
                                 C[aws.AttributeValue]]
  ): PrimitivesReader[aws.AttributeValue, C[aws.AttributeValue]] =
    new PrimitivesReader[aws.AttributeValue, C[aws.AttributeValue]] {
      override def extract(a: aws.AttributeValue)
        : Either[DecodingError, C[aws.AttributeValue]] = {
        val c = cbf()
        Either.catchNonFatal(a.l().asScala).map { r =>
          r.foreach(c += _)
          c.result()
        }
      }.leftMap { t: Throwable =>
        new ReadingError(t)
      }
    }

  implicit def readMap
    : PrimitivesReader[aws.AttributeValue, Map[String, aws.AttributeValue]] =
    instance(a => a.m().asScala.toMap)

  def writerInstance[A](
      f: A => aws.AttributeValue): PrimitivesWriter[A, aws.AttributeValue] =
    new PrimitivesWriter[A, aws.AttributeValue] {
      override def write(a: A): aws.AttributeValue =
        f(a)
    }

  def builder() = aws.AttributeValue.builder()

  override implicit def writeInt: PrimitivesWriter[Int, aws.AttributeValue] =
    writerInstance(a => builder().n(a.toString).build())

  override implicit def writeLong: PrimitivesWriter[Long, aws.AttributeValue] =
    writerInstance(a => builder().n(a.toString).build())

  override implicit def writeFloat
    : PrimitivesWriter[Float, aws.AttributeValue] =
    writerInstance(a => builder().n(a.toString).build())

  override implicit def writeDouble
    : PrimitivesWriter[Double, aws.AttributeValue] =
    writerInstance(a => builder().n(a.toString).build())

  override implicit def writeBigDecimal
    : PrimitivesWriter[BigDecimal, aws.AttributeValue] =
    writerInstance(a => builder().n(a.toString).build())

  override implicit def writeBoolean
    : PrimitivesWriter[Boolean, aws.AttributeValue] =
    writerInstance(a => builder().bool(a).build())

  override implicit def writeString
    : PrimitivesWriter[String, aws.AttributeValue] =
    writerInstance(a => builder().s(a).build())

  override implicit def writeIterable[C[X] <: Iterable[X]](
      implicit canBuildFrom: CanBuildFrom[C[aws.AttributeValue],
                                          aws.AttributeValue,
                                          C[aws.AttributeValue]])
    : PrimitivesWriter[C[aws.AttributeValue], aws.AttributeValue] =
    writerInstance { a =>
      val j: java.util.Collection[aws.AttributeValue] = a.asJavaCollection
        builder().l(j).build()
    }

  override implicit def writeMap
    : PrimitivesWriter[Map[String, aws.AttributeValue], aws.AttributeValue] =
    writerInstance(a => builder().m(a.asJava).build())
}

trait AwsSdkBindings {

  def decode[A](getItemResult: GetItemResponse)(
      implicit decoder: Decoder[aws.AttributeValue, A])
    : Either[DecodingError, A] = {
    decoder.decode(getItemResult.item().asScala.toMap)
  }

  def encode[A](a: A)(
      implicit encoder: Encoder[A, aws.AttributeValue])
    : Either[EncodingError, PutItemRequest] =
    encoder.encode(a).map(m => PutItemRequest.builder().item(m.asJava).build())
}

object AwsSdkBindings extends AwsSdkBindings
