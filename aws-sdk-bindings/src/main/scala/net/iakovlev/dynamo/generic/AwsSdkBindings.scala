package net.iakovlev.dynamo.generic

import com.amazonaws.services.dynamodbv2.model.{GetItemResult, PutItemRequest}
import com.amazonaws.services.dynamodbv2.{model => aws}

import scala.collection.JavaConverters._
import scala.collection.generic._
import scala.language.higherKinds
import scala.util.Try
import cats.implicits._

import scala.collection.mutable

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
        Either.catchNonFatal(f(a)).leftMap(e => new ExtractionError(e))
    }

  implicit def readInt: PrimitivesReader[aws.AttributeValue, Int] =
    instance(_.getN.toInt)

  implicit def readLong: PrimitivesReader[aws.AttributeValue, Long] =
    instance(_.getN.toLong)

  implicit def readBoolean
    : PrimitivesReader[aws.AttributeValue, Boolean] =
    instance(_.getBOOL)

  implicit def readFloat: PrimitivesReader[aws.AttributeValue, Float] =
    instance(_.getN.toFloat)

  implicit def readDouble: PrimitivesReader[aws.AttributeValue, Double] =
    instance(_.getN.toDouble)

  implicit def readBigDecimal
    : PrimitivesReader[aws.AttributeValue, BigDecimal] =
    instance(a => BigDecimal(a.getN))

  implicit def readString: PrimitivesReader[aws.AttributeValue, String] =
    instance(a => Option(a.getS).get)

  implicit def readIterable[C[X] <: Iterable[X]](
      implicit cbf: CanBuildFrom[C[aws.AttributeValue],
                                 aws.AttributeValue,
                                 C[aws.AttributeValue]]
  ): PrimitivesReader[aws.AttributeValue, C[aws.AttributeValue]] =
    new PrimitivesReader[aws.AttributeValue, C[aws.AttributeValue]] {
      override def extract(a: aws.AttributeValue)
        : Either[DecodingError, C[aws.AttributeValue]] = {
        val c = cbf()
        Either.catchNonFatal(a.getL.asScala).map { r =>
          r.foreach(c += _)
          c.result()
        }
      }.leftMap { t: Throwable =>
        new ExtractionError(t)
      }
    }

  implicit def readMap
    : PrimitivesReader[aws.AttributeValue, Map[String, aws.AttributeValue]] =
    instance(a => a.getM.asScala.toMap)

  def writerInstance[A](
      f: A => aws.AttributeValue): PrimitivesWriter[A, aws.AttributeValue] =
    new PrimitivesWriter[A, aws.AttributeValue] {
      override def write(a: A): aws.AttributeValue =
        f(a)
    }

  override implicit def writeInt: PrimitivesWriter[Int, aws.AttributeValue] =
    writerInstance(a => new aws.AttributeValue().withN(a.toString))

  override implicit def writeLong: PrimitivesWriter[Long, aws.AttributeValue] =
    writerInstance(a => new aws.AttributeValue().withN(a.toString))

  override implicit def writeFloat
    : PrimitivesWriter[Float, aws.AttributeValue] =
    writerInstance(a => new aws.AttributeValue().withN(a.toString))

  override implicit def writeDouble
    : PrimitivesWriter[Double, aws.AttributeValue] =
    writerInstance(a => new aws.AttributeValue().withN(a.toString))

  override implicit def writeBigDecimal
    : PrimitivesWriter[BigDecimal, aws.AttributeValue] =
    writerInstance(a => new aws.AttributeValue().withN(a.toString))

  override implicit def writeBoolean
    : PrimitivesWriter[Boolean, aws.AttributeValue] =
    writerInstance(a => new aws.AttributeValue().withBOOL(a))

  override implicit def writeString
    : PrimitivesWriter[String, aws.AttributeValue] =
    writerInstance(a => new aws.AttributeValue().withS(a))

  override implicit def writeIterable[C[X] <: Iterable[X]](
      implicit canBuildFrom: CanBuildFrom[C[aws.AttributeValue],
                                          aws.AttributeValue,
                                          C[aws.AttributeValue]])
    : PrimitivesWriter[C[aws.AttributeValue], aws.AttributeValue] =
    writerInstance { a =>
      val j: java.util.Collection[aws.AttributeValue] = a.asJavaCollection
        new aws.AttributeValue()
          .withL(j)
    }

  override implicit def writeMap
    : PrimitivesWriter[Map[String, aws.AttributeValue], aws.AttributeValue] =
    writerInstance(a => new aws.AttributeValue().withM(a.asJava))
}

trait AwsSdkBindings {

  def decode[A](getItemResult: GetItemResult)(
      implicit decoder: Decoder[aws.AttributeValue, A])
    : Either[DecodingError, A] = {
    decoder.decode(getItemResult.getItem.asScala.toMap)
  }

  def encode[A](a: A)(
      implicit encoder: Encoder[A, aws.AttributeValue])
    : Either[EncodingError, PutItemRequest] =
    encoder.encode(a).map(m => new PutItemRequest().withItem(m.asJava))
}

object AwsSdkBindings extends AwsSdkBindings //with AwsAttributeValueDecoder
