package net.iakovlev.dynamo.generic

import com.amazonaws.services.dynamodbv2.model.GetItemResult
import com.amazonaws.services.dynamodbv2.{model => aws}

import scala.collection.JavaConverters._
import scala.collection.generic._
import scala.language.higherKinds
import scala.util.Try
import cats.implicits._

import scala.collection.mutable

trait AwsAttributeValueDecoder extends Extractors[aws.AttributeValue] {

  def awsDecoder[A](m: Map[String, aws.AttributeValue])(
    implicit d: Decoder[aws.AttributeValue, A]): Either[DecodingError, A] =
    d.decode(m)

  def instance[A](f: (aws.AttributeValue) => A)
    : PrimitivesExtractor[aws.AttributeValue, A] =
    new PrimitivesExtractor[aws.AttributeValue, A] {
      override def extract(a: aws.AttributeValue): Either[DecodingError, A] =
        Either.catchNonFatal(f(a)).leftMap(e => new ExtractionError(e))
    }

  implicit def extractInt: PrimitivesExtractor[aws.AttributeValue, Int] =
    instance(_.getN.toInt)

  implicit def extractLong: PrimitivesExtractor[aws.AttributeValue, Long] =
    instance(_.getN.toLong)

  implicit def extractBoolean
    : PrimitivesExtractor[aws.AttributeValue, Boolean] =
    instance(_.getBOOL)

  implicit def extractFloat: PrimitivesExtractor[aws.AttributeValue, Float] =
    instance(_.getN.toFloat)

  implicit def extractDouble: PrimitivesExtractor[aws.AttributeValue, Double] =
    instance(_.getN.toDouble)

  implicit def extractBigDecimal
    : PrimitivesExtractor[aws.AttributeValue, BigDecimal] =
    instance(a => BigDecimal(a.getN))

  implicit def extractString: PrimitivesExtractor[aws.AttributeValue, String] =
    instance(a => Option(a.getS).get)

  implicit def extractSeq[C[X] <: Seq[X]](
      implicit cbf: CanBuildFrom[C[aws.AttributeValue],
                                 aws.AttributeValue,
                                 C[aws.AttributeValue]]
  ): PrimitivesExtractor[aws.AttributeValue,
                         C[aws.AttributeValue]] =
    new PrimitivesExtractor[aws.AttributeValue,
                            C[aws.AttributeValue]] {
      override def extract(a: aws.AttributeValue)
        : Either[DecodingError, C[aws.AttributeValue]] = {
        val c = cbf()
        Either.catchNonFatal(a.getL.asScala).map { r =>
          r.foreach(c += _)
          c.result()
        }
      }.leftMap {
        case t: Throwable => new ExtractionError(t)
      }
    }

  implicit def extractMap: PrimitivesExtractor[
    aws.AttributeValue,
    Map[String, aws.AttributeValue]] = instance(a => a.getM.asScala.toMap)
}

trait AwsSdkBindings {
  def awsSdkToCore(
      attributeValue: aws.AttributeValue): Option[AttributeValue] = {
    val b = attributeValue.getB
    val bool = attributeValue.getBOOL
    val bs = attributeValue.getBS
    val l = attributeValue.getL
    val m = attributeValue.getM
    val n = attributeValue.getN
    val ns = attributeValue.getNS
    //val `null` = attributeValue.getNULL
    val s = attributeValue.getS
    val ss = attributeValue.getSS

    if (b != null) Some(AttributeValueBinary(b))
    else if (bool != null) Some(AttributeValueBoolean(bool))
    else if (bs != null) Some(AttributeValueBinarySet(bs.asScala))
    else if (l != null)
      Some(AttributeValueList(l.asScala.flatMap(awsSdkToCore)))
    else if (m != null)
      Some(
        AttributeValueMap(
          m.asScala
            .mapValues(awsSdkToCore)
            .collect {
              case (k, Some(v)) => k -> v
            }
            .toMap))
    else if (n != null) Some(AttributeValueNumeric(n))
    else if (ns != null) Some(AttributeValueNumericSet(ns.asScala))
    else if (s != null) Some(AttributeValueString(s))
    else if (ss != null) Some(AttributeValueStringSet(ss.asScala))
    else
      None
  }

  def getItemResultToCore(
      getItemResult: GetItemResult): Map[String, AttributeValue] = {
    getItemResult.getItem.asScala
      .mapValues(awsSdkToCore)
      .collect {
        case (k, Some(v)) => k -> v
      }
      .toMap
  }

  def from[A](getItemResult: GetItemResult)(
      implicit decoder: OptionalDecoder[A]): Option[A] =
    Option(getItemResult).flatMap(gotItemResult =>
      decoder.decode(getItemResultToCore(gotItemResult)))

  def monadic[A](getItemResult: GetItemResult)(
      implicit decoder: Decoder[aws.AttributeValue, A])
    : Either[DecodingError, A] = {
    decoder.decode(getItemResult.getItem.asScala.toMap)
  }
}

object AwsSdkBindings extends AwsSdkBindings //with AwsAttributeValueDecoder
