package net.iakovlev.dynamo.generic

import com.amazonaws.services.dynamodbv2.model.{AttributeValue, GetItemResult}
import com.amazonaws.services.dynamodbv2.{model => aws}

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}
import cats.implicits._
import shapeless.{Coproduct, LabelledGeneric, Lazy, LowPriority}

trait AwsAttributeValueDecoder {
  implicit def optionalAwsDecoder[A](
      implicit d: SingleFieldEffectfulDecoder[Try, aws.AttributeValue, A])
    : SingleFieldEffectfulDecoder[Try, aws.AttributeValue, Option[A]] =
    new SingleFieldEffectfulDecoder[Try, aws.AttributeValue, Option[A]] {
      override def decode(a: Try[aws.AttributeValue]): Try[Option[A]] = {
        d.decode(a).map(Some.apply).recoverWith {
          case _: NoSuchElementException =>
            Success(None)
          case e =>
            Failure(e)
        }
      }
    }
  implicit def intAwsDecoder: SingleFieldEffectfulDecoder[Try,
                                                          aws.AttributeValue,
                                                          Int] =
    new SingleFieldEffectfulDecoder[Try, aws.AttributeValue, Int] {
      override def decode(a: Try[aws.AttributeValue]): Try[Int] =
        a.map(_.getN.toInt)
    }
  implicit def longAwsDecoder: SingleFieldEffectfulDecoder[Try,
                                                           aws.AttributeValue,
                                                           Long] =
    new SingleFieldEffectfulDecoder[Try, aws.AttributeValue, Long] {
      override def decode(a: Try[aws.AttributeValue]): Try[Long] =
        a.map(_.getN.toLong)
    }
  implicit def floatAwsDecoder: SingleFieldEffectfulDecoder[Try,
                                                            aws.AttributeValue,
                                                            Float] =
    new SingleFieldEffectfulDecoder[Try, aws.AttributeValue, Float] {
      override def decode(a: Try[aws.AttributeValue]): Try[Float] =
        a.map(_.getN.toFloat)
    }
  implicit def doubleAwsDecoder: SingleFieldEffectfulDecoder[
    Try,
    aws.AttributeValue,
    Double] =
    new SingleFieldEffectfulDecoder[Try, aws.AttributeValue, Double] {
      override def decode(a: Try[aws.AttributeValue]): Try[Double] =
        a.map(_.getN.toDouble)
    }
  implicit def bigDecimalAwsDecoder: SingleFieldEffectfulDecoder[
    Try,
    aws.AttributeValue,
    BigDecimal] =
    new SingleFieldEffectfulDecoder[Try, aws.AttributeValue, BigDecimal] {
      override def decode(a: Try[aws.AttributeValue]): Try[BigDecimal] =
        a.map(n => BigDecimal(n.getN))
    }
  implicit def stringAwsDecoder: SingleFieldEffectfulDecoder[
    Try,
    aws.AttributeValue,
    String] =
    new SingleFieldEffectfulDecoder[Try, aws.AttributeValue, String] {
      override def decode(attr: Try[aws.AttributeValue]): Try[String] =
        attr.map(a => Option(a.getS).get)
    }
  implicit def listAwsDecoder[A](
      implicit ad: SingleFieldEffectfulDecoder[Try, aws.AttributeValue, A])
    : SingleFieldEffectfulDecoder[Try, aws.AttributeValue, List[A]] =
    new SingleFieldEffectfulDecoder[Try, aws.AttributeValue, List[A]] {
      override def decode(a: Try[aws.AttributeValue]): Try[List[A]] = {
        a.flatMap(_.getL.asScala.toList.map(b => Try(b)).traverse(ad.decode))
      }
    }
  implicit def mapAsClassAwsDecoder[A](
      implicit d: EffectfulDecoder[Try, aws.AttributeValue, A])
    : SingleFieldEffectfulDecoder[Try, aws.AttributeValue, A] =
    new SingleFieldEffectfulDecoder[Try, aws.AttributeValue, A] {
      override def decode(a: Try[aws.AttributeValue]): Try[A] = {
        a.flatMap(attr => d.decode(attr.getM.asScala.toMap))
      }
    }
  implicit def mapAsMapAwsDecoder[A](
      implicit d: SingleFieldEffectfulDecoder[Try, aws.AttributeValue, A]) =
    new SingleFieldEffectfulDecoder[Try, aws.AttributeValue, Map[String, A]] {
      override def decode(a: Try[aws.AttributeValue]): Try[Map[String, A]] = {
        a.map(_.getM.asScala.toMap.mapValues(v => d.decode(Try(v))).collect {
          case (k, Success(v)) => k -> v
        })
      }
    }
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
      implicit decoder: Decoder[A]): Option[A] =
    Option(getItemResult).flatMap(gotItemResult =>
      decoder.decode(getItemResultToCore(gotItemResult)))

  def monadic[A](getItemResult: GetItemResult)(
      implicit decoder: EffectfulDecoder[Try, aws.AttributeValue, A])
    : Try[A] = {
    Try {
      decoder.decode(getItemResult.getItem.asScala.toMap)
    }.flatten
  }
}

object AwsSdkBindings extends AwsSdkBindings //with AwsAttributeValueDecoder
