package net.iakovlev.dynamo.generic

import com.amazonaws.services.dynamodbv2.model.{AttributeValue, GetItemResult}
import com.amazonaws.services.dynamodbv2.{model => aws}

import scala.collection.JavaConverters._
import scala.collection.generic._
import scala.language.higherKinds
import scala.util.Try

trait AwsAttributeValueDecoder extends Extractors[Try, aws.AttributeValue] {

  implicit def extractInt: PrimitivesExtractor[Try, aws.AttributeValue, Int] =
    new PrimitivesExtractor[Try, aws.AttributeValue, Int] {
      override def extract(a: aws.AttributeValue): Try[Int] = {
        Try(a.getN.toInt)
      }
    }

  implicit def extractLong: PrimitivesExtractor[Try, aws.AttributeValue, Long] =
    new PrimitivesExtractor[Try, aws.AttributeValue, Long] {
      override def extract(a: aws.AttributeValue): Try[Long] = {
        Try(a.getN.toLong)
      }
    }

  implicit def extractBoolean: PrimitivesExtractor[Try,
                                                   aws.AttributeValue,
                                                   Boolean] =
    new PrimitivesExtractor[Try, aws.AttributeValue, Boolean] {
      override def extract(a: aws.AttributeValue): Try[Boolean] = {
        Try(a.getBOOL)
      }
    }

  implicit def extractFloat: PrimitivesExtractor[Try,
                                                 aws.AttributeValue,
                                                 Float] =
    new PrimitivesExtractor[Try, aws.AttributeValue, Float] {
      override def extract(a: aws.AttributeValue): Try[Float] = {
        Try(a.getN.toFloat)
      }
    }

  implicit def extractDouble: PrimitivesExtractor[Try,
                                                  aws.AttributeValue,
                                                  Double] =
    new PrimitivesExtractor[Try, aws.AttributeValue, Double] {
      override def extract(a: aws.AttributeValue): Try[Double] = {
        Try(a.getN.toDouble)
      }
    }

  implicit def extractBigDecimal: PrimitivesExtractor[Try,
                                                      aws.AttributeValue,
                                                      BigDecimal] =
    new PrimitivesExtractor[Try, aws.AttributeValue, BigDecimal] {
      override def extract(a: aws.AttributeValue): Try[BigDecimal] = {
        Try(BigDecimal(a.getN))
      }
    }
  implicit def extractString: PrimitivesExtractor[Try,
                                                  aws.AttributeValue,
                                                  String] =
    new PrimitivesExtractor[Try, aws.AttributeValue, String] {
      override def extract(a: aws.AttributeValue): Try[String] = {
        Try(Option(a.getS).get)
      }
    }
  implicit def extractSeq[C[X] <: Seq[X]](
      implicit cbf: CanBuildFrom[C[Try[aws.AttributeValue]],
                                 Try[aws.AttributeValue],
                                 C[Try[aws.AttributeValue]]]
  ): PrimitivesExtractor[Try, aws.AttributeValue, C[Try[aws.AttributeValue]]] =
    new PrimitivesExtractor[Try,
                            aws.AttributeValue,
                            C[Try[aws.AttributeValue]]] {
      override def extract(
          a: aws.AttributeValue): Try[C[Try[aws.AttributeValue]]] = {
        for {
          c <- Try(cbf())
          r = a.getL.asScala.map(Try(_))
        } yield {
          r.foreach(c += _)
          c.result()
        }
      }
    }

  implicit def extractMap: PrimitivesExtractor[
    Try,
    aws.AttributeValue,
    Map[String, aws.AttributeValue]] =
    new PrimitivesExtractor[Try,
                            aws.AttributeValue,
                            Map[String, aws.AttributeValue]] {
      override def extract(
          a: aws.AttributeValue): Try[Map[String, aws.AttributeValue]] = {
        Try(a.getM.asScala.toMap)
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
