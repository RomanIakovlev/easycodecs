package net.iakovlev.dynamo.generic

import com.amazonaws.services.dynamodbv2.model.GetItemResult
import com.amazonaws.services.dynamodbv2.{model => aws}

import scala.collection.JavaConverters._

trait AwsSdkBindings {
  def awsSdkToCore(attributeValue: aws.AttributeValue): Option[AttributeValue] = {
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

    if (b != null) Some(AttributeValueBinary(b)) else
    if (bool != null) Some(AttributeValueBoolean(bool)) else
    if (bs != null) Some(AttributeValueBinarySet(bs.asScala)) else
    if (l != null) Some(AttributeValueList(l.asScala.flatMap(awsSdkToCore))) else
    if (m != null) Some(AttributeValueMap(m.asScala.mapValues(awsSdkToCore).collect {
      case (k, Some(v)) => k -> v
    }.toMap)) else
    if (n != null) Some(AttributeValueNumeric(n)) else
    if (ns != null) Some(AttributeValueNumericSet(ns.asScala)) else
    if (s != null) Some(AttributeValueString(s)) else
    if (ss != null) Some(AttributeValueStringSet(ss.asScala)) else
    None
  }

  def getItemResultToCore(getItemResult: GetItemResult): Map[String, AttributeValue] = {
    getItemResult.getItem.asScala.mapValues(awsSdkToCore).collect {
      case (k, Some(v)) => k -> v
    }.toMap
  }

  def from[A](getItemResult: GetItemResult)(implicit decoder: Decoder[A]): Option[A] =
    Option(getItemResult).flatMap(gotItemResult => decoder.decode(getItemResultToCore(gotItemResult)))
}

object AwsSdkBindings extends AwsSdkBindings