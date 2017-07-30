package net.iakovlev.dynamo.generic
import cats.syntax.either._

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

sealed trait AttributeValue
sealed trait AttributeValueScalar extends AttributeValue
sealed trait AttributeValueDocument extends AttributeValue
sealed trait AttributeValueSet extends AttributeValue

case class AttributeValueNumeric(value: String) extends AttributeValueScalar
case class AttributeValueInt(value: Int) extends AttributeValueScalar
case class AttributeValueLong(value: Long) extends AttributeValueScalar
case class AttributeValueFloat(value: Float) extends AttributeValueScalar
case class AttributeValueDouble(value: Double) extends AttributeValueScalar
case class AttributeValueBigDecimal(value: BigDecimal)
    extends AttributeValueScalar
case class AttributeValueString(value: String) extends AttributeValueScalar
case class AttributeValueBoolean(value: Boolean) extends AttributeValueScalar
case class AttributeValueBinary(value: java.nio.ByteBuffer)
    extends AttributeValueScalar
case class AttributeValueMap(value: Map[String, AttributeValue])
    extends AttributeValueDocument
case class AttributeValueList(value: Seq[AttributeValue])
    extends AttributeValueDocument
case class AttributeValueBinarySet(value: Seq[java.nio.ByteBuffer])
    extends AttributeValueSet
case class AttributeValueNumericSet(value: Seq[String])
    extends AttributeValueSet
case class AttributeValueStringSet(value: Seq[String]) extends AttributeValueSet

object AttributeValue extends AttributeValueBindings {
  def apply(i: Int) = AttributeValueInt(i)
  def apply(l: Long) = AttributeValueLong(l)
  def apply(f: Float) = AttributeValueFloat(f)
  def apply(d: Double) = AttributeValueDouble(d)
  def apply(b: BigDecimal) = AttributeValueBigDecimal(b)
  def apply(s: String) = AttributeValueString(s)
  def apply(a: (String, AttributeValue)*) = AttributeValueMap(a.toMap)
  def apply(a: AttributeValue*) = AttributeValueList(a.toList)
}

trait AttributeValueBindings
    extends Extractors[AttributeValue]
    with Writers[AttributeValue] {

  def extractorInstance[A](
      f: (AttributeValue) => A): PrimitivesExtractor[AttributeValue, A] =
    new PrimitivesExtractor[AttributeValue, A] {
      override def extract(a: AttributeValue): Either[DecodingError, A] =
        Either.catchNonFatal(f(a)).leftMap(e => new ExtractionError(e))
    }

  override implicit def extractInt: PrimitivesExtractor[AttributeValue, Int] =
    extractorInstance {
      case a: AttributeValueInt => a.value
    }

  override implicit def extractLong: PrimitivesExtractor[AttributeValue, Long] =
    extractorInstance {
      case a: AttributeValueLong => a.value
    }

  override implicit def extractFloat
    : PrimitivesExtractor[AttributeValue, Float] =
    extractorInstance {
      case a: AttributeValueFloat => a.value
    }

  override implicit def extractDouble
    : PrimitivesExtractor[AttributeValue, Double] =
    extractorInstance {
      case a: AttributeValueDouble => a.value
    }

  override implicit def extractBigDecimal
    : PrimitivesExtractor[AttributeValue, BigDecimal] =
    extractorInstance {
      case a: AttributeValueBigDecimal => a.value
    }

  override implicit def extractBoolean
    : PrimitivesExtractor[AttributeValue, Boolean] =
    extractorInstance {
      case a: AttributeValueBoolean => a.value
    }

  override implicit def extractString
    : PrimitivesExtractor[AttributeValue, String] =
    extractorInstance {
      case a: AttributeValueString => a.value
    }

  override implicit def extractSeq[C[X] <: Seq[X]](
      implicit canBuildFrom: CanBuildFrom[C[AttributeValue],
                                          AttributeValue,
                                          C[AttributeValue]])
    : PrimitivesExtractor[AttributeValue, C[AttributeValue]] =
    new PrimitivesExtractor[AttributeValue, C[AttributeValue]] {
      override def extract(
          a: AttributeValue): Either[DecodingError, C[AttributeValue]] = {
        val c = canBuildFrom()
        Either
          .catchNonFatal {
            a match {
              case a: AttributeValueList =>
                a.value.foreach(c += _)
            }
            c.result()
          }
          .leftMap { t: Throwable =>
            new ExtractionError(t)
          }
      }
    }

  implicit override def extractMap
    : PrimitivesExtractor[AttributeValue, Map[String, AttributeValue]] =
    extractorInstance {
      case a: AttributeValueMap => a.value
    }

  def writerInstance[A](
      f: A => AttributeValue): PrimitivesWriter[A, AttributeValue] =
    new PrimitivesWriter[A, AttributeValue] {
      override def write(a: A): AttributeValue =
        f(a)
    }

  override implicit def writeInt: PrimitivesWriter[Int, AttributeValue] =
    writerInstance(AttributeValueInt)

  override implicit def writeLong: PrimitivesWriter[Long, AttributeValue] =
    writerInstance(AttributeValueLong)

  override implicit def writeFloat: PrimitivesWriter[Float, AttributeValue] =
    writerInstance(AttributeValueFloat)

  override implicit def writeDouble: PrimitivesWriter[Double, AttributeValue] =
    writerInstance(AttributeValueDouble)

  override implicit def writeBigDecimal: PrimitivesWriter[BigDecimal, AttributeValue] =
    writerInstance(AttributeValueBigDecimal)

  override implicit def writeBoolean: PrimitivesWriter[Boolean, AttributeValue] =
    writerInstance(AttributeValueBoolean)

  override implicit def writeString: PrimitivesWriter[String, AttributeValue] =
    writerInstance(AttributeValueString)

  override implicit def writeSeq[C[X] <: Seq[X]](implicit canBuildFrom: CanBuildFrom[C[AttributeValue], AttributeValue, C[AttributeValue]]): PrimitivesWriter[C[AttributeValue], AttributeValue] =
    writerInstance(AttributeValueList)

  override implicit def writeMap: PrimitivesWriter[Map[String, AttributeValue], AttributeValue] =
    writerInstance(AttributeValueMap)
}
