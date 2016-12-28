package net.iakovlev.dynamo.generic

sealed trait AttributeValue
sealed trait AttributeValueScalar extends AttributeValue
sealed trait AttributeValueDocument extends AttributeValue
sealed trait AttributeValueSet extends AttributeValue

case class AttributeValueNumeric(value: String) extends AttributeValueScalar
case class AttributeValueInt(value: Int) extends AttributeValueScalar
case class AttributeValueLong(value: Long) extends AttributeValueScalar
case class AttributeValueFloat(value: Float) extends AttributeValueScalar
case class AttributeValueDouble(value: Double) extends AttributeValueScalar
case class AttributeValueBigDecimal(value: BigDecimal) extends AttributeValueScalar
case class AttributeValueString(value: String) extends AttributeValueScalar
case class AttributeValueBoolean(value: Boolean) extends AttributeValueScalar
case class AttributeValueBinary(value: java.nio.ByteBuffer) extends AttributeValueScalar
case class AttributeValueMap(value: Map[String, AttributeValue])
  extends AttributeValueDocument
case class AttributeValueList(value: Seq[AttributeValue])
  extends AttributeValueDocument
case class AttributeValueBinarySet(value: Seq[java.nio.ByteBuffer]) extends AttributeValueSet
case class AttributeValueNumericSet(value: Seq[String]) extends AttributeValueSet
case class AttributeValueStringSet(value: Seq[String]) extends AttributeValueSet

object AttributeValue {
  def apply(i: Int) = AttributeValueInt(i)
  def apply(l: Long) = AttributeValueLong(l)
  def apply(f: Float) = AttributeValueFloat(f)
  def apply(d: Double) = AttributeValueDouble(d)
  def apply(b: BigDecimal) = AttributeValueBigDecimal(b)
  def apply(s: String) = AttributeValueString(s)
  def apply(a: (String, AttributeValue)*) = AttributeValueMap(a.toMap)
  def apply(a: AttributeValue*) = AttributeValueList(a.toList)
}
