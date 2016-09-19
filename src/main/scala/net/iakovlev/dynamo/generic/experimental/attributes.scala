package net.iakovlev.dynamo.generic.experimental

sealed trait AttributeValue
sealed trait AttributeValueScalar extends AttributeValue
sealed trait AttributeValueDocument extends AttributeValue
sealed trait AttributeValueSet extends AttributeValue

case class AttributeValueInt(value: Int) extends AttributeValueScalar
case class AttributeValueLong(value: Long) extends AttributeValueScalar
case class AttributeValueFloat(value: Float) extends AttributeValueScalar
case class AttributeValueDouble(value: Double) extends AttributeValueScalar
case class AttributeValueBigDecimal(value: BigDecimal) extends AttributeValueScalar
case class AttributeValueString(value: String) extends AttributeValueScalar
case class AttributeValueMap(value: Map[String, AttributeValue])
  extends AttributeValueDocument
case class AttributeValueList(value: List[AttributeValue])
  extends AttributeValueDocument

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
