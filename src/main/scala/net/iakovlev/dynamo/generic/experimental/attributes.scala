package net.iakovlev.dynamo.generic.experimental

sealed trait AttributeValue
sealed trait AttributeValueScalar extends AttributeValue
sealed trait AttributeValueDocument extends AttributeValue
sealed trait AttributeValueSet extends AttributeValue

case class AttributeValueNumeric(value: Int) extends AttributeValueScalar
case class AttributeValueString(value: String) extends AttributeValueScalar
case class AttributeValueMap(value: Map[String, AttributeValue])
  extends AttributeValueDocument
case class AttributeValueList(value: List[AttributeValue])
  extends AttributeValueDocument

object AttributeValue {
  def apply(i: Int) = AttributeValueNumeric(i)
  def apply(s: String) = AttributeValueString(s)
  def apply(a: (String, AttributeValue)*) = AttributeValueMap(a.toMap)
  def apply(a: AttributeValue*) = AttributeValueList(a.toList)
}
