package net.iakovlev.dynamo.generic

sealed trait AttributeValueGeneric[A] {
  val value: A
}
sealed trait AttributeValueScalarGeneric[A] extends AttributeValueGeneric[A]

case class AttributeValueNumericGeneric(value: String) extends AttributeValueScalarGeneric[String]
case class AttributeValueIntGeneric(value: Int) extends AttributeValueScalarGeneric[Int]
case class AttributeValueLongGeneric(value: Long) extends AttributeValueScalarGeneric[Long]
case class AttributeValueFloatGeneric(value: Float) extends AttributeValueScalarGeneric[Float]
case class AttributeValueDoubleGeneric(value: Double) extends AttributeValueScalarGeneric[Double]
case class AttributeValueBigDecimalGeneric(value: BigDecimal) extends AttributeValueScalarGeneric[BigDecimal]
case class AttributeValueStringGeneric(value: String) extends AttributeValueScalarGeneric[String]
case class AttributeValueBooleanGeneric(value: Boolean) extends AttributeValueScalarGeneric[Boolean]
case class AttributeValueBinaryGeneric(value: java.nio.ByteBuffer) extends AttributeValueScalarGeneric[java.nio.ByteBuffer]

object AttributeValueGeneric {
  def apply(i: Int) = AttributeValueIntGeneric(i)
  def apply(l: Long) = AttributeValueLongGeneric(l)
  def apply(f: Float) = AttributeValueFloatGeneric(f)
  def apply(d: Double) = AttributeValueDoubleGeneric(d)
  def apply(b: BigDecimal) = AttributeValueBigDecimalGeneric(b)
  def apply(s: String) = AttributeValueStringGeneric(s)
}
