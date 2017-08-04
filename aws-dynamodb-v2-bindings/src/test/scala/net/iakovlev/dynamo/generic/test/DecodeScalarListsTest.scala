package net.iakovlev.dynamo.generic.test

import net.iakovlev.dynamo.generic._
import org.specs2.mutable.Specification

class DecodeScalarListsTest
    extends Specification
    with AwsAttributeValueDecoder
with TestBase{

  "Decode scalar lists" >> {
    case class Parent(cc: Seq[Int])
    val res = awsDecoder[Parent](
      Map("cc" -> attr.l(attr().n("1").build()).build())
    )
    res must beRight(Parent(Seq(1)))
  }
}
