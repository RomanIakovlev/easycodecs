package net.iakovlev.dynamo.generic.test

import net.iakovlev.dynamo.generic.AwsAttributeValueDecoder
import org.specs2.mutable.Specification
import com.amazonaws.services.dynamodbv2.{model => aws}

class DecodeCaseClassesListsTest
    extends Specification
    with AwsAttributeValueDecoder {

  "Decode case classes lists" >> {
    case class Child(s: String)
    case class Parent(c: Vector[Child])
    val res = awsDecoder[Parent](
      Map(
        "c" -> new aws.AttributeValue()
          .withL(new aws.AttributeValue()
            .addMEntry("s", new aws.AttributeValue("bla")))))

    res must beRight(Parent(Vector(Child("bla"))))
  }
}
