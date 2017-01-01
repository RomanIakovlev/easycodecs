package net.iakovlev.dynamo.generic.test
import com.amazonaws.services.dynamodbv2.{AmazonDynamoDBClient, model => aws}
import com.amazonaws.services.dynamodbv2.document.DynamoDB
import com.amazonaws.services.dynamodbv2.model._
import net.iakovlev.dynamo.generic.{AwsAttributeValueDecoder, AwsSdkBindings, SingleFieldEffectfulDecoder}
import org.specs2.mutable.Specification

import scala.collection.JavaConverters._
import cats.implicits._
import org.specs2.specification.BeforeAfterAll

import scala.util.{Failure, Success, Try}

class IntegrationTest extends Specification with AwsAttributeValueDecoder with BeforeAfterAll {

  val dynamoDB = new AmazonDynamoDBClient
  val dynamoDBHelper = new DynamoDB(dynamoDB)
  val tableName = "dynamo-generic-test"
  override def beforeAll(): Unit = {

    println(s"Creating the table $tableName")
    val createTableRequest = new CreateTableRequest(
      List(
        new AttributeDefinition("hashKey", ScalarAttributeType.S),
        new AttributeDefinition("rangeKey", ScalarAttributeType.S)
      ).asJava,
      tableName,
      List(
        new KeySchemaElement("hashKey", KeyType.HASH),
        new KeySchemaElement("rangeKey", KeyType.RANGE)
      ).asJava, new ProvisionedThroughput(5l, 5l)
    )

    val table = dynamoDBHelper.createTable(createTableRequest)
    table.waitForActive()
    println(s"Table $tableName has became active")
  }

  override def afterAll(): Unit = {
    val table = dynamoDBHelper.getTable(tableName)
    table.delete()
    table.waitForDelete()
    println(s"Table $tableName is deleted")
  }

  "Generic AWS SDK bindings should be able to" >> {
    "decode nested case classes" >> {
      case class Inner(j: String)
      case class Nested(i: Int, p: Inner)
      case class Outer(n: Nested, hashKey: String, rangeKey: String)

      val keys = Map(
        "hashKey" -> new aws.AttributeValue("2"),
        "rangeKey" -> new aws.AttributeValue("2"))

      dynamoDB.putItem(tableName,
        (keys + (
          "n" -> new aws.AttributeValue()
            .addMEntry("i", new aws.AttributeValue().withN("10"))
            .addMEntry("p", new aws.AttributeValue()
              .addMEntry("j", new aws.AttributeValue().withS("hello"))))).asJava)

      val req = new GetItemRequest("dynamo-generic-test", keys.asJava, true)
      val resp = dynamoDB.getItem(req)
      val decoded = AwsSdkBindings.monadic[Outer](resp)
      decoded match {
        case Success(r) => r must_== Outer(Nested(10, Inner("hello")), "2", "2")
        case Failure(ex) =>
          ex.printStackTrace()
      }
      println(decoded)
      decoded must_== Success(Outer(Nested(10, Inner("hello")), "2", "2"))
    }
    "read data from real table" >> {

      dynamoDB.putItem(tableName,
        Map(
          "hashKey" -> new aws.AttributeValue("1"),
          "rangeKey" -> new aws.AttributeValue().withS("2"),
          "numList" -> new aws.AttributeValue().withL(
            new aws.AttributeValue().withN("10"),
            new aws.AttributeValue().withN("20"),
            new aws.AttributeValue().withN("30"))).asJava)

      val keys = Map(
        "hashKey" -> new aws.AttributeValue("1"),
        "rangeKey" -> new aws.AttributeValue().withS("2"))
      val req = new GetItemRequest("dynamo-generic-test", keys.asJava, true)
      val resp = dynamoDB.getItem(req)

      case class NumList(hashKey: String, rangeKey: String, numList: List[Int])
      val decoded = AwsSdkBindings.monadic[NumList](resp)
      println(decoded)
      decoded must_== Success(NumList("1", "2", List(10, 20, 30)))

      case class NumList1(hashKey: String, rangeKey: String)
      val decoded1 = AwsSdkBindings.monadic[NumList1](resp)
      println(decoded1)
      decoded1 must_== Success(NumList1("1", "2"))
    }

  }
}
