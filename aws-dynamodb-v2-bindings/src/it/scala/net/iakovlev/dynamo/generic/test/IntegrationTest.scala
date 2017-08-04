package net.iakovlev.dynamo.generic.test

import java.net.URI

import cats.implicits._
import software.amazon.awssdk.services.dynamodb.model._
import software.amazon.awssdk.services.dynamodb._
import software.amazon.awssdk.services.dynamodb.document._
import net.iakovlev.dynamo.generic.{AwsAttributeValueDecoder, AwsSdkBindings}
import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAfterAll
import software.amazon.awssdk.client.builder.AwsClientBuilder

import scala.collection.JavaConverters._

class IntegrationTest
    extends Specification
    with AwsAttributeValueDecoder
    with BeforeAfterAll {

  private val dynamoDB = DynamoDBClient
    .builder()
    .endpointOverride(URI.create("http://localhost:8000"))
    .build()
  private val dynamoDBHelper = new DynamoDb(dynamoDB)
  private val tableName = "dynamo-generic-test"
  override def beforeAll(): Unit = {

    println(s"Creating the table $tableName")
    val createTableRequest = CreateTableRequest
      .builder()
      .tableName(tableName)
      .keySchema(
        KeySchemaElement
          .builder()
          .attributeName("hashKey")
          .keyType(KeyType.HASH)
          .build(),
        KeySchemaElement
          .builder()
          .attributeName("rangeKey")
          .keyType(KeyType.RANGE)
          .build()
      )
      .attributeDefinitions(
        AttributeDefinition.builder
          .attributeName("hashKey")
          .attributeType(ScalarAttributeType.S)
          .build(),
        AttributeDefinition
          .builder()
          .attributeName("rangeKey")
          .attributeType(ScalarAttributeType.S)
          .build()
      )
      .provisionedThroughput(
        ProvisionedThroughput
          .builder()
          .readCapacityUnits(5l)
          .writeCapacityUnits(5l)
          .build())
      .build()

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

  def attr() = AttributeValue.builder()
  def attr(s: String) = AttributeValue.builder.s(s).build()

  "Generic AWS SDK bindings should be able to" >> {
    "do full cycle codec on nested case classes" >> {

      case class Inner(j: String)
      case class Nested(i: Int, p: Inner)
      case class DynamoItem(n: Nested, hashKey: String, rangeKey: String)

      val original = DynamoItem(Nested(3, Inner("hello")), "2", "2")
      val i = for {
        encoded <- AwsSdkBindings.encode[DynamoItem](original)
      } yield {
        dynamoDB.putItem(encoded.toBuilder.tableName(tableName).build())
      }
      i must beRight
      val hk = "2"
      val rk = "2"
      val keys = Map("hashKey" -> attr(hk), "rangeKey" -> attr(rk))

      val req = GetItemRequest.builder
        .tableName(tableName)
        .key(keys.asJava)
        .consistentRead(true)
        .build()
      val resp = dynamoDB.getItem(req)
      val decoded = AwsSdkBindings.decode[DynamoItem](resp)
      decoded must beRight(original)
    }
    "read hand-written data" >> {

      dynamoDB.putItem(
        PutItemRequest
          .builder()
          .tableName(tableName)
          .item(
            Map(
              "hashKey" -> attr("1"),
              "rangeKey" -> attr("2"),
              "numList" -> attr()
                .l(attr().n("10").build(),
                   attr().n("20").build(),
                   attr().n("30").build())
                .build()
            ).asJava
          )
          .build())

      val keys = Map("hashKey" -> attr("1"), "rangeKey" -> attr("2"))
      val req = GetItemRequest
        .builder()
        .tableName(tableName)
        .key(keys.asJava)
        .consistentRead(true)
        .build()
      val resp = dynamoDB.getItem(req)

      case class NumList(hashKey: String, rangeKey: String, numList: List[Int])
      val decoded = AwsSdkBindings.decode[NumList](resp)
      println(decoded)
      decoded must beRight(NumList("1", "2", List(10, 20, 30)))
    }

    "write data" >> {
      val hk = "23"
      val rk = "23"
      val keys = Map("hashKey" -> attr(hk), "rangeKey" -> attr(rk))

      case class Inner(j: String)
      case class Nested(i: Int, p: Inner)
      case class DynamoItem(n: Nested, hashKey: String, rangeKey: String)

      val original = DynamoItem(Nested(10, Inner("hello")), hk, rk)
      val i = for {
        encoded <- AwsSdkBindings.encode[DynamoItem](original)
      } yield {
        dynamoDB.putItem(encoded.toBuilder.tableName(tableName).build)
      }
      i must beRight

      val req = GetItemRequest
        .builder()
        .tableName(tableName)
        .key(keys.asJava)
        .consistentRead(true)
        .build()
      val resp = dynamoDB.getItem(req)

      val expected =
        Map(
          "rangeKey" -> attr("23"),
          "hashKey" -> attr("23"),
          "n" -> attr().m(
            Map("i" -> attr().n("10").build(),
                "p" -> attr()
                  .m(Map("j" -> attr("hello")).asJava)
                  .build()).asJava).build()
        )
      resp.item().asScala.toMap must_=== expected
    }
  }
}
