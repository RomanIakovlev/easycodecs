package net.iakovlev.dynamo.generic.test
import cats.implicits._
import com.amazonaws.client.builder.AwsClientBuilder
import com.amazonaws.services.dynamodbv2.document.DynamoDB
import com.amazonaws.services.dynamodbv2.model._
import com.amazonaws.services.dynamodbv2.{
  AmazonDynamoDBClientBuilder,
  model => aws
}
import net.iakovlev.dynamo.generic.{AwsAttributeValueDecoder, AwsSdkBindings}
import org.specs2.mutable.Specification
import org.specs2.specification.BeforeAfterAll

import scala.collection.JavaConverters._

class IntegrationTest
    extends Specification
    with AwsAttributeValueDecoder
    with BeforeAfterAll {

  private val dynamoDB = AmazonDynamoDBClientBuilder
    .standard()
    .withEndpointConfiguration(
      new AwsClientBuilder.EndpointConfiguration("http://localhost:8000",
                                                 "us-west-2"))
    .build()
  private val dynamoDBHelper = new DynamoDB(dynamoDB)
  private val tableName = "dynamo-generic-test"
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
      ).asJava,
      new ProvisionedThroughput(5l, 5l)
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

  def attr() = new aws.AttributeValue()
  def attr(s: String) = new aws.AttributeValue(s)

  "Generic AWS SDK bindings should be able to" >> {
    "do full cycle codec on nested case classes" >> {

      case class Inner(j: String)
      case class Nested(i: Int, p: Inner)
      case class DynamoItem(n: Nested, hashKey: String, rangeKey: String)

      val original = DynamoItem(Nested(3, Inner("hello")), "2", "2")
      val i = for {
        encoded <- AwsSdkBindings.encode[DynamoItem](original)
      } yield {
        dynamoDB.putItem(encoded.withTableName(tableName))
      }
      i must beRight
      val hk = "2"
      val rk = "2"
      val keys = Map("hashKey" -> attr(hk), "rangeKey" -> attr(rk))

      val req = new GetItemRequest(tableName, keys.asJava, true)
      val resp = dynamoDB.getItem(req)
      val decoded = AwsSdkBindings.decode[DynamoItem](resp)
      decoded must beRight(original)
    }
    "read hand-written data" >> {

      dynamoDB.putItem(
        tableName,
        Map(
          "hashKey" -> new aws.AttributeValue("1"),
          "rangeKey" -> new aws.AttributeValue().withS("2"),
          "numList" -> new aws.AttributeValue().withL(
            new aws.AttributeValue().withN("10"),
            new aws.AttributeValue().withN("20"),
            new aws.AttributeValue().withN("30"))
        ).asJava
      )

      val keys = Map("hashKey" -> new aws.AttributeValue("1"),
                     "rangeKey" -> new aws.AttributeValue().withS("2"))
      val req = new GetItemRequest(tableName, keys.asJava, true)
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
        dynamoDB.putItem(encoded.withTableName(tableName))
      }
      i must beRight

      val req = new GetItemRequest(tableName, keys.asJava, true)
      val resp = dynamoDB.getItem(req)

      val expected =
        Map(
          "rangeKey" -> attr().withS("23"),
          "hashKey" -> attr().withS("23"),
          "n" -> attr()
            .addMEntry("i", attr().withN("10"))
            .addMEntry("p", attr().addMEntry("j", attr().withS("hello")))
        )
      resp.getItem.asScala must_== expected
    }
  }
}
