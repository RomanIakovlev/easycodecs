package net.iakovlev.dynamo.generic.test
import com.amazonaws.services.dynamodbv2.{AmazonDynamoDBClient, model => aws}
import com.amazonaws.services.dynamodbv2.document.DynamoDB
import com.amazonaws.services.dynamodbv2.model._
import net.iakovlev.dynamo.generic.AwsSdkBindings
import org.specs2.mutable.Specification
import scala.collection.JavaConverters._

class IntegrationTest extends Specification {
  "Generic AWS SDK bindings should be able to" >> {
    "read data from real table" >> {
      val dynamoDB = new AmazonDynamoDBClient

      val dynamoDBHelper = new DynamoDB(dynamoDB)
      val tableName = "dynamo-generic-test"

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

      dynamoDB.putItem(tableName,
        Map(
          "hashKey" -> new aws.AttributeValue("1"),
          "rangeKey" -> new aws.AttributeValue("2"),
          "numList" -> new aws.AttributeValue().withL(
            new aws.AttributeValue().withN("10"),
            new aws.AttributeValue().withN("20"),
            new aws.AttributeValue().withN("30"))).asJava)

      val keys = Map(
        "hashKey" -> new aws.AttributeValue("1"),
        "rangeKey" -> new aws.AttributeValue("2"))

      val req = new GetItemRequest("dynamo-generic-test", keys.asJava, true)

      val resp = dynamoDB.getItem(req)

      case class NumList(hashKey: String, rangeKey: String, numList: List[Double])

      val decoded = AwsSdkBindings.from[NumList](resp)

      println(decoded)

      table.delete()
      table.waitForDelete()
      println(s"Table $tableName is deleted")

      decoded must_== Some(NumList("1", "2", List(10.0, 20.0, 30.0)))

    }
  }
}
