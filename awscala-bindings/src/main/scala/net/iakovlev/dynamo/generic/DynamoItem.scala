package net.iakovlev.dynamo.generic

import com.amazonaws.services.dynamodbv2.model.GetItemResult
import scala.collection.JavaConverters._

trait DynamoItem[A] {
  def readItem(item: GetItemResult): Option[A]
}

object DynamoItem {
  def apply[A](item: GetItemResult)(implicit decoder: Decoder[A]): Option[A] = new DynamoItem[A] {
    override def readItem(getItemResult: GetItemResult): Option[A] = {
      val attrs = getItemResult.getItem.asScala
      decoder.decode()
    }
  }
}