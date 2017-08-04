package net.iakovlev.dynamo.generic.test

import software.amazon.awssdk.services.dynamodb.model.AttributeValue

trait TestBase {
  def attr(s: String) = AttributeValue.builder().s(s).build()
  def attr() = AttributeValue.builder()
}
