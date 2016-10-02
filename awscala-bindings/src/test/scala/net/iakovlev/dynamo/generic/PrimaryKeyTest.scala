package net.iakovlev.dynamo.generic

import org.specs2.mutable.Specification

class PrimaryKeyTest extends Specification {
  "Primary key derivation facility should" >> {
    "Derive default primary key as first field of class" >> {
      case class C(a: String, b: Int)
      val key = PrimaryKey[C, FirstFieldPrimaryKey[C]]
      key("123") must_== Map("a" -> "123")
    }
  }
}
