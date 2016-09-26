package net.iakovlev.dynamo.generic

import org.specs2.mutable.Specification

case class C(a: String, b: Int)
object C {
  implicit val pk = FirstFieldPrimaryKey[C]
}
class PrimaryKeyTest extends Specification {
  "Primary key derivation facility should" >> {
    "Derive default primary key as first field of class" >> {

      val key = PrimaryKey[C, FirstFieldPrimaryKey[C]]
      key.value must_== "a"

      // just a bunch of different ways to test the same fact
      // 1. Compare classes at runtime using reflection
      classOf[key.KeyType] must_== classOf[String]
      // 2. Define a function with the derived type and see if it compiles
      def t(p: key.KeyType, pp: String): Boolean = {p === pp}
      t("a", "a") must_== true
    }
  }
}
