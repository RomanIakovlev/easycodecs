package net.iakovlev.dynamo.generic

import shapeless.ops.hlist.{IsHCons, ToTraversable}
import shapeless.ops.record._
import shapeless.{Generic, HList, LabelledGeneric}

trait PrimaryKey[A] {
  type KK
  protected val primaryKeyName: String
  def primaryKeyValue(value: KK): Map[String, KK]
}

object PrimaryKey {
  def apply[A, PK <: PrimaryKey[A]](implicit pk: PK): (pk.KK => Map[String, pk.KK]) =
    (a) => pk.primaryKeyValue(a)
}

trait FirstFieldPrimaryKey[A] extends PrimaryKey[A]

object FirstFieldPrimaryKey {
  implicit def default[A,
                       LabelledRepr <: HList,
                       UnlabelledRepr <: HList,
                       Keys <: HList,
                       H](
      implicit lg: LabelledGeneric.Aux[A, LabelledRepr],
      g: Generic.Aux[A, UnlabelledRepr],
      keys: Keys.Aux[LabelledRepr, Keys],
      isHC: IsHCons.Aux[UnlabelledRepr, H, _],
      ev: ToTraversable.Aux[Keys, List, Symbol]) = new FirstFieldPrimaryKey[A] {
    override type KK = H
    override val primaryKeyName: String = {
      keys.apply().toList.head.name
    }

    override def primaryKeyValue(value: H): Map[String, H] = {
      Map(primaryKeyName -> value)
    }
  }

  def apply[A](implicit pk: FirstFieldPrimaryKey[A]): FirstFieldPrimaryKey[A] = pk
}
