package net.iakovlev.dynamo.generic

import shapeless.ops.hlist.{IsHCons, ToTraversable}
import shapeless.ops.record._
import shapeless.{Generic, HList, LabelledGeneric}

trait PrimaryKey[A] {
  type KK
  case class KeyName(value: String) {
    type KeyType = KK
  }
  def primaryKeyName: KeyName
}

object PrimaryKey {
  implicit def default[A, LabelledRepr <: HList, UnlabelledRepr <: HList, Keys <: HList, HV, TV <: HList](
      implicit lg: LabelledGeneric.Aux[A, LabelledRepr],
      g: Generic.Aux[A, UnlabelledRepr],
      keys: Keys.Aux[LabelledRepr, Keys],
      isHConsValue: IsHCons.Aux[UnlabelledRepr, HV, TV],
      ev: ToTraversable.Aux[Keys, List, Symbol]) = new PrimaryKey[A] {
    type KK = HV
    override def primaryKeyName: KeyName = {
      KeyName(keys.apply().toList.head.name)
    }
  }
  def apply[A](implicit pk: PrimaryKey[A]) = pk.primaryKeyName
}