package net.iakovlev.easycodecs

import shapeless._
import shapeless.labelled._

/**
  * Credit goes to Travis Brown, see http://stackoverflow.com/a/37015184/371804
  */
trait IsEnum[C <: Coproduct] {
  def to(c: C): String
  def from(s: String): Option[C]
}

object IsEnum {
  implicit val cNilIsEnum: IsEnum[CNil] = new IsEnum[CNil] {
    def to(c: CNil): String = sys.error("Impossible")
    def from(s: String): Option[CNil] = None
  }

  implicit def cConsIsEnum[K <: Symbol, H <: Product, T <: Coproduct](
      implicit witK: Witness.Aux[K],
      witH: Witness.Aux[H],
      gen: Generic.Aux[H, HNil],
      tie: IsEnum[T]): IsEnum[FieldType[K, H] :+: T] =
    new IsEnum[FieldType[K, H] :+: T] {
      def to(c: FieldType[K, H] :+: T): String = c match {
        case Inl(h) => witK.value.name
        case Inr(t) => tie.to(t)
      }
      def from(s: String): Option[FieldType[K, H] :+: T] =
        if (s == witK.value.name) Some(Inl(field[K](witH.value)))
        else tie.from(s).map(Inr(_))
    }
}
