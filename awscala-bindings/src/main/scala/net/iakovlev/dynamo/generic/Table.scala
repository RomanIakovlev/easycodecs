package net.iakovlev.dynamo.generic

//import com.amazonaws.services.dynamodbv2.model.GetItemRequest
import net.iakovlev.dynamo.generic.LensAndType.Aux
import shapeless.tag.@@
import shapeless.{HList, HNil, Lens, Path, Select, ^, lens, ::}

trait TableBuilder[A] {

  def apply(expr: Symbol): Table[A]
  def apply(exprH: Symbol, exprR: Symbol): Table[A]
}

object TableBuilder {
  /*def apply[A] = new TableBuilder[A] {
    override def apply(expr: Symbol): Table[A] = {
      //val h = lens[A] >> expr
      new Table[A] {
        override def getRequest(hashKey: HashKey[A]): GetItemRequest = ???

        override def getRequest(hashKey: HashKey[A],
                                rangeKey: RangeKey[A]): GetItemRequest = ???
      }
    }

    override def apply(exprH: Symbol, exprR: Symbol): Table[A] = {
      new Table[A] {
        override def getRequest(hashKey: HashKey[A]): GetItemRequest = ???

        override def getRequest(hashKey: HashKey[A],
                                rangeKey: RangeKey[A]): GetItemRequest = ???
      }
    }
  }*/
}
trait LensAndType[T] {
  type P
  val l: Lens[T, P]
}
object LensAndType {
  type Aux[T, P0] = LensAndType[T] { type P = P0 }
  class Helper[T] {
    def apply[E, L <: HList](path: Path[L])(
        implicit mkLens: path.Lens[T, E]): LensAndType.Aux[T, E] =
      new LensAndType[T] {
        type P = E
        val l = mkLens()
      }
  }
  def create[T] = {
    new Helper[T]
  }
}

trait KeyHelper[A] {
  def lensAndType[E]: LensAndType.Aux[A, E]
}

trait HashKey[A] extends KeyHelper[A]
trait RangeKey[A] extends KeyHelper[A]

object KeyHelper {}

/*object HashKey {
  def apply[A](s: Symbol) = new HashKey[A] {
    override def lensAndType[E]: LensAndType.Aux[A, E] =
      LensAndType.create[A](Path[s.type :: HNil])
  }
}*/

/*object RangeKey {
  def apply[A, L <: HList](p: Path[L]) = new RangeKey[A] {
    override def lensAndType[E]: LensAndType.Aux[A, E] =
      LensAndType.create[A](^.i)
  }
}*/

trait Table[A] {

  case class Foo(i: Int, s: String)
  val v: LensAndType.Aux[Foo, Int] = LensAndType.create[Foo](^.i)
  val m: v.P = 1

  //def getRequest(hashKey: HashKey[A]): GetItemRequest
  //def getRequest(hashKey: HashKey[A], rangeKey: RangeKey[A]): GetItemRequest
}
