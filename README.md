#Codecs for recursive map-like data types

This project aims to give developers tools to easily derive typeclasses for encoding and decoding Scala classes to and from recursive map-like data types.

##Motivation

Converting from application-specific classes (e.g. model layer) to an external data structure and back is a frequent task programmer faces. In Scala such conversion is typically done via typeclasses, which you can either write by hand or, in certain cases, derive automatically.

However, when one tries to set up automatic typeclasses derivation for encoding and decoding to external data structures, Scala starts to show its quirky character. Coming up with a reliable and feature-rich generic typeclasses derivation solution is definitely possible, but can be quite intimidating, especially (but not only) for beginners.

On the other hand, recursive map-like data types are quite numerous. Here are some examples of them:

* JSON
* NoSQL database records (e.g. DynamoDB, Cassandra)
* Configuration files (e.g. Typesafe config)
* etc.

Essentially, it's a data type that can be represented as: 

```scala
type Data = Map[Key, Data]
```

Despite the similar meta-structure, the actual structure of the types used to represent JSON is very different from Cassandra records. Therefore it's not possible to create a single typeclass supporting conversion of Scala classes into all of such data structures and back. However it's possible to define a typeclass working with an abstract map-like recursive data type, and delegates details of how to work with primitive types (numbers, strings, collections, etc.) in the context of a concrete map-like data type (JSON, DynamoDB record) to a different typeclass. This is exactly what this project does.

Many Scala libraries provide generic codec typeclasses for their own data types. One of the best examples is [Circe](https://circe.github.io/circe/) library, which provides very feature-rich and configurable generic codec derivation for JSON. However, there are still many libraries providing recursive map-like data structures, especially rooted in Java world, which don't offer such desirable capabilities. This project aims to enable developers to fill those gaps with much less efforts than writing full-blown generic typeclass derivation themselves.

##Implementation details

The core typeclasses supporting the mapping between Scala classes and recursive map-like data types are `Encoder` and `Decoder` (collectively as codecs). `Encoder` converts a Scala class instance to data item, and `Decoder` converts a data item into a Scala class instance. The other typeclasses supporting concrete data types are called `PrimitivesReader` and `PrimitivesWriter`. `PrimitivesReader` can read Scala primitive values (e.g. `Int`, `String`, collections) from a single data item. `PrimitivesWriter` can create a single data item from a Scala primitive value.

Such approach have the following benefits:

* All the heavy lifting is done once, in the `Encoder` and `Decoder` typeclasses. No need to have different codecs for e.g. DynamoDB and Cassandra.
* By writing new `PrimitivesReader` and `PrimitivesWriter` instances, one can add support for new map-like data types.

You can find the definitions of typeclasses used in this project below:

`Encnoder`:
```scala
trait Encoder[A, B] {
  def encode(a: A): Either[EncodingError, Map[String, B]]
}
```
`Decoder`:
```scala
trait Decoder[A, B] {
  def decode(a: Map[String, A]): Either[DecodingError, B]
}
```
`PrimitivesReader`: 

```scala
trait PrimitivesReader[A, B] {
  def extract(a: A): Either[DecodingError, B]
}
```
`PrimitivesWriter`:
```scala
trait PrimitivesWriter[A, B] {
  def write(a: A): B
}
```

Codecs can be derived for the following Scala types:

* case classes (and certain non-case classes)
* numbers (`Int`, `Long`, `Float`, `Double`, `BigDecimal`)
* `String`
* Collections (`Iterable`-based) of supported types
* Maps with `String` key and values of supported types
* Options of supported types
* Sealed trait families
    * Enum-like sealed trait families

The generic derivation of `Encoder` and `Decoder` is implemented using pure [shapeless](https://github.com/milessabin/shapeless), with no custom macros.

##Project structure
This project is a multi-project SBT build with the following structure: there is a `core` project, where all typeclasses are defined. `Encoder` and `Decoder` are also implemented there. Then there are additional projects providing implementations of `PrimitivesReader` and `PrimitivesWriter` for concrete data types (e.g. DynamoDB's `AttributeValue`). Those additional projects depend on `core` for `PrimitivesReader` and `PrimitivesWriter` definitions.

Currently, one data type is supported, namely DynamoDB's [AttributeValue](http://docs.aws.amazon.com/AWSJavaSDK/latest/javadoc/com/amazonaws/services/dynamodbv2/model/AttributeValue.html) from AWS Java SDK v1.x.

##Project status

This project has started as an attempt to learn how to write generic codec derivation for DynamoDB records. However, during implementation I've understood this problem can be generalized, and the project acquired its current scope. I personally feel excited about opportunities this approach provides, and I hope others will feel the same. However, I'm aware of the [law of the leaky abstractions](https://www.joelonsoftware.com/2002/11/11/the-law-of-leaky-abstractions/), and therefore I'm sure further usage of this approach, especially by someone other than me, will uncover its weaknesses. 

Therefore, please use codecs from this project, submit support for new codecs (see [Contributing](../CONTRIBUTING)), and share your experience. 


##License
Apache License 2.0
