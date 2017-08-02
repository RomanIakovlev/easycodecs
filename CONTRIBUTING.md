
##Contributing

To add support for a new data type, create a new project and implement `PrimitivesReader` and `PrimitivesWriter` instances for this new type in it. The easiest way to provide all the necessary instances is to implement `Extractors` and `Writers` traits. Use existing projects as guidance.