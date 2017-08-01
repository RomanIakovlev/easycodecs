resolvers in ThisBuild += Resolver.bintrayRepo("tek", "maven")
val commonSettings =
  Seq(
    organization := "net.iakovlev",
    scalaVersion := "2.11.8",
    scalacOptions in Test ++= Seq("-Yrangepos"),
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-core" % "3.8.6" % "test",
      compilerPlugin("tryp" %% "splain" % "0.1.22"))
  )

lazy val core = (project in file("."))
  .settings(commonSettings)
  .settings(
    name := "dynamo-generic-core",
    //scalacOptions ++= Seq("-Xlog-implicits"),
    libraryDependencies ++= Seq("com.chuusai" %% "shapeless" % "2.3.2",
                                "org.typelevel" %% "cats" % "0.8.1")
  )
lazy val awscala_bindings =
  (project in file("awscala-bindings"))
    .settings(commonSettings)
    .dependsOn(core)
    .settings(
      name := "dynamo-generic-awscala",
      libraryDependencies ++= Seq(
        "com.amazonaws" % "aws-java-sdk-dynamodb" % "1.11.171")
    )
import scala.concurrent.duration._
lazy val aws_sdk_bindings =
  (project in file("aws-sdk-bindings"))
    .settings(commonSettings)
    .dependsOn(core)
    .configs(IntegrationTest)
    .settings(
      inConfig(IntegrationTest)(baseDynamoDBSettings),
      //scalacOptions ++= Seq("-Xlog-implicits"),
      Defaults.itSettings,
      name := "Dynamo Java SDK bindings",
      libraryDependencies ++= Seq(
        "com.amazonaws" % "aws-java-sdk-dynamodb" % "1.11.171",
        "org.typelevel" %% "cats" % "0.8.1",
        "org.specs2" %% "specs2-core" % "3.8.6" % "test, it"),
      startDynamoDBLocal in IntegrationTest := startDynamoDBLocal.dependsOn(compile in IntegrationTest).value,
      test in IntegrationTest := (test in IntegrationTest).dependsOn(startDynamoDBLocal).value,
      testOnly in IntegrationTest := (testOnly in IntegrationTest)
        .dependsOn(startDynamoDBLocal)
        .value,
      testOptions in IntegrationTest += dynamoDBLocalTestCleanup.value,
      dynamoDBLocalDownloadIfOlderThan := 100.days
    )
