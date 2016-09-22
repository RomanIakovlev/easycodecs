val commonSettings =
  Seq(organization := "net.iakovlev",
      scalaVersion := "2.11.8",
      scalacOptions in Test ++= Seq("-Yrangepos"),
      libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.4" % "test"))

lazy val core = (project in file("."))
  .settings(commonSettings)
  .settings(name := "dynamo-generic-core",
            scalacOptions ++= Seq("-Xlog-implicits"),
            libraryDependencies ++= Seq("com.chuusai" %% "shapeless" % "2.3.2",
                                        "org.typelevel" %% "cats" % "0.7.2"))
lazy val awscala_bindings =
  (project in file("awscala-bindings"))
    .settings(commonSettings)
    .dependsOn(core)
    .settings(
      name := "dynamo-generic-awscala",
      libraryDependencies ++= Seq("com.github.seratch" %% "awscala" % "0.5.7"))
