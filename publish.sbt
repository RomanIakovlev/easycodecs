import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  ReleaseStep(action = Command.process("publishSigned", _), enableCrossBuild = true),
  setNextVersion,
  commitNextVersion,
  ReleaseStep(action = Command.process("sonatypeReleaseAll", _), enableCrossBuild = true),
  pushChanges
)

releasePublishArtifactsAction := PgpKeys.publishSigned.value

pomExtra in Global := {
  <url>https://github.com/RomanIakovlev/easycodecs</url>
    <licenses>
      <license>
        <name>Apache License 2.0</name>
        <url>https://www.apache.org/licenses/LICENSE-2.0</url>
      </license>
    </licenses>
    <scm>
      <connection>scm:git:github.com/RomanIakovlev/easycodecs.git</connection>
      <developerConnection>scm:git:git@github.com:RomanIakovlev/easycodecs.git</developerConnection>
      <url>github.com/RomanIakovlev/easycodecs</url>
    </scm>
    <developers>
      <developer>
        <id>Roman Iakovlev</id>
        <name>Roman Iakovlev</name>
        <url>http://github.com/RomanIakovlev</url>
      </developer>
    </developers>
}
