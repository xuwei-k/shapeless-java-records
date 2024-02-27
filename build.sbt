import ReleaseTransformations._

javacOptions ++= Seq("--enable-preview", "--release", scala.util.Properties.javaSpecVersion)

organization := "com.github.xuwei-k"
name := "shapeless-java-records"

val Scala213 = "2.13.13"

scalaVersion := Scala213

crossScalaVersions := Seq(Scala213, "2.12.19")

libraryDependencies ++= {
  if (scalaBinaryVersion.value == "3") {
    Nil
  } else {
    Seq(
      "com.chuusai" %% "shapeless" % "2.3.10",
      scalaOrganization.value % "scala-reflect" % scalaVersion.value,
    )
  }
}

libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.3" % "test"

// https://github.com/scala/bug/issues/11908
compileOrder := CompileOrder.JavaThenScala

scalacOptions ++= Seq(
  "-deprecation",
  "-Xsource:3",
)

homepage := Some(url("https://github.com/xuwei-k/shapeless-java-records"))

licenses := Seq(
  "MIT License" -> url("https://raw.githubusercontent.com/xuwei-k/shapeless-java-records/main/LICENSE.txt"),
)

description := "derive shapeless.Generic instances for Java Records and Sealed Classes"

pomExtra := (
  <developers>
    <developer>
      <id>xuwei-k</id>
      <name>Kenji Yoshida</name>
      <url>https://github.com/xuwei-k</url>
    </developer>
  </developers>
  <scm>
    <url>git@github.com:xuwei-k/shapeless-java-records.git</url>
    <connection>scm:git:git@github.com:xuwei-k/shapeless-java-records.git</connection>
  </scm>
)

publishTo := sonatypePublishToBundle.value

releaseCrossBuild := true
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  releaseStepCommandAndRemaining("sonatypeBundleRelease"),
  setNextVersion,
  commitNextVersion,
  pushChanges,
)

(Compile / doc / scalacOptions) ++= {
  val hash = sys.process.Process("git rev-parse HEAD").lineStream_!.head
  val base = (LocalRootProject / baseDirectory).value.getAbsolutePath
  Seq(
    "-sourcepath",
    base,
    "-doc-source-url",
    "https://github.com/xuwei-k/shapeless-java-records/tree/" + hash + "€{FILE_PATH}.scala"
  )
}
