javacOptions ++= Seq("--enable-preview", "--release", "15")

val Scala213 = "2.13.3"

scalaVersion := Scala213

crossScalaVersions := Seq(Scala213, "2.12.12")

libraryDependencies += "com.chuusai" %% "shapeless" % "2.3.3"
libraryDependencies += scalaOrganization.value % "scala-reflect" % scalaVersion.value
libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test"

// https://github.com/scala/bug/issues/11908
compileOrder := CompileOrder.JavaThenScala

scalacOptions += "-deprecation"
