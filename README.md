# shapeless java records


## setup

- install jdk 15 or later
- `build.sbt`

```scala
javacOptions ++= Seq("--enable-preview", "--release", "15")

scalaVersion := // 2.12.x or 2.13.x

libraryDependencies += "com.github.xuwei-k" %% "shapeless-java-records" % "latest version"

// https://github.com/scala/bug/issues/11908
// https://github.com/scala/bug/issues/12159
compileOrder := CompileOrder.JavaThenScala
```

- run `sbt -J--enable-preview`


## usage

```scala
import shapeless._
import shapeless.JavaRecordGeneric._

// summon shapeless.Generic instances
Generic[YourJavaRecordType]
Generic[YourJavaSealedType]
```

### LabelledGeneric

```scala
import shapeless._
import shapeless.JavaRecordGeneric._
import shapeless.JavaRecordGeneric.string._ // string label
// or import shapeless.JavaRecordGeneric.symbol._

LabelledGeneric[YourJavaRecordType]
```
