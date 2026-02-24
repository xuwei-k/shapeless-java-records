# shapeless java records

[![scaladoc](https://javadoc.io/badge2/com.github.xuwei-k/shapeless-java-records_2.13/javadoc.svg)](https://javadoc.io/doc/com.github.xuwei-k/shapeless-java-records_2.13)

## setup

- install jdk 17 or later
- `build.sbt`

```scala
scalaVersion := // 2.12.x or 2.13.x

libraryDependencies += "com.github.xuwei-k" %% "shapeless-java-records" % "latest version"

// if old scala version
// https://github.com/scala/bug/issues/11908
// https://github.com/scala/bug/issues/12159
compileOrder := CompileOrder.JavaThenScala
```

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
LabelledGeneric[YourJavaSealedType]
```
