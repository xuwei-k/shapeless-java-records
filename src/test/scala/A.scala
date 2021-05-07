package example

import java.util.{List => JavaList}
import shapeless3.deriving.K0.*
import org.junit.Test
import scala.quoted.Quotes

class A {
  import JavaRecordGenericTest._

  @Test
  def sealedTest1: Unit = {
    val a1 = new foo.A(2, "a", JavaList.of("b", "c"))

    val t: scala.deriving.Mirror.Sum{
      type MirroredMonoType = "Base"
//      type MirroredLabel = "Base"
      type MirroredElemLabels = EmptyTuple //("A", "B")
    } = javaInstance[foo.Base]


/*
    val b1 = new foo.B(true, Long.MaxValue)
    val coproductA = gen.to(a1)
    val coproductB = gen.to(b1)
*/
  }

}
