package example

import java.util.{List => JavaList}
import org.junit.Test
import shapeless.:+:
import shapeless.Generic
import shapeless.LabelledGeneric
import shapeless.JavaRecordGeneric

class JavaRecordGenericTest {
  private val a1 = new foo.A(2, "a", JavaList.of("b", "c"))

  private def typed[A](a: A): Unit = ()

  @Test
  def sealedTest1: Unit = {
    import JavaRecordGeneric._
    val gen = Generic[foo.Base]
    val b1 = new foo.B(true, Long.MaxValue)
    val coproductA = gen.to(a1)
    val coproductB = gen.to(b1)
    typed[foo.A :+: foo.B :+: shapeless.CNil](coproductA)
    typed[foo.A :+: foo.B :+: shapeless.CNil](coproductB)
    assert(gen.from(coproductA) == a1)
    assert(gen.from(coproductB) == b1)
  }

  @Test
  def genericTest1: Unit = {
    import JavaRecordGeneric._
    val gen = Generic[foo.A]
    val list = gen.to(a1)
    assert(list(0) == 2)
    assert(list(1) == "a")
    assert(list(2) == JavaList.of("b", "c"))
    println(list)
    val a2 = gen.from(list)
    println(a2)
    assert(a1 == a2)
  }

  @Test
  def labelledGenericSymbolTest: Unit = {
    val labelledGen = {
      import JavaRecordGeneric._
      import JavaRecordGeneric.symbol._
      LabelledGeneric[foo.A]
    }
    val record = labelledGen.to(a1)
    import shapeless.record._
    assert(record(Symbol("x")) == 2)
    assert(record(Symbol("y")) == "a")
    assert(record(Symbol("z")) == JavaList.of("b", "c"))
    val a2 = labelledGen.from(record)
    assert(a1 == a2)
  }

  @Test
  def labelledGenericStringTest: Unit = {
    val labelledGen = {
      import JavaRecordGeneric._
      import JavaRecordGeneric.string._
      LabelledGeneric[foo.A]
    }
    val record = labelledGen.to(a1)
    import shapeless.record._
    assert(record("x") == 2)
    assert(record("y") == "a")
    assert(record("z") == JavaList.of("b", "c"))
    val a2 = labelledGen.from(record)
    assert(a1 == a2)
  }
}
