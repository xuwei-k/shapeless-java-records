import java.util.{List => JavaList}
import org.junit.Test
import shapeless.Generic
import shapeless.LabelledGeneric

class JavaRecordGenericTest {
  private val a1 = new foo.A(2, "a", JavaList.of("b", "c"))

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
  def labelledGenericTest1: Unit = {
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
}
