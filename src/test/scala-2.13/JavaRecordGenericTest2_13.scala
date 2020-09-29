import java.util.{List => JavaList}
import org.junit.Test
import shapeless.LabelledGeneric

class JavaRecordGenericTest2_13 {
  private val a1 = new foo.A(2, "a", JavaList.of("b", "c"))

  // TODO support Scala 2.12 ?
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
