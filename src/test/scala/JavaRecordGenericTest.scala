import java.util.{List => JavaList}
import org.junit.Test

class JavaRecordGenericTest {
  @Test
  def test1: Unit = {
    val gen = JavaRecordGeneric[foo.A]
    val a1 = new foo.A(2, "a", JavaList.of("b", "c"))
    val list = gen.to(a1)
    assert(list(0) == 2)
    assert(list(1) == "a")
    assert(list(2) == JavaList.of("b", "c"))
    println(list)
    val a2 = gen.from(list)
    println(a2)
    assert(a1 == a2)
  }
}
