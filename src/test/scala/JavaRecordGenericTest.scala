import java.util.List
import org.junit.Test

class JavaRecordGenericTest {
  @Test
  def test1: Unit = {
    val gen = JavaRecordGeneric[foo.A]
    val a1 = new foo.A(2, "a", List.of("b", "c"))
    val list = gen.to(a1)
    println(list)
    val a2 = gen.from(list)
    println(a2)
    assert(a1 == a2)
  }
}
