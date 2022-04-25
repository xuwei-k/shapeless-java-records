package example

import java.util.{List => JavaList}
import org.junit.Test
import scala.deriving.Mirror.ProductOf
import shapeless.JavaRecordGeneric

class JavaRecordGenericTest {
  private val a1 = new foo.A(2, "a", JavaList.of("b", "c"))

  private def typed[A](a: A): Unit = ()

  @Test
  def sealedTest1: Unit = {
    val a = JavaRecordGeneric.javaRecordMirror[foo.A]
    val a2 = a.fromProduct((3, "b", JavaList.of("x1", "x2")))
    println(a2)
    assert(a2 == new foo.A(3, "b", JavaList.of("x1", "x2")))
  }

}
