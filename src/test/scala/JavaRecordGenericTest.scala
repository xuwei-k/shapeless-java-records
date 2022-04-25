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
  }

}
