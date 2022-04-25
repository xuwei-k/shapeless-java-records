package example

import java.util.{List => JavaList}
import org.junit.Test
import scala.deriving.Mirror.ProductOf
import shapeless.JavaRecordGeneric

class JavaRecordGenericTest {
  private val a1 = new foo.A(2, "a", JavaList.of("b", "c"))

  private def typed[A](a: A): Unit = ()

  @Test
  def test1: Unit = {
    val mirror = JavaRecordGeneric.javaRecordMirror[foo.A]
    val a2 = mirror.fromProduct((3, "b", JavaList.of("x1", "x2")))
    println(a2)
    typed[foo.A](a2)
    assert(a2 == new foo.A(3, "b", JavaList.of("x1", "x2")))
  }

  @Test
  def test2: Unit = {
    import JavaRecordGeneric.javaRecordMirror
    val mirror = JavaRecordGeneric.javaRecordMirror[foo.Base]
    val b1 = new foo.B(true, Long.MaxValue)
    summon[mirror.MirroredType =:= foo.Base]
    summon[mirror.MirroredMonoType =:= foo.Base]
    summon[mirror.MirroredElemTypes =:= (foo.A, foo.B)]
    assert(mirror.ordinal(a1) == 0)
    assert(mirror.ordinal(b1) == 1)
  }
}
