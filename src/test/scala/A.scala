package example

import java.util.{List => JavaList}
import shapeless3.deriving.K0
import shapeless3.deriving.K0.*
import org.junit.Test
import scala.quoted.Quotes

trait Eq[A] {
  def equal(a1: A, a2: A): Boolean
}

object Eq {
  implicit def aInstance: Eq[foo.A] = _ equals _
  implicit def bInstance: Eq[foo.B] = _ equals _
}

class A {
  import JavaRecordGenericTest._
  import JavaRecordGenericTest.given

  @Test
  def sealedTest1: Unit = {
    val a1 = new foo.A(2, "a", JavaList.of("b", "c"))

    val t: K0.CoproductGeneric[foo.Base] = javaInstance[foo.Base]

//    javaInstance[foo.Base] : K0.CoproductGeneric[foo.Base]

    val x = summon[K0.CoproductGeneric[foo.Base]]
    val e: K0.CoproductInstances[example.Eq, foo.Base] = summon[K0.Instances[Eq, foo.Base]]
    val fooInstance = e.map(a1) {
      [t] => (eqInstance: Eq[t], value: t) => value
    }

/*
    val b1 = new foo.B(true, Long.MaxValue)
    val coproductA = gen.to(a1)
    val coproductB = gen.to(b1)
*/
  }

}
