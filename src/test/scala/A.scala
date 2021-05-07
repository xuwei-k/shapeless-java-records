package example

import java.util.{List => JavaList}
import shapeless3.deriving.K0
import shapeless3.deriving.K0.given
import org.junit.Test
import scala.quoted.Quotes

trait Eq[A] {
  def equal(a1: A, a2: A): Boolean
}

object Eq {
  implicit def intInstance: Eq[Int] = _ == _
  implicit def stringInstance: Eq[String] = _ == _
  implicit def booleanInstance: Eq[Boolean] = _ == _
  implicit def longInstance: Eq[Long] = _ == _
  implicit def javaListInstance[A](using a: Eq[A]): Eq[JavaList[A]] = (a1, a2) => {
    import scala.jdk.CollectionConverters._
    (a1.size == a2.size) && (a1.asScala, a2.asScala).zipped.forall(a.equal(_, _))
  }
}

class A {
  import JavaRecordGenericTest._
  import JavaRecordGenericTest.given

  @Test
  def sealedTest1: Unit = {
    //val a1 = new foo.A(2, "a", JavaList.of("b", "c"))
    val a1: foo.A = new foo.A(2, "a") //, JavaList.of("b", "c"))

    javaRecord[foo.A]

    val t: K0.CoproductGeneric[foo.Base] = javaInstance[foo.Base]

    val x = summon[K0.Generic[foo.Base]]
    val e: K0.Instances[Eq, foo.Base] = summon[K0.Instances[Eq, foo.Base]]

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
