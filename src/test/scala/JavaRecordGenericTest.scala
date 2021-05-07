package example

import java.util.{List => JavaList}
import shapeless3.deriving.K0.*
import org.junit.Test
import scala.quoted.*
import scala.jdk.OptionConverters._

object JavaRecordGenericTest {
  private def typed[A](a: A): Unit = ()

  transparent inline def javaInstance[A]: deriving.Mirror.Sum = ${ javaInstanceImpl[A] }
  transparent inline given javaInstanceGiven[A]: deriving.Mirror.Sum = ${ javaInstanceImpl[A] }

  def javaInstanceImpl[A](using a: Type[A], q: Quotes): Expr[deriving.Mirror.Sum] = {
    import q.reflect._
    val aa = q.reflect.TypeRepr.of[A]
    val name = aa.show
    val clazz = Class.forName(name)
    val subClasses = clazz.getPermittedSubclasses.flatMap(_.describeConstable.toScala).map{ desc =>
      desc.packageName + "." + desc.displayName
    }.toList

    println(subClasses)

    val x = aa.typeSymbol.fullName
    println(x)
    def c(s: String) = ConstantType(StringConstant(s)).asType

    c(name.split('.').last) match {
      case '[t] =>
        '{
          new scala.deriving.Mirror.Sum {
            override def ordinal(p: t): Int = ???
            override type MirroredMonoType = t
            override type MirroredElemLabels = EmptyTuple
          }
        }
    }
  }

}

