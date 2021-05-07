package example

import java.util.{List => JavaList}
import shapeless3.deriving.K0
import org.junit.Test
import scala.quoted.*
import scala.jdk.OptionConverters._

object JavaRecordGenericTest {
  private def typed[A](a: A): Unit = ()

  transparent inline def javaInstance[A]: K0.CoproductGeneric[A] = ${ javaInstanceImpl[A] }
  transparent inline given javaInstanceGiven[A]: K0.CoproductGeneric[A] = ${ javaInstanceImpl[A] }

  def javaInstanceImpl[A](using a: Type[A], q: Quotes) = {
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
    def c(s: String): Type[?] = ConstantType(StringConstant(s)).asType

    val util = new shapeless3.deriving.ReflectionUtils(q)


    c(name.split('.').last).match {
      case '[t] =>
        TypeRepr.typeConstructorOf(Class.forName(subClasses.head)).asType match {
          case '[a1] =>
            TypeRepr.typeConstructorOf(Class.forName(subClasses.last)).asType match {
              case '[a2] =>
               '{
                 new scala.deriving.Mirror.Sum {
                   def ordinal(p: MirroredMonoType): Int = 0
                   type Kind = K0.type
                   type MirroredType = A
                   type MirroredElemTypes = (a1, a2)
                 } : K0.CoproductGeneric[A]  {
                   type MirroredElemTypes = (a1, a2)
                 }
               }
            }
        }
    }
  }

}

