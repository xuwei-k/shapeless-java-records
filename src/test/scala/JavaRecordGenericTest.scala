package example

import java.util.{List => JavaList}
import shapeless3.deriving.K0
import scala.quoted.*
import scala.jdk.OptionConverters._

object JavaRecordGenericTest {
  transparent inline def javaInstance[A]: K0.CoproductGeneric[A] = ${ javaInstanceImpl[A] }
  transparent inline given javaInstanceGiven[A]: K0.CoproductGeneric[A] = ${ javaInstanceImpl[A] }

  def javaInstanceImpl[A](using a: Type[A], q: Quotes) = {
    import q.reflect._
    val name = TypeRepr.of[A].show
    val clazz = Class.forName(name)
    val subClasses = clazz.getPermittedSubclasses.flatMap(_.describeConstable.toScala).map{ desc =>
      desc.packageName + "." + desc.displayName
    }.toList
    val tupleClass = TypeRepr.typeConstructorOf(Class.forName("scala.Tuple" + subClasses.size.toString))
    val tupleApplied = tupleClass.appliedTo(subClasses.map(x => TypeRepr.typeConstructorOf(Class.forName(x)))).asType

    tupleApplied match {
      case '[elems] =>
        '{
          new scala.deriving.Mirror.Sum {
            def ordinal(p: MirroredMonoType): Int = 0
            type Kind = K0.type
            type MirroredType = A
            type MirroredElemTypes = elems
          } : K0.CoproductGeneric[A]  {
            type MirroredElemTypes = elems
          }
        }
    }
  }
}

