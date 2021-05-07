package example

import java.util.{List => JavaList}
import shapeless3.deriving.K0
import scala.quoted.*
import scala.jdk.OptionConverters._

object JavaRecordGenericTest {

  transparent inline given javaRecordSealed[A]: K0.Generic[A] = ${ impl[A] }

  transparent inline def javaRecord[A]: K0.ProductGeneric[A] = ${ javaRecordImpl[A] }

  def impl[A](using a: Type[A], q: Quotes) = {
    import q.reflect._
    val name = TypeRepr.of[A].show
    val clazz = Class.forName(name)
    if(clazz.isRecord) {
      javaRecordImpl[A]
    } else if (clazz.isSealed) {
      javaSealedImpl[A]
    } else {
      sys.error(s"${name} is neither record nor sealed")
    }
  }

  def javaRecordImpl[A](using a: Type[A], q: Quotes) = {
    import q.reflect._
    val name = TypeRepr.of[A].show
    val clazz = Class.forName(name)
    val components = clazz.getRecordComponents.toList
    val tupleClass = TypeRepr.typeConstructorOf(Class.forName("scala.Tuple" + components.size.toString))

    // TODO use `getGenericType`
    val tupleApplied = tupleClass.appliedTo(components.map(x => TypeRepr.typeConstructorOf(x.getType))).asType

    tupleApplied match {
      case '[elems] =>
        '{
          new scala.deriving.Mirror.Product {
            override def fromProduct(p: Product) = ???
            type Kind = K0.type
            type MirroredType = A
            type MirroredElemTypes = elems
          } : K0.ProductGeneric[A]  {
            type MirroredElemTypes = elems
          }
        }
    }
  }

  transparent inline def javaInstance[A]: K0.CoproductGeneric[A] = ${ javaSealedImpl[A] }

  def javaSealedImpl[A](using a: Type[A], q: Quotes) = {
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
            def ordinal(p: MirroredMonoType): Int = 0 // TODO
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

