package shapeless

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.typer.Synthesizer
import dotty.tools.dotc.transform.SyntheticMembers
import java.lang.constant.ClassDesc
import java.lang.reflect.ParameterizedType
import scala.deriving.Mirror
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import scala.jdk.OptionConverters._
import scala.quoted.runtime.impl.QuotesImpl

final case class Parameterized(base: Class[?], params: List[Parameterized]) {
  def asScalaType(using q: Quotes): q.reflect.TypeRepr = {
    val x = q.reflect.TypeRepr.typeConstructorOf(base)
    if (params.isEmpty) {
      x
    } else {
      x.appliedTo(params.map(_.asScalaType))
    }
  }
}

object JavaRecordGeneric {
  transparent inline implicit def javaRecordMirror[A]: Any = ${ javaRecordMirrorImpl[A] }

  private def javaRecordMirrorImpl[A](using t: Type[A], q: Quotes) = {
    import q.reflect.*
    val name = TypeRepr.of[A].show
    val clazz = Class.forName(name)
    if (clazz.isRecord) {
      javaRecordImpl[A]
    } else if (clazz.isSealed) {
      javaSealedImpl[A]
    } else {
      report.errorAndAbort(s"${name} is neither record nor sealed")
    }
  }

  private def getParameterized(t: java.lang.reflect.Type): Parameterized = {
    t match {
      case clazz: Class[?] =>
        Parameterized(clazz, Nil)
      case p: ParameterizedType =>
        Parameterized(p.getRawType.asInstanceOf[Class[?]], p.getActualTypeArguments.map(getParameterized).toList)
      case other =>
        sys.error(s"Does not support ${other.getClass}")
    }
  }

  def javaRecordImpl[A](using a: Type[A], q: Quotes) = {
    import q.reflect.*
    val name: String = TypeRepr.of[A].show
    val clazz = Class.forName(name)
    val components = clazz.getRecordComponents.toList
    val tupleClass = TypeRepr.typeConstructorOf(Class.forName("scala.Tuple" + components.size.toString))
    val parameterized = components.map(c => getParameterized(c.getGenericType))
    val tupleApplied = tupleClass.appliedTo(parameterized.map(_.asScalaType)).asType
    val Array(constructor) = clazz.getConstructors

    tupleApplied match {
      case '[elems] =>
        // println(TypeRepr.of[elems].show)

        '{
          new Mirror.Product {
            override def fromProduct(p: Product) = {
              Class
                .forName(${ Expr(name) })
                .getConstructors
                .head
                .newInstance(p.productIterator.toArray: _*)
                .asInstanceOf[MirroredMonoType]
            }
          }.asInstanceOf[
            Mirror.Product {
              type MirroredType = A
              type MirroredMonoType = A
              type MirroredElemTypes = elems
            }
          ]
        }
    }
  }

  def javaSealedImpl[A](using a: Type[A], q: Quotes): Expr[Mirror.SumOf[A]] = {
    import q.reflect.*
    val name = TypeRepr.of[A].show
    val clazz = Class.forName(name)
    val subClasses = clazz.getPermittedSubclasses
      .flatMap(_.describeConstable.toScala)
      .map { desc =>
        desc.packageName + "." + desc.displayName
      }
      .toList
    val tupleClass = TypeRepr.typeConstructorOf(Class.forName("scala.Tuple" + subClasses.size.toString))
    val tupleApplied = tupleClass.appliedTo(subClasses.map(x => TypeRepr.typeConstructorOf(Class.forName(x)))).asType
    tupleApplied match {
      case '[elems] =>
        ???
    }
  }
}
