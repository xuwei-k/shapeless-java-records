package shapeless

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.typer.Synthesizer
import dotty.tools.dotc.transform.SyntheticMembers
import java.lang.constant.ClassDesc
import scala.deriving.Mirror
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.quoted.Type
import scala.jdk.OptionConverters._
import scala.quoted.runtime.impl.QuotesImpl

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

  def javaRecordImpl[A](using a: Type[A], q: Quotes) = {
    import q.reflect.*
    val name = TypeRepr.of[A].show
    val clazz = Class.forName(name)
    val components = clazz.getRecordComponents.toList
    val tupleClass = TypeRepr.typeConstructorOf(Class.forName("scala.Tuple" + components.size.toString))
    // TODO use `getGenericType`
    val tupleApplied = tupleClass.appliedTo(components.map(x => TypeRepr.typeConstructorOf(x.getType))).asType
    val Array(constructor) = clazz.getConstructors
    if (false) {
      val quotesImpl = q.asInstanceOf[QuotesImpl]
      implicit val ctx: Context = quotesImpl.ctx
      val s = new Synthesizer(ctx.typer)
      val Some(anonymousMirror) = classOf[Synthesizer].getDeclaredMethods.find(_.getName == "anonymousMirror")
      anonymousMirror.setAccessible(true)
      val span = TypeTree.of[A].asInstanceOf[tpd.Tree].span
      println(anonymousMirror)
      //    println((s, TypeRepr.of[A], SyntheticMembers.ExtendsSumMirror, span, ctx))
      println(List(s, TypeRepr.of[A], SyntheticMembers.ExtendsSumMirror, span, ctx).map(_.getClass))
      // https://github.com/lampepfl/dotty/blob/3.1.3-RC2/compiler/src/dotty/tools/dotc/typer/Synthesizer.scala#L224
      val tree =
        anonymousMirror
          .invoke(s, TypeRepr.of[A], SyntheticMembers.ExtendsSumMirror, span.coords, ctx)
          .asInstanceOf[Tree]
      tree.asExprOf[Mirror.ProductOf[A]]
    }

    val expr = tupleApplied match {
      case '[elems] =>
        println(TypeRepr.of[elems])

        '{
          new scala.deriving.Mirror.Product {
            override def fromProduct(p: Product) = {
              // constructor.newInstance(p.productIterator.toArray: _*).asInstanceOf[MirroredMonoType]
              ???
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

    expr
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
