import shapeless._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object JavaRecordGeneric {
  implicit def javaRecordGeneric[A]: Generic[A] =
    macro JavaRecordGeneric.genericImpl[A]

  implicit def javaRecordSymbolicLabelling[A]: DefaultSymbolicLabelling[A] =
    macro JavaRecordGeneric.defaultSymbolicLabellingImpl[A]
}

class JavaRecordGeneric(val c: whitebox.Context) extends shapeless.CaseClassMacros with shapeless.SingletonTypeUtils {

  def defaultSymbolicLabellingImpl[A](implicit tTag: c.WeakTypeTag[A]): c.Tree = {
    import c.universe._
    val tpe = weakTypeOf[A]
    val name = tpe.toString
    val clazz = Class.forName(name)

    if (clazz.isRecord) {
      val labels = clazz.getRecordComponents.map(_.getName).toList
      val labelTypes = labels.map(SingletonSymbolType(_))
      val labelValues = labels.map(mkSingletonSymbol)
      val labelsType = mkHListTpe(labelTypes)
      val labelsValue = mkHListValue(labelValues)

      q"""
      (new _root_.shapeless.DefaultSymbolicLabelling[$tpe] {
        override type Out = $labelsType
        override def apply(): $labelsType = $labelsValue
      }) : _root_.shapeless.DefaultSymbolicLabelling.Aux[$tpe, $labelsType]
    """
    } else {
      c.error(c.enclosingPosition, s"$name is not record")
      q"_root_.scala.Predef.???"
    }
  }

  def genericImpl[A: c.WeakTypeTag]: c.Tree = {
    import c.universe._

    case class Field(name: String, method: MethodSymbol)

    val tpe = weakTypeOf[A]
    val name = tpe.toString
    val clazz = Class.forName(name)

    if (clazz.isRecord) {
      val methodsList = tpe.decls.collect {
        case sym: MethodSymbol if sym.isMethod && sym.isPublic && sym.paramLists.forall(_.isEmpty) =>
          sym.name.toString -> sym
      }.toList
      val methods = methodsList.toMap
      assert(methodsList.size == methods.size, methodsList)

      val recordComponents = clazz.getRecordComponents.toList

      val fields = recordComponents.map { c =>
        val fieldName = c.getName
        val m = methods.getOrElse(fieldName, sys.error(s"not found $fieldName in ${methods.keys.toList}"))
        Field(fieldName, m)
      }

      val reprType = mkHListTypTree(fields.map(_.method.typeSignatureIn(tpe).finalResultType))

      val toParamName, fromParamName = TermName(c.freshName("a"))

      val toImpl = fields.reverseIterator.foldLeft(q"_root_.shapeless.HNil": Tree) { case (acc, field) =>
        val t = Select(Ident(toParamName), TermName(field.name))
        q"$t :: $acc"
      }

      val fromImpl = {
        val fieldTrees: List[Tree] = fields.zipWithIndex.map { case (f, i) =>
          q"$fromParamName($i)"
        }

        q"new $tpe(..${fieldTrees})"
      }

      q"""
        (new _root_.shapeless.Generic[$tpe] {
          override type Repr = $reprType
          override def to($toParamName: $tpe) = $toImpl
          override def from($fromParamName: Repr) = $fromImpl
        }): _root_.shapeless.Generic.Aux[$tpe, $reprType]
      """
    } else {
      c.error(c.enclosingPosition, s"$name is not record")
      q"_root_.scala.Predef.???"
    }
  }
}
