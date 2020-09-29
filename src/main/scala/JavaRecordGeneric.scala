import shapeless._

import scala.language.experimental.macros
import scala.reflect.macros.whitebox

object JavaRecordGeneric {
  def apply[A]: Generic[A] = macro JavaRecordGeneric.impl[A]
}

class JavaRecordGeneric(val c: whitebox.Context) extends shapeless.CaseClassMacros {
  def impl[A: c.WeakTypeTag]: c.Tree = {
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
