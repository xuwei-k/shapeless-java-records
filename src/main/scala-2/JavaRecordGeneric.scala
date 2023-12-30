package shapeless

import scala.language.experimental.macros
import scala.reflect.macros.whitebox
import java.lang.constant.ClassDesc

object JavaRecordGeneric {
  implicit def javaRecordGeneric[A]: Generic[A] =
    macro JavaRecordGeneric.genericImpl[A]

  object symbol {
    implicit def javaRecordSymbolLabel[A]: DefaultSymbolicLabelling[A] =
      macro JavaRecordGeneric.symbolImpl[A]
  }

  object string {
    implicit def javaRecordStringLabel[A]: DefaultSymbolicLabelling[A] =
      macro JavaRecordGeneric.stringImpl[A]
  }

  private sealed abstract class KeyType extends Product with Serializable
  private case object StringKey extends KeyType
  private case object SymbolKey extends KeyType

  /**
   * for JDK15 compatibility
   * [[https://github.com/openjdk/jdk/commit/637b0c64b0a5d67d31b4b61dee55e8d682790da0#diff-a6270f4b50989abe733607c69038b2036306d13f77276af005d023b7fc57f1a2R4424]]
   */
  private implicit class GetPermittedSubclassesCompat(private val self: Class[?]) extends AnyVal {
    import scala.language.reflectiveCalls
    def getPermittedSubClassDescList: Array[ClassDesc] = {
      try {
        // jdk16 or later
        self.asInstanceOf[{ def getPermittedSubclasses: Array[Class[?]] }].getPermittedSubclasses.flatMap { c =>
          val x = c.asInstanceOf[{ def describeConstable: java.util.Optional[ClassDesc] }].describeConstable
          if (x.isPresent) Some(x.get) else None
        }
      } catch {
        case e: NoSuchMethodException =>
          // jdk15
          self.asInstanceOf[{ def permittedSubclasses: Array[ClassDesc] }].permittedSubclasses
      }
    }
  }
}

class JavaRecordGeneric(val c: whitebox.Context)
    extends shapeless.CaseClassMacros
    with shapeless.SingletonTypeUtils
    with JavaRecordGenericCompat {
  import JavaRecordGeneric._
  import c.universe._

  def symbolImpl[A: c.WeakTypeTag]: c.Tree =
    defaultSymbolicLabellingImpl[A](SymbolKey)

  def stringImpl[A: c.WeakTypeTag]: c.Tree =
    defaultSymbolicLabellingImpl[A](StringKey)

  private def defaultSymbolicLabellingImpl[A](keyType: KeyType)(implicit tTag: c.WeakTypeTag[A]): c.Tree = {
    val tpe = weakTypeOf[A]
    val name = tpe.toString
    val clazz = Class.forName(name)

    if (clazz.isSealed || clazz.isRecord) {
      val labels = {
        if (clazz.isRecord) {
          clazz.getRecordComponents.map(_.getName)
        } else {
          clazz.getPermittedSubClassDescList.map(_.displayName)
        }
      }.toList
      val labelTypes = {
        keyType match {
          case StringKey =>
            labels.map(s => c.internal.constantType(Constant(s)))
          case SymbolKey =>
            labels.map(SingletonSymbolType(_))
        }
      }
      val labelValues = {
        keyType match {
          case StringKey =>
            labels.map(s => q"$s")
          case SymbolKey =>
            labels.map(mkSingletonSymbol)
        }
      }
      val labelsType = mkHListTpe(labelTypes)
      val labelsValue = mkHListValue(labelValues)

      q"""
      (new _root_.shapeless.DefaultSymbolicLabelling[$tpe] {
        override type Out = $labelsType
        override def apply(): $labelsType = ${castIfScala212(labelsValue, labelsType)}
      }) : _root_.shapeless.DefaultSymbolicLabelling.Aux[$tpe, $labelsType]
    """
    } else {
      c.error(c.enclosingPosition, s"$name is neither record nor sealed")
      q"_root_.scala.Predef.???"
    }
  }

  def genericSealedImpl[A: c.WeakTypeTag]: c.Tree = {
    val tpe = weakTypeOf[A]
    val name = tpe.toString
    val clazz = Class.forName(name)

    val subTypes = clazz.getPermittedSubClassDescList.map { subClassDesc =>
      val subClassName = subClassDesc.packageName + "." + subClassDesc.displayName
      parseType(subClassName).getOrElse {
        abort(s"could not found $subClassName")
      }
    }.toList

    val coproductTypeTree =
      subTypes.foldRight(mkAttributedRef(cnilTpe): Tree) { case (tpe, acc) =>
        AppliedTypeTree(mkAttributedRef(cconsTpe), List(mkTypTree(tpe), acc))
      }

    val toParamName, fromParamName = TermName(c.freshName("p"))

    val to = {
      val toCases = subTypes.zipWithIndex.map { case (tpe0, index) =>
        cq"_: $tpe0 => $index"
      }
      q"""_root_.shapeless.Coproduct.unsafeMkCoproduct(($toParamName: @_root_.scala.unchecked) match { case ..$toCases }, $toParamName).asInstanceOf[Repr]"""
    }

    q"""
      (new _root_.shapeless.Generic[$tpe] {
        type Repr = $coproductTypeTree
        override def to($toParamName: $tpe): Repr = $to
        override def from($fromParamName: Repr): $tpe = _root_.shapeless.Coproduct.unsafeGet($fromParamName).asInstanceOf[$tpe]
      }): _root_.shapeless.Generic.Aux[$tpe, $coproductTypeTree]
    """
  }

  def genericImpl[A: c.WeakTypeTag]: c.Tree = {
    val tpe = weakTypeOf[A]
    val name = tpe.toString
    val clazz = Class.forName(name)

    if (clazz.isSealed) {
      genericSealedImpl[A]
    } else if (clazz.isRecord) {
      genericRecordImpl(weakTypeOf[A])
    } else {
      c.error(c.enclosingPosition, s"$name is neither record nor sealed")
      q"_root_.scala.Predef.???"
    }
  }
  def genericRecordImpl(tpe: Type): c.Tree = {
    case class Field(name: String, method: MethodSymbol)

    val name = tpe.toString
    val clazz = Class.forName(name)

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
  }
}
