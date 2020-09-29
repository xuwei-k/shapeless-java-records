import scala.reflect.macros.whitebox

trait JavaRecordGenericCompat { self: JavaRecordGeneric =>
  import c.universe._
  protected def castIfScala212(tree: c.Tree, tpe: c.Type): c.Tree =
    q"$tree.asInstanceOf[$tpe]"
}
