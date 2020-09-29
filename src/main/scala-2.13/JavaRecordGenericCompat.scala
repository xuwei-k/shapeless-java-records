trait JavaRecordGenericCompat { self: JavaRecordGeneric =>
  protected def castIfScala212(tree: c.Tree, tpe: c.Type): c.Tree =
    tree
}
