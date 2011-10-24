package de.sciss.collection.geom

object Dim {
   object Two extends Dim
}
sealed trait Dim {
   type PointType <: PointLike[Dim]
}