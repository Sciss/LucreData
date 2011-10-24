package de.sciss.collection.geom

object Dim {
   sealed trait Two extends Dim {
      type PointType = Point2DLike
      type QuadType  = Quad2D
   }
//   object Two extends Two {
//      type PointType = Point2DLike
//      type QuadType  = Quad2D
//   }
}
sealed trait Dim {
   type PointType <: PointLike[ _ <: Dim ]
   type QuadType <: Quad[ _ <: Dim ]
}