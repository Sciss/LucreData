package de.sciss.collection.geom

object Dim {
   sealed trait Two extends Dim[ Two ] {
//      type Self = Two
      type PointType = Point2DLike
      type QuadType  = Quad2D
   }
//   object Two extends Two {
//      type PointType = Point2DLike
//      type QuadType  = Quad2D
//   }
}

/**
 * Big thanks to Aleksey Nikiforov for figuring out
 * how to plug the types together...
 */
sealed trait Dim[ Self <: Dim[ Self ]] {
   type PointType <: PointLike[ Self ]
   type QuadType  <: Quad[ Self ]
}