package de.sciss.collection.geom

object Dim {
   sealed trait Two extends Dim[ Two ] {
      type PointType = Point2DLike
      type QuadType  = Quad2DLike
   }

   sealed trait Three extends Dim[ Three ] {
      type PointType = Point3DLike
      type QuadType  = Quad3DLike
   }
}

/**
 * Big thanks to Aleksey Nikiforov for figuring out
 * how to plug the types together...
 */
sealed trait Dim[ Self <: Dim[ Self ]] {
   type PointType <: PointLike[ Self ]
   type QuadType  <: QuadLike[ Self ]
}