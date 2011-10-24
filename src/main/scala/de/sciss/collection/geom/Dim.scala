package de.sciss.collection.geom

object Dim {
   sealed trait Two extends Dim[ Two ] {
      type Point  = Point2DLike
      type Quad   = Quad2DLike
   }

   sealed trait Three extends Dim[ Three ] {
      type Point  = Point3DLike
      type Quad   = Quad3DLike
   }
}

/**
 * Big thanks to Aleksey Nikiforov for figuring out
 * how to plug the types together...
 */
sealed trait Dim[ Self <: Dim[ Self ]] {
   type Point <: PointLike[ Self ]
   type Quad  <: QuadLike[ Self ]
}