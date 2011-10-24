package de.sciss.collection.geom

object Dim {
   sealed trait Two extends Dim[ Two ] {
      type Point     = Point2DLike
      type Quad      = Quad2DLike

      val maxPoint   = Point2D( Int.MaxValue, Int.MaxValue )
   }
   object Two extends Two

   sealed trait Three extends Dim[ Three ] {
      type Point     = Point3DLike
      type Quad      = Quad3DLike

      val maxPoint   = Point3D( Int.MaxValue, Int.MaxValue, Int.MaxValue )
   }
   object Three extends Three
}

/**
 * Big thanks to Aleksey Nikiforov for figuring out
 * how to plug the types together...
 */
sealed trait Dim[ Self <: Dim[ Self ]] {
   type Point <: PointLike[ Self ]
   type Quad  <: QuadLike[ Self ]

   def maxPoint : Point
}