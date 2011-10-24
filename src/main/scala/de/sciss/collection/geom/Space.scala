package de.sciss.collection.geom

object Space {
   sealed trait Two extends Space[ Two ] {
      type Point           = Point2DLike
      type Quad            = Quad2DLike

      final val maxPoint   = Point2D( Int.MaxValue, Int.MaxValue )
      final val dim        = 2
   }
   object Two extends Two

   sealed trait Three extends Space[ Three ] {
      type Point           = Point3DLike
      type Quad            = Quad3DLike

      final val maxPoint   = Point3D( Int.MaxValue, Int.MaxValue, Int.MaxValue )
      final val dim        = 2
   }
   object Three extends Three
}

/**
 * A `Space` abstracts over the number of dimensions
 * that are used for point and hypercube operations.
 *
 * Big thanks to Aleksey Nikiforov for figuring out
 * how to plug the types together...
 */
sealed trait Space[ Self <: Space[ Self ]] {
   /**
    * The point in the space
    */
   type Point <: PointLike[ Self ]
   /**
    * The square or hypercube in the space.
    */
   type Quad  <: QuadLike[ Self ]

   /**
    * Given that the space is limited, this represents the farthest
    * point in the space, typically which each coordinate component
    * equal to `Int.MaxValue`.
    */
   def maxPoint : Point

   /**
    * The number of dimensions in the space.
    */
   def dim : Int
}