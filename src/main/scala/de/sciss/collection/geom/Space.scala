package de.sciss.collection.geom

object Space {
   sealed trait TwoDim extends Space[ TwoDim ] {
      type Point           = Point2DLike
      type HyperCube       = SquareLike
      type BigNum          = Long

      final val maxPoint   = Point2D( Int.MaxValue, Int.MaxValue )
      final val dim        = 2

      final def bigGtZero( num: Long ) : Boolean = num > 0
      final def bigGt( a: Long, b: Long ) : Boolean = a > b
   }
   object TwoDim extends TwoDim

   sealed trait ThreeDim extends Space[ ThreeDim ] {
      type Point           = Point3DLike
      type HyperCube       = CubeLike
      type BigNum          = BigInt

      final val maxPoint   = Point3D( Int.MaxValue, Int.MaxValue, Int.MaxValue )
      final val dim        = 2

      val bigZero          = BigInt( 0 )

      final def bigGtZero( num: BigInt ) : Boolean = num > bigZero
      final def bigGt( a: BigInt, b: BigInt ) : Boolean = a > b
   }
   object ThreeDim extends ThreeDim
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
   type Point // <: PointLike[ Self ]

   /**
    * The square or hypercube in the space.
    */
   type HyperCube <: de.sciss.collection.geom.HyperCube[ Self ]

   /**
    * Represents larger values from multiplications
    * (e.g. areas, squared distances).
    */
   @specialized( Int, Long ) type BigNum

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

   /**
    * Returns `true` if `num` is greater than zero, `false` otherwise
    */
   def bigGtZero( num: Self#BigNum ) : Boolean

   /**
    * Returns `true` if `a` is greater than `b`, `false` otherwise
    */
   def bigGt( a: Self#BigNum, b: Self#BigNum ) : Boolean
}