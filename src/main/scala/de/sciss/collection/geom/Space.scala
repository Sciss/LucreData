package de.sciss.collection.geom

object Space {
   sealed trait TwoDim extends Space[ TwoDim ] {
      type Point           = Point2DLike
      type HyperCube       = SquareLike
//      type BigNum          = Long

      final val maxPoint   = Point2D( Int.MaxValue, Int.MaxValue )
      final val dim        = 2

//      final val bigZero    = 0L
//      final def bigGtZero( a: Long ) : Boolean = a > 0
//      final def bigGeqZero( a: Long ) : Boolean = a >= 0
//      final def bigLeqZero( a: Long ) : Boolean = a <= 0
//      final def bigGt( a: Long, b: Long ) : Boolean = a > b
//      final def bigGeq( a: Long, b: Long ) : Boolean = a >= b
//      final def bigOrdering : Ordering[ Long ] = Ordering.Long
//      final def bigCompare( a: Long, b: Long ) : Int = if( a < b ) -1 else if( a > b ) 1 else 0
   }
   object TwoDim extends TwoDim

   sealed trait ThreeDim extends Space[ ThreeDim ] {
      type Point           = Point3DLike
      type HyperCube       = CubeLike
//      type BigNum          = BigInt

      final val maxPoint   = Point3D( Int.MaxValue, Int.MaxValue, Int.MaxValue )
      final val dim        = 3

      final val bigZero    = BigInt( 0 )
//      final def bigGtZero( a: BigInt ) : Boolean = a > bigZero
//      final def bigGeqZero( a: BigInt ) : Boolean = a >= bigZero
//      final def bigLeqZero( a: BigInt ) : Boolean = a <= bigZero
//      final def bigGt( a: BigInt, b: BigInt ) : Boolean = a > b
//      final def bigGeq( a: BigInt, b: BigInt ) : Boolean = a >= b
//      final def bigOrdering : Ordering[ BigInt ] = Ordering.BigInt
//      final def bigCompare( a: BigInt, b: BigInt ) : Int = a compare b
   }
   object ThreeDim extends ThreeDim

//   /**
//    * Space for arbitrary number of dimensions.
//    *
//    * @param   dim   the number of dimensions, which must be in the interval [2, 32]
//    */
//   final case class NDim( dim: Int ) extends Space[ NDim ] {
//      require( dim >= 2 && dim <= 32, "Illegal number of dimensions (" + dim + "). Must be between 2 and 32" )
//
//
//   }
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

//   /**
//    * Represents larger values from multiplications
//    * (e.g. areas, squared distances).
//    */
//   @specialized( Int, Long ) type BigNum

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

//   /**
//    * Returns `true` if `num` is greater than zero, `false` otherwise
//    */
//   def bigGtZero( num: Self#BigNum ) : Boolean

//   /**
//    * The zero value for `BigNum`
//    */
//   def bigZero : Self#BigNum
//
//   /**
//    * Returns whether `a` is greater than zero
//    */
//   def bigGtZero( a: Self#BigNum ) : Boolean
//
//   /**
//    * Returns whether `a` is greater than or equal to zero
//    */
//   def bigGeqZero( a: Self#BigNum ) : Boolean
//
//   /**
//    * Returns whether `a` is less than or equal to zero
//    */
//   def bigLeqZero( a: Self#BigNum ) : Boolean
//
//   /**
//    * Returns `true` if `a` is greater than `b`, `false` otherwise
//    */
//   def bigGt( a: Self#BigNum, b: Self#BigNum ) : Boolean
//
//   /**
//    * Returns `true` if `a` is greater than or equal to `b`, `false` otherwise
//    */
//   def bigGeq( a: Self#BigNum, b: Self#BigNum ) : Boolean
//
//   /**
//    * Compares `a` to `b`. Returns `-1` if `a < b`, `0` if `a == b`, and `1` if `a > b`
//    */
//   def bigCompare( a: Self#BigNum, b: Self#BigNum ) : Int
//
//   /**
//    * An ordering for the `BigNum` type.
//    */
//   def bigOrdering: Ordering[ Self#BigNum ]
}