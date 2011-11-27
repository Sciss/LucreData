/*
 *  Space.scala
 *  (TreeTests)
 *
 *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.collection.geom

object Space {
   sealed trait TwoDim extends Space[ TwoDim ] {
      type PointLike       = Point2DLike
      type Point           = Point2D
      type HyperCubeLike   = SquareLike
      type HyperCube       = Square
   }
   implicit object TwoDim extends TwoDim {
      val maxPoint         = Point2D( Int.MaxValue, Int.MaxValue )
      val dim              = 2
   }

   sealed trait ThreeDim extends Space[ ThreeDim ] {
      type PointLike       = Point3DLike
      type Point           = Point3D
      type HyperCubeLike   = CubeLike
      type HyperCube       = Cube
   }
   implicit object ThreeDim extends ThreeDim {
      val maxPoint         = Point3D( Int.MaxValue, Int.MaxValue, Int.MaxValue )
      val dim              = 3
      val bigZero          = BigInt( 0 )
   }

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
/* sealed */ trait Space[ D <: Space[ D ]] {
   /**
    * The point in the space
    */
   type PointLike /* <: Writer */ // <: PointLike[ Self ]
   type Point <: D#PointLike

   /**
    * The square or hypercube in the space.
    */
   type HyperCubeLike <: de.sciss.collection.geom.HyperCube[ D ]
   type HyperCube <: D#HyperCubeLike

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
   def maxPoint : D#Point // Like

   /**
    * The number of dimensions in the space.
    */
   def dim : Int
}