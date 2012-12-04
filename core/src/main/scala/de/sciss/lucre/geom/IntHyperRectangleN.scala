/*
 *  IntRectangle.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.lucre
package geom

import geom.IntSpace.NDim
import collection.immutable.{IndexedSeq => IIdxSeq}

/**
 * An n-dimensional rectangular query shape.
 */
trait IntHyperRectangleNLike extends QueryShape[ BigInt, NDim ] {
   def dim: Int
   def min( dim: Int ) : Int

   /**
    * Maximum coordinate for given dimension. This is inclusive
    * (considered to be inside the shape).
    */
   def max( dim: Int ) : Int

   final def contains( point: NDim#PointLike ) : Boolean = {
      var i = 0; while( i < dim ) {
         val pc = point( i )
         if( min( i ) > pc || max( i ) < pc ) return false
      i += 1 }
      true
   }

   final def overlapArea( b: NDim#HyperCube ) : BigInt = {
      val be   = b.extent
      val bem1 = be - 1
      var prod = Space.bigOne

      var i = 0; while( i < dim ) {
         val bcc     = b.center( i )
         val mn      = math.max( min( i ), bcc - be   ).toLong
         val mx      = math.min( max( i ), bcc + bem1 ).toLong
         val delta   = mx - mn + 1
         if( delta <= 0L ) return Space.bigZero
         prod *= delta
      i += 1 }

      prod
   }

   final def isAreaGreater( a: NDim#HyperCube, b: BigInt ) : Boolean = a.area > b

   final def isAreaNonEmpty( area: BigInt ) : Boolean = area > Space.bigZero
}
//object IntHyperRectangleN {
//   def apply( min: IIdxSeq[ Int ], max: IIdxSeq[ Int ]) : IntHyperRectangleN = {
//      val dim = min.size
//      require( dim == max.size, "Min and max components must have same dimension (min dim is " + dim +
//         " and max dim is " + max.size )
//      new IntHyperRectangleN( min, max )
//   }
//}

/**
 * An n-dimensional hyper rectangular.
 *
 * @param components pairs of minimum and maximum coordinates for each dimension. In each tuple,
 *                   `_1` must be `<=` `_2`. The maximum coordinates are inclusive (defining the maximum
 *                   coordinate **within** the shape), thus it is not possible to create rectangles with
 *                   side lenghts of zero.
 */
final case class IntHyperRectangleN( components: IIdxSeq[ (Int, Int) ])
extends IntHyperRectangleNLike {
//   {
//      var i = 0; while( i < dim ) {
//         require( min( i ) <= max( i ), "Max components must be >= min components (in dim " + i +
//            " min is " + min( i ) + " and max is " + max( i ))
//      i += 1 }
//   }

   def dim: Int = components.size
   def min( dim: Int ) = components( dim )._1
   def max( dim: Int ) = components( dim )._1
}
