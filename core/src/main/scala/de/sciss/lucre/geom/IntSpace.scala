/*
 *  IntSpace.scala
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

import annotation.tailrec

object IntSpace {
   sealed trait TwoDim extends Space[ TwoDim ] {
      type PointLike       = IntPoint2DLike
      type Point           = IntPoint2D
      type HyperCubeLike   = IntSquareLike
      type HyperCube       = IntSquare
   }
   implicit object TwoDim extends TwoDim {
      val maxPoint         = IntPoint2D( Int.MaxValue, Int.MaxValue )
      val dim              = 2
   }

   sealed trait ThreeDim extends Space[ ThreeDim ] {
      type PointLike       = IntPoint3DLike
      type Point           = IntPoint3D
      type HyperCubeLike   = IntCubeLike
      type HyperCube       = IntCube
   }
   implicit object ThreeDim extends ThreeDim {
      val maxPoint         = IntPoint3D( Int.MaxValue, Int.MaxValue, Int.MaxValue )
      val dim              = 3
   }

   final case class NDim( dim: Int ) extends Space[ NDim ] {
      space =>

      type PointLike       = IntPointNLike
      type Point           = IntPointN
      type HyperCubeLike   = IntHyperCubeNLike
      type HyperCube       = IntHyperCubeN
      val maxPoint         = IntPointN( Vector.fill( dim )( Int.MaxValue ))

   }

   /**
    * A helper method which efficiently calculates the unique integer in an interval [a, b] which has
    * the maximum number of trailing zeros in its binary representation (a and b are integers > 0).
    * This is used by the `HyperCube` implementations to find the greatest interesting square for
    * two given children.
    *
    * Thanks to Rex Kerr and Daniel Sobral
    * ( http://stackoverflow.com/questions/6156502/integer-in-an-interval-with-maximized-number-of-trailing-zero-bits )
    */
   def binSplit( a: Int, b: Int ): Int = binSplitRec( a, b, 0xFFFF0000, 8 )

   @tailrec private def binSplitRec( a: Int, b: Int, mask: Int, shift: Int ): Int = {
      val gt = a > (b & mask)
      if( shift == 0 ) {
         if( gt ) mask >> 1 else mask
      } else {
         binSplitRec( a, b, if( gt ) mask >> shift else mask << shift, shift >> 1 )
      }
   }
}