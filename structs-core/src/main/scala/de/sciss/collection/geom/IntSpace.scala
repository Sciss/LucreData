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

package de.sciss.collection.geom

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
