/*
 *  QueryShape.scala
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

/**
 * A shape for range queries. Type `A` indicates the results
 * of area calculations, and may be specialized.
 */
trait QueryShape[ @specialized( Long ) Area, D <: Space[ D ]] {
   def overlapArea( q: D#HyperCube ) : Area
//   def area : Long

//   def compareArea( a: D#BigNum, b: D#BigNum ) : Int

   def isAreaGreater( a: D#HyperCube, b: Area ) : Boolean

   def isAreaNonEmpty( area: Area ) : Boolean

   /**
    * Queries the overlap of this shape with a given
    * `Point2D p`. The point is considered to have
    * a side length of 1!
    *
    * @return  `true` if this shape contains or partly overlaps
    *          the given point
    */
   def contains( p: D#PointLike ) : Boolean
}
