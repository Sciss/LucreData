/*
 *  HyperCube.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2013 Hanns Holger Rutz. All rights reserved.
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

trait HyperCube[ D <: Space[ D ]] {

   def orthant( idx: Int ) : D#HyperCube

   def contains( point: D#PointLike ) : Boolean

   /**
    * Checks whether a given hyper-cube is fully contained in this hyper-cube.
    * This is also the case if their bounds full match.
    */
   def contains( hyperCube: D#HyperCube ) : Boolean

   /**
    * Calculates the minimum distance to a point in the euclidean metric.
    * This calls `minDistanceSq` and then takes the square root.
    */
   def minDistance( point: D#PointLike ) : Double

   /**
    * Calculates the maximum distance to a point in the euclidean metric.
    * This calls `maxDistanceSq` and then takes the square root.
    */
   def maxDistance( point: D#PointLike ) : Double

   /**
    * Determines the orthant index of a point `point`.
    *
    * @return  the index of the orthant (beginning at 0), or -1 if `point` lies
    *          outside of this hyper-cube.
    */
   def indexOf( point: D#PointLike ) : Int

   /**
    * Determines the orthant index of another internal hyper-cube `inner`.
    *
    * @return  the index of the orthant (beginning at 0), or -1 if `inner` lies
    *          outside of this hyper-cube.
    */
   def indexOf( inner: D#HyperCube ) : Int

   /**
    * Calculates the greatest interesting hyper-cube inside this hyper-cube which
    * contains both points `a` and `b`, and they occupy distinct orthants.
    */
   def greatestInteresting( a: D#PointLike, b: D#PointLike ) : D#HyperCube

   /**
    * Calculates the greatest interesting hyper-cube inside this hyper-cube which
    * contains both hyper-cube `a` and point `b`, and they occupy distinct orthants.
    */
   def greatestInteresting( a: D#HyperCube, b: D#PointLike ) : D#HyperCube
}