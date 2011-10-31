package de.sciss.collection.geom

import annotation.tailrec

object HyperCube {
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
trait HyperCube[ D <: Space[ D ]] /* extends RectangleLike[ D ] */ {
//   def extent: Int

   def orthant( idx: Int ) : D#HyperCube

   /**
    * The side length is two times the extent.
    */
//   def side : Int //   = extent << 1

   def contains( point: D#Point ) : Boolean

   /**
    * Checks whether a given hyper-cube is fully contained in this hyper-cube.
    * This is also the case if their bounds full match.
    */
   def contains( hyperCube: D#HyperCube ) : Boolean

//   def area : D#BigNum

//   def overlapArea( hyperCube: D#HyperCube ) : D#BigNum

   /**
    * Calculates the minimum distance to a point in the euclidean metric.
    * This calls `minDistanceSq` and then takes the square root.
    */
   def minDistance( point: D#Point ) : Double

   /**
    * Calculates the maximum distance to a point in the euclidean metric.
    * This calls `maxDistanceSq` and then takes the square root.
    */
   def maxDistance( point: D#Point ) : Double

//   /**
//    * The squared euclidean distance of the
//    * closest of the hyper-cube's corners or sides to the point, if the point is outside the hyper-cube,
//    * or zero, if the point is contained
//    */
//   def minDistanceSq( point: D#Point ) : D#BigNum

//   /**
//    * Calculates the maximum squared euclidean
//    * distance to a point in the euclidean metric.
//    * This is the distance (pow space) to the corner which is the furthest from
//    * the `point`, no matter if it lies within the hyper-cube or not.
//    */
//   def maxDistanceSq( point: D#Point ) : D#BigNum

   /**
    * Determines the orthant index of a point `point`.
    *
    * @return  the index of the orthant (beginning at 0), or -1 if `point` lies
    *          outside of this hyper-cube.
    */
   def indexOf( point: D#Point ) : Int

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
   def greatestInteresting( a: D#Point, b: D#Point ) : D#HyperCube

   /**
    * Calculates the greatest interesting hyper-cube inside this hyper-cube which
    * contains both hyper-cube `a` and point `b`, and they occupy distinct orthants.
    */
   def greatestInteresting( a: D#HyperCube, b: D#Point ) : D#HyperCube
}