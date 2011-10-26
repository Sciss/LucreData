package de.sciss.collection.geom

trait QuadLike[ D <: Space[ D ]] /* extends RectangleLike[ D ] */ {
   def extent: Int

   def quadrant( idx: Int ) : D#Quad // Quad[ D ]

   /**
    * The side length is two times the extent.
    */
//   def side : Int //   = extent << 1

   def contains( point: D#Point ) : Boolean

   /**
    * Checks whether a given quad is fully contained in this quad.
    * This is also the case if their bounds full match.
    */
   def contains( quad: D#Quad ) : Boolean

   def area : D#BigNum

   def overlapArea( quad: D#Quad ) : Long

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

   /**
    * The 'squared' (to the power of the dimension) euclidean distance of the
    * closest of the quad's corners to the point, if the point is outside the quad,
    * or `0L`, if the point is contained
    */
   def minDistanceSq( point: D#Point ) : Long

   /**
    * Calculates the maximum 'squared' (to the power of the dimension) euclidean
    * distance to a point in the euclidean metric.
    * This is the distance (pow space) to the corner which is the furthest from
    * the `point`, no matter if it lies within the quad or not.
    */
   def maxDistanceSq( point: D#Point ) : Long

   /**
    * Determines the quadrant index of a point `point`.
    *
    * @return  the index of the quadrant (beginning at 0), or (-index - 1) if `point` lies
    *          outside of this quad.
    */
   def indexOf( point: D#Point ) : Int

   /**
    * Determines the quadrant index of another internal quad `inner`.
    *
    * @return  the index of the quadrant (beginning at 0), or (-index - 1) if `inner` lies
    *          outside of this quad.
    */
   def indexOf( inner: D#Quad ) : Int

   /**
    * Calculates the greatest interesting quad inside this quad which
    * contains both points `a` and `b`, and they occupy distinct quadrants.
    */
   def greatestInteresting( a: D#Point, b: D#Point ) : D#Quad

   /**
    * Calculates the greatest interesting quad inside this quad which
    * contains both quad `a` and point `b`, and they occupy distinct quadrants.
    */
   def greatestInteresting( a: D#Quad, b: D#Point ) : D#Quad
}