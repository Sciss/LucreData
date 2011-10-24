package de.sciss.collection.geom

trait QueryShape2D {
   def overlapArea( q: Quad2D ) : Long
   def area : Long

   /**
    * Queries the overlap of this shape with a given
    * `Point2D p`. The point is considered to have
    * a side length of 1!
    *
    * @return  `true` if this shape contains or partly overlaps
    *          the given point
    */
   def contains( p: Point2DLike ) : Boolean
}
