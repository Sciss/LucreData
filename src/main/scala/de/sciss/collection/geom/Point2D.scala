package de.sciss.collection.geom

trait Point2DLike extends PointLike[ Dim.Two ] with Rectangle2DLike {
   def x: Int
   def y: Int

   final def left             = x
   final def top              = y
   final override def right   = x
   final override def bottom  = y
//   final def width            = 1
//   final def height           = 1

   def distanceSq( that: Point2DLike ) : Long = {
      val dx = that.x.toLong - x.toLong
      val dy = that.y.toLong - y.toLong
      dx * dx + dy * dy
   }

   // ---- QueryShape2D ----
   final def overlapArea( q: Quad2D ) : Long = if( q.contains( this )) 1L else 0L
   final def area : Long = 1L

   /**
    * Queries the overlap of this shape with a given
    * `Point2D p`. The point is considered to have
    * a side length of 1!
    *
    * @return  `true` if this shape contains or partly overlaps
    *          the given point
    */
   final def contains( p: Point2DLike ) : Boolean = p.x == this.x && p.y == this.y

   /**
    * Returns the orientation of the given point wrt this point, according
    * to the following scheme:
    *
    *   5   4    6
    *     +---+
    *   1 | 0 |  2
    *     +---+
    *   9   8   10
    *
    *  Therefore the horizontal orientation can be extracted
    *  with `_ & 3`, and the vertical orientation with `_ >> 2`,
    *  where orientation is 0 for 'parallel', 1 for 'before' and
    *  '3' for 'after', so that if the orient is before or
    *  after, the sign can be retrieved via `_ - 2`
    *
    *  For example, if this is `Point2D(4, 4)` and the query
    *  point is `Point2D(4, 5)`, the result is `12`. If the
    *  query is `Point2D(0, 0)`, the result is `5`, etc.
    */
   final def orient( b: Point2DLike ) : Int = {
      val ax = x
      val ay = y
      val bx = b.x
      val by = b.y
      val dx = if( bx < ax ) 1 else if( bx > ax ) 2 else 0
      val dy = if( by < ay ) 4 else if( by > ay ) 8 else 0
      dx | dy
   }
}

final case class Point2D( x: Int, y: Int ) extends Point2DLike {
   def +( p: Point2D ) = Point2D( x + p.x, y + p.y )
   def -( p: Point2D ) = Point2D( x - p.x, y - p.y )
}
