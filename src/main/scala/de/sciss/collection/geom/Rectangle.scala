package de.sciss.collection.geom

import sys.error

trait RectangleLike extends QueryShape {
   def top : Int
   def left : Int
//   def width : Int
//   def height : Int
   def right : Int // = left + (width - 1)
   def bottom : Int // = top + (height - 1)

   final def topLeft       = Point( left, top )
   final def topRight      = Point( right, top )
   final def bottomLeft    = Point( left, bottom )
   final def bottomRight   = Point( right, bottom )
}

/**
 * Warning: We are currently allowing the rectangle to have a width or height of
 * `0x80000000`. If you use the corner methods `topRight`, `bottomLeft` or
 * `bottomRight`, these will still give you correct points (given that
 * `topLeft` is `Point(0,0)`)!
 */
final case class Rectangle( left: Int, top: Int, right: Int, bottom: Int ) extends RectangleLike {
   def area : Long = {
      val width   = right.toLong  + 1 - left.toLong
      val height  = bottom.toLong + 1 - top.toLong
      width * height
   }
   def contains( p: PointLike ) : Boolean = {
      val px = p.x
      val py = p.y
      px >= left && px <= right && py >= top && py <= bottom
   }
   def overlapArea( q: Quad ) : Long = error( "NOT YET IMPLEMENTED" )
}