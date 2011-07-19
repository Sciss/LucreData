package de.sciss.collection

import sys.error

trait RectangleLike extends QueryShape {
   def top : Int
   def left : Int
   def width : Int
   def height : Int
   def right : Int = left + (width - 1)
   def bottom : Int = top + (height - 1)

   final def topLeft       = Point( left, top )
   final def topRight      = Point( right, top )
   final def bottomLeft    = Point( left, bottom )
   final def bottomRight   = Point( right, bottom )
}
final case class Rectangle( left: Int, top: Int, width: Int, height: Int ) extends RectangleLike {
   def area : Long = width.toLong * height.toLong
   def contains( p: PointLike ) : Boolean = {
      val px = p.x
      val py = p.y
      px >= left && px <= right && py >= top && py <= bottom
   }
   def overlapArea( q: Quad ) : Long = error( "NOT YET IMPLEMENTED" )
}