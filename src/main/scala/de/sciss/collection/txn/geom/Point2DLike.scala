//package de.sciss.collection
//package txn.geom
//
//import de.sciss.lucrestm.{DataOutput, Writer}
//
//
//trait Point2DLike extends geom.Point2DLike with Writer
//
//final case class Point2D( x: Int, y: Int ) extends Point2DLike {
//   def +( p: geom.Point2D ) = Point2D( x + p.x, y + p.y )
//   def -( p: geom.Point2D ) = Point2D( x - p.x, y - p.y )
//
//   def write( out: DataOutput ) {
//      out.writeInt( x )
//      out.writeInt( y )
//   }
//}