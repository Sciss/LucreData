package de.sciss.tree

case class Point( x: Int, y: Int ) {
//   def orthoDist( p: Point ) : Int = math.max( math.abs( x - p.x ), math.abs( y - p.y ))
   def +( p: Point ) = Point( x + p.x, y + p.y )
   def -( p: Point ) = Point( x - p.x, y - p.y )
}

sealed trait Quad[ V ] {
   def center: Point
   def extent: Int
}
final case class QuadEmpty[ V ]( center: Point, extent: Int ) extends Quad[ V ]
final case class QuadLeaf[ V ]( center: Point, extent: Int, point: Point, value: V ) extends Quad[ V ]

object QuadTree {
   def apply[ V ]( center: Point, extent: Int ) = new QuadTree[ V ]( center, extent )
}
final class QuadTree[ V ]( val center: Point, val extent: Int )
extends Quad[ V ] {
   private val halfExt = math.max( 1, extent >> 1 )
   private var nwVar: Quad[ V ] = QuadEmpty( center + Point( -halfExt, -halfExt ), halfExt )
   private var neVar: Quad[ V ] = QuadEmpty( center + Point(  halfExt, -halfExt ), halfExt )
   private var swVar: Quad[ V ] = QuadEmpty( center + Point( -halfExt,  halfExt ), halfExt )
   private var seVar: Quad[ V ] = QuadEmpty( center + Point(  halfExt,  halfExt ), halfExt )

   def nw : Quad[ V ] = nwVar
   def ne : Quad[ V ] = neVar
   def sw : Quad[ V ] = swVar
   def se : Quad[ V ] = seVar

   def insert( point: Point, value: V ) {
      val isWest  = point.x < center.x
      val isNorth = point.y < center.y
      (isWest, isNorth) match {
         case (true,  true)   => nwVar = insert( nwVar, point, value )
         case (false, true)   => neVar = insert( neVar, point, value )
         case (true,  false)  => swVar = insert( swVar, point, value )
         case (false, false)  => seVar = insert( seVar, point, value )
      }
   }

   private def insert( quad: Quad[ V ], point: Point, value: V ) : Quad[ V ] = {
      val d = point - quad.center
      val e = quad.extent
      require( d.x >= -e && d.x < e && d.y >= -e && d.y < e )
      quad match {
         case QuadEmpty( center, extent ) => QuadLeaf( center, extent, point, value )
         case t: QuadTree[ _ ] => t.insert( point, value ); t
         case QuadLeaf( center, extent, point2, value2 ) =>
            val t = QuadTree[ V ]( center, extent )
            t.insert( point2, value2 )
            t.insert( point, value )
            t
      }
   }
}