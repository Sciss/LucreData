/*
 *  QuadTree.scala
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
 *
 *
 *  Changelog:
 */

package de.sciss.tree

object QuadTree {
   def apply[ V ]( center: Point, extent: Int ) = new QuadTree[ V ]( center, extent )
   def fromMap[ V ]( center: Point, extent: Int, m: Map[ Point, V ]) : QuadTree[ V ] = {
      val t = new QuadTree[ V ]( center, extent )
      m.foreach { case (point, value) => t.insert( point, value )}
      t
   }
}
class QuadTree[ V ]( val center: Point, val extent: Int )
extends QuadNode[ V ] {
   private val halfExt = math.max( 1, extent >> 1 )
   private var neVar: Quad[ V ] = QuadEmpty( center + Point(  halfExt, -halfExt ), halfExt )
   private var nwVar: Quad[ V ] = QuadEmpty( center + Point( -halfExt, -halfExt ), halfExt )
   private var swVar: Quad[ V ] = QuadEmpty( center + Point( -halfExt,  halfExt ), halfExt )
   private var seVar: Quad[ V ] = QuadEmpty( center + Point(  halfExt,  halfExt ), halfExt )

   def ne : Quad[ V ] = neVar
   def nw : Quad[ V ] = nwVar
   def sw : Quad[ V ] = swVar
   def se : Quad[ V ] = seVar

   def insert( point: Point, value: V ) : QuadNode[ V ] = {
      val isWest  = point.x < center.x
      val isNorth = point.y < center.y
      (isWest, isNorth) match {
         case (false, true)   => neVar = insert( neVar, point, value )
         case (true,  true)   => nwVar = insert( nwVar, point, value )
         case (true,  false)  => swVar = insert( swVar, point, value )
         case (false, false)  => seVar = insert( seVar, point, value )
      }
      this
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