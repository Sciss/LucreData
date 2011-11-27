/*
 *  Quadtree.scala
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
 */

package de.sciss.collection.obsolete

import de.sciss.collection.geom.{Space, Square, Point2D}


object Quadtree {
   import Space.TwoDim._

   def apply[ V ]( center: Point2D, extent: Int ) : QNode[ V ] = new NodeImpl[ V ]( center, extent )
   def fromMap[ V ]( center: Point2D, extent: Int, m: Map[ Point, V ]) : QNode[ V ] = {
      val t = new NodeImpl[ V ]( center, extent )
      m.foreach { case (point, value) => t.insert( point, value )}
      t
   }

   sealed trait Q[ V ] {
      def center: Point
      def extent: Int
      def quad = Square( center.x, center.y, extent )
   }
   final case class QEmpty[ V ]( center: Point2D, extent: Int ) extends Q[ V ]
   final case class QLeaf[ V ]( center: Point2D, extent: Int, point: Point, value: V ) extends Q[ V ]
   sealed trait QNode[ V ] extends Q[ V ] {
      /**
       * North east quadrant (aka I)
       */
      def ne: Q[ V ]
      /**
       * North west quadrant (aka II)
       */
      def nw: Q[ V ]
      /**
       * South west quadrant (aka III)
       */
      def sw: Q[ V ]
      /**
       * South east quadrant (aka IV)
       */
      def se: Q[ V ]

      def insert( point: Point, value: V ) : Unit
   }

   private class NodeImpl[ V ]( val center: Point2D, val extent: Int ) extends QNode[ V ] {
      private val halfExt = math.max( 1, extent >> 1 )
      private var neVar: Q[ V ] = QEmpty( center + Point2D(  halfExt, -halfExt ), halfExt )
      private var nwVar: Q[ V ] = QEmpty( center + Point2D( -halfExt, -halfExt ), halfExt )
      private var swVar: Q[ V ] = QEmpty( center + Point2D( -halfExt,  halfExt ), halfExt )
      private var seVar: Q[ V ] = QEmpty( center + Point2D(  halfExt,  halfExt ), halfExt )

      def ne : Q[ V ] = neVar
      def nw : Q[ V ] = nwVar
      def sw : Q[ V ] = swVar
      def se : Q[ V ] = seVar

      def insert( point: Point, value: V ) {
         val isWest  = point.x < center.x
         val isNorth = point.y < center.y
         (isWest, isNorth) match {
            case (false, true)   => neVar = insert( neVar, point, value )
            case (true,  true)   => nwVar = insert( nwVar, point, value )
            case (true,  false)  => swVar = insert( swVar, point, value )
            case (false, false)  => seVar = insert( seVar, point, value )
         }
      }

      private def insert( quad: Q[ V ], point: Point, value: V ) : Q[ V ] = {
         val d = Point2D( point.x - quad.center.x, point.y - quad.center.y )
         val e = quad.extent
         require( d.x >= -e && d.x < e && d.y >= -e && d.y < e )
         quad match {
            case QEmpty( _c, _e ) => QLeaf( _c, _e, point, value )
            case t: QNode[ _ ] => t.insert( point, value ); t
            case QLeaf( _c, _e, point2, value2 ) =>
               val t = Quadtree[ V ]( _c, _e )
               t.insert( point2, value2 )
               t.insert( point, value )
               t
         }
      }
   }
}