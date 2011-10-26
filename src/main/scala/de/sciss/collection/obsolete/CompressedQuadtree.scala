/*
 *  CompressedQuadtree.scala
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

import de.sciss.collection.geom.{SquareLike, Point2DLike}

object CompressedQuadtree {
   def apply[ V ]( quad: SquareLike ) : QNode[ V ] = {
      val quads = new Array[ Q[ V ]]( 4 )
      QNode[ V ]( quad )( quads )
   }

   def fromMap[ V ]( quad: SquareLike, m: Map[ Point2DLike, V ]) : QNode[ V ] = {
      val t = QNode[ V ]( quad )()
      m.foreach {
         case (point, value) =>
            t.insert( point, value )
      }
      t
   }

   sealed trait Q[ +V ]
   case object QEmpty extends Q[ Nothing ]
   final case class QLeaf[ V ]( point: Point2DLike, value: V ) extends Q[ V ]

   final case class QNode[ V ]( quad: SquareLike )( quads: Array[ Q[ V ]] = new Array[ Q[ V ]]( 4 ))
   extends Q[ V ] {
      // fix null squares
      {
         var i = 0; while( i < 4 ) {
            if( quads( i ) == null ) quads( i ) = QEmpty
         i += 1 }
      }

      def child( idx: Int ) : Q[ V ] = quads( idx )

      def insert( point: Point2DLike, value: V ) {
         val qidx = quad.indexOf( point )
         require( qidx >= 0, point.toString + " lies outside of root square " + quad )
         quads( qidx ) match {
            case QEmpty => quads( qidx ) = QLeaf( point, value )
            case t @ QNode( tq ) =>
               if( tq.contains( point )) {
                  t.insert( point, value )
               } else {
                  val iq      = quad.orthant( qidx ).greatestInteresting( tq, point )
                  val iquads  = new Array[ Q[ V ]]( 4 )
                  val tidx    = iq.indexOf( tq )
                  iquads( tidx ) = t
                  val pidx    = iq.indexOf( point )
                  iquads( pidx ) = QLeaf( point, value )
                  quads( qidx ) = QNode[ V ]( iq )( iquads )
               }

            /*
               "If the quadrant of p(x) that x is inserted into already contains a point y or
               an interesting square r, then we insert to Child a new interesting square q ⊂ p
               that contains both x and y (or r) but separates x and y (or r) into different
               quadrants of q."
             */
            case l @ QLeaf( point2, value2 ) =>
               val iq      = quad.orthant( qidx ).greatestInteresting ( point2, point )
               val iquads  = new Array[ Q[ V ]]( 4 )
               val lidx    = iq.indexOf( point2 )
               iquads( lidx ) = l
               val pidx    = iq.indexOf( point )
               iquads( pidx ) = QLeaf( point, value )
               quads( qidx ) = QNode[ V ]( iq )( iquads )
         }
      }
   }
}