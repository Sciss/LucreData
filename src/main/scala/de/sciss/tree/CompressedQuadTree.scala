/*
 *  CompressedQuadTree.scala
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

import annotation.tailrec

object CompressedQuadTree {
   def apply[ V ]( center: Point, extent: Int ) = new CompressedQuadTree[ V ]( center, extent )
}
class CompressedQuadTree[ V ]( val center: Point, val extent: Int )
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

//   private def interesting( point: Point ) : Quad[ V ] = {
//
//   }

//   private def binFloor( i: Int ) : Int = if( i >= 0 ) {
//      Integer.highestOneBit( i )
//   } else {
//      -Integer.highestOneBit( -i )
//   }
//
//   private def binCeil( i: Int ) : Int = if( i >= 0 ) {
//      val j = Integer.highestOneBit( i )
//      if( j == i ) i else j << 1
//   } else {
//      val i0 = -i
//      val j = Integer.highestOneBit( i0 )
//      if( j == i0 ) i else -(j << 1)
//   }

   /*
    * "Given a quadrant of a square p containing two points x and y, find the largest interesting square inside this quadrant."
    * (greatest interesting square)
    *
    * @return  a tuple consisting of the centre point and extent of the greatest interesting square
    */
   private def gisqr( pc: Point, pe: Int, a: Point, b: Point ) : (Point, Int) = {
      val tlx        = pc.x - pe
      val tly        = pc.y - pe
      val akx        = a.x - tlx
      val aky        = a.y - tly
      val bkx        = b.x - tlx
      val bky        = b.y - tly
      val (x1, x2)   = if( akx <= bkx ) (akx, bkx) else (bkx, akx )
      val (y1, y2)   = if( aky <= bky ) (aky, bky) else (bky, aky )
      val mx         = binSplit( x1 + 1, x2 )
      val my         = binSplit( y1 + 1, y2 )
      if( mx <= my ) { // that means the x extent is greater (x grid more coarse)!
         (Point( tlx + (x2 & mx), tly + (y1 & mx) - mx ), -mx)
      } else {
         (Point( tlx + (x1 & my) - my, tly + (y2 & my) ), -my)
      }
   }

//   @tailrec private def binSplit( a: Int, b: Int, mask: Int = 0xFFFFFFFF ): Int = {
//     val mask2 = mask << 1
//     if( a > (b & mask2) ) mask // (b & mask, -mask)
//     else binSplit( a, b, mask2 )
//   }

   // http://stackoverflow.com/questions/6156502/integer-in-an-interval-with-maximized-number-of-trailing-zero-bits
   @tailrec private def binSplit( a: Int, b: Int, mask: Int = 0xFFFF0000, shift: Int = 8 ): Int = {
      val gt = a > (b & mask)
      if( shift == 0 ) {
         if( gt ) mask >> 1 else mask
      } else {
        binSplit( a, b, if( gt ) mask >> shift else mask << shift, shift >> 1 )
      }
   }

   private def insert( quad: Quad[ V ], point: Point, value: V ) : Quad[ V ] = {
      val d = point - quad.center
      val e = quad.extent
      require( d.x >= -e && d.x < e && d.y >= -e && d.y < e )
      quad match {
         case QuadEmpty( center, extent ) => QuadLeaf( center, extent, point, value )
         case t: QuadTree[ _ ] => error( "TODO" ) // t.insert( point, value ); t
         /*
            "If the quadrant of p(x) that x is inserted into already contains a point y or
            an interesting square r, then we insert to Q a new interesting square q ⊂ p
            that contains both x and y (or r) but separates x and y (or r) into different
            quadrants of q."
          */
         case QuadLeaf( center, extent, point2, value2 ) =>
            val (ic, ie) = gisqr( center, extent, point2, point )
            val t = CompressedQuadTree[ V ]( center, extent )
//            t.insertQuad( point2, value2 )
//            t.insertQuad( point, value )
            error( "TODO" )
            t
      }
   }
}