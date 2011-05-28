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

   private val quads = new Array[ Quad[ V ]]( 4 )

   def ne : Quad[ V ] = neVar
   def nw : Quad[ V ] = nwVar
   def sw : Quad[ V ] = swVar
   def se : Quad[ V ] = seVar

   def insert( point: Point, value: V ) : QuadNode[ V ] = {
      val qidx = quadIdx( center, extent, point )
      require( qidx >= 0, point.toString + " lies outside of root square " + (center, extent) )
      quads( qidx ) = quads( qidx ) match {
         case QuadEmpty( ec, ee ) => QuadLeaf( ec, ee, point, value )
         case t: QuadTree[ _ ] => error( "TODO" ) // t.insert( point, value ); t
         /*
            "If the quadrant of p(x) that x is inserted into already contains a point y or
            an interesting square r, then we insert to Q a new interesting square q ⊂ p
            that contains both x and y (or r) but separates x and y (or r) into different
            quadrants of q."
          */
         case QuadLeaf( lc, le, point2, value2 ) =>
            val (ic, ie) = gisqr( lc, le, point2, point )
            val t = CompressedQuadTree[ V ]( center, extent )
//            t.insertQuad( point2, value2 )
//            t.insertQuad( point, value )
            error( "TODO" )
            t
      }
      error( "TODO" )
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
    * @return  a tuple consisting of `_1` the centre point, `_2` the extent of the greatest interesting square,
    *          `_3` the quadrant of `a`, and `_4` the quadrant of `b` in this interesting square.
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
      // that means the x extent is greater (x grid more coarse).
      if( mx <= my ) {
         (Point( tlx + (x2 & mx), tly + (y1 & mx) - mx ), -mx)
      } else {
         (Point( tlx + (x1 & my) - my, tly + (y2 & my) ), -my)
      }
   }

   /**
    * Determines the quadrant index of a point `a` in a square `p` defined
    * by its center `pc` and extent `pe`.
    *
    * @return  the index of the quadrant (beginning at 0), or (-index - 1) if `a` lies
    *          outside of `p`.
    */
   private def quadIdx( pc: Point, pe: Int, a: Point ) : Int = {
      if( a.y < pc.y ) {      // north
         if( a.x >= pc.x ) {  // east
            if( pc.x + pe >  a.x && pc.y - pe <= a.y ) 0 else -1   // ne
         } else {             // west
            if( pc.x - pe <= a.x && pc.y - pe <= a.y ) 1 else -2   // nw
         }
      } else {                // south
         if( a.x < pc.x ) {   // west
            if( pc.x - pe <= a.x && pc.y + pe >  a.y ) 2 else -3   // sw
         } else {             // east
            if( pc.x + pe >  a.x && pc.y + pe >  a.y ) 3 else -4   // se
         }
      }
   }

   /**
    * Determines the quadrant index of a point `a` in a square `p` defined
    * by its center `pc`
    *
    * @return  the index of the quadrant (beginning at 0)
    */
   private def quadIdx( pc: Point, a: Point ) : Int = {
      if( a.y < pc.y ) {      // north
         if( a.x >= pc.x ) 0  // ne
         else 1               // nw
      } else {                // south
         if( a.x < pc.x ) 2   // sw
         else 3               // se
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
}