/*
 *  SkipList.scala
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

import sys.error  // suckers

/**
 * A deterministic k-(2k+1) top-down operated skip list
 * as described in T. Papadakis, Skip Lists and Probabilistic Analysis of
 * Algorithms. Ch. 4 (Deterministic Skip Lists), pp. 55--78. Waterloo (CA) 1993
 *
 * It uses the horizontal array technique with a parameter for k (minimum gap size)
 */
object HASkipList {
//   def empty[ @specialized( Int, Long ) B : Manifest, A ]( minGap: Int = 1, key: A => B, maxKey: B ) : HASkipList[ A ] =
//      new Impl[ B, A ]( minGap, key maxKey )

   def withIntKey[ A ]( key: A => Int, minGap: Int = 1 ) : HASkipList[ A ] =
      new IntImpl( key, minGap )

   private class IntImpl[ @specialized( Int, Long ) A ]( key: A => Int, minGap: Int )
   extends Impl[ Int, A ] {
      val maxGap  = (minGap << 1) + 1
      val arrSize = maxGap + 1

      def contains( v: A ) : Boolean = error( "TODO" )
      def size : Int = error( "TODO" )
      def height : Int = error( "TODO" )

      sealed trait NodeLike {
         val keys = new Array[ Int ]( arrSize )
         var size = 0
//         def downOption( idx: Int ) : Option[ NodeLike ]
      }

      class Leaf extends NodeLike {
//         def downOption( idx: Int ) : Option[ NodeLike ] = None
         def keys: Array[ Int ] = error( "Not supported" )
      }

      class Node extends NodeLike {
         val down = new Array[ Node ]( arrSize )
         def keys: Array[ Int ]
//         def downOption( idx: Int ) : Option[ NodeLike ] = Some( down( idx ))
      }
   }

   private sealed trait Impl[ @specialized( Int, Long ) B, @specialized( Int, Long ) A ]
   extends HASkipList[ A ] {
//      def key: A => B
//      def maxKey: B

//      val maxGap = (minGap << 1) + 1
//      var head : NodeLike = {
//         val res        = new Leaf
//         res.keys( 0 )  = maxKey
//         res.size       = 1
//         res
//      }

//      def add( v: A ) : Boolean = {
//         var nl = head
//         var idx = 0; var cmp = ord.compare( v, nl.keys( idx )); while( cmp > 0 ) {
//            idx += 1
//            cmp = ord.compare( v, nl.keys( idx ))
//         }
//         if( cmp == 0 ) false  // already in set - XXX todo: should replace value anyway!
//         else nl match {
//            case l: Leaf =>
//               val idx1 = idx + 1
//               // unfortunately Platform.arraycopy causes some overhead
//               System.arraycopy( l.keys, idx, l.keys, idx1, l.size - idx1 )
//               l.keys( idx ) = v
//               true
//
//            case n: Node =>
//               error( "todo" )
//         }
//      }
   }
}
trait HASkipList[ A ] extends SkipList[ A ] {
}