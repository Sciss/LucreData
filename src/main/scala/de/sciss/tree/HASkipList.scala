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

import sys.error
import compat.Platform

// suckers

/**
 * A deterministic k-(2k+1) top-down operated skip list
 * as described in T. Papadakis, Skip Lists and Probabilistic Analysis of
 * Algorithms. Ch. 4 (Deterministic Skip Lists), pp. 55--78. Waterloo (CA) 1993
 *
 * It uses the horizontal array technique with a parameter for k (minimum gap size)
 */
object HASkipList {
   def empty[ A : Ordering : Manifest ]( minGap: Int = 1, maxKey: A ) : HASkipList[ A ] = new Impl[ A ]( minGap, maxKey )

   private class Impl[ A ]( minGap: Int, maxKey: A )( implicit ord: Ordering[ A ], mf: Manifest[ A ])
   extends HASkipList[ A ] {
      val maxGap = (minGap << 1) + 1
      var head : NodeLike = {
         val res        = new Leaf
         res.keys( 0 )  = maxKey
         res.size       = 1
         res
      }

      def add( a: A ) : Boolean = {
         var nl = head
         var idx = 0; var cmp = ord.compare( a, nl.keys( idx )); while( cmp > 0 ) {
            idx += 1
            cmp = ord.compare( a, nl.keys( idx ))
         }
         if( cmp == 0 ) false  // already in set - XXX todo: should replace value anyway!
         else nl match {
            case l: Leaf =>
               val idx1 = idx + 1
               // unfortunately Platform.arraycopy causes some overhead
               System.arraycopy( l.keys, idx, l.keys, idx1, l.size - idx1 )
               l.keys( idx ) = a
               true

            case n: Node =>
               error( "todo" )
         }
      }

      sealed trait NodeLike {
         val keys = new Array[ A ]( maxGap )
         var size = 0
//         def downOption( idx: Int ) : Option[ NodeLike ]
      }

      class Leaf extends NodeLike {
//         def downOption( idx: Int ) : Option[ NodeLike ] = None
      }

      class Node extends NodeLike {
         val down = new Array[ Node ]( maxGap )
//         def downOption( idx: Int ) : Option[ NodeLike ] = Some( down( idx ))
      }
   }
}
trait HASkipList[ A ] {
   /**
    * Adds an element to this list.
    *  @param elem the element to be added
    *  @return `true` if the element was not yet present in the list, `false` otherwise.
    */
   def add( a: A ) : Boolean
}