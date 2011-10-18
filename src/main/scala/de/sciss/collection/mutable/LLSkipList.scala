/*
 *  LLSkipList.scala
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

package de.sciss.collection
package mutable

/**
 * A deterministic 1-3 skip list implemented using a linked list with
 * 'drawn back keys', as described by T. Papadakis.
 */
object LLSkipList {
   def empty[ A : Ordering : MaxKey ] : LLSkipList[ A ] = empty()
   def empty[ A ]( keyObserver: SkipList.KeyObserver[ A ] = new SkipList.NoKeyObserver[ A ])
                 ( implicit ord: Ordering[ A ], maxKey: MaxKey[ A ]) : LLSkipList[ A ] =
      new Impl( maxKey.value, keyObserver )

   sealed trait Node[ /* @specialized( Int, Long ) */ A ] {
      def key: A
      def right: Node[ A ]
      def down: Node[ A ]
      def isBottom: Boolean
      def isTail: Boolean
   }

   private class Impl[ /* @specialized( Int, Long ) */ A ]( val maxKey: A, keyObserver: SkipList.KeyObserver[ A ])( implicit val ordering: Ordering[ A ])
   extends LLSkipList[ A ] {
      // XXX fucking shit : scala has a specialization bug; we needed to add lazy here when Node was an instance trait of
      // LLSkipList -- now that we have put Node back into the object, the access error is gone again.

      val bottom= {
         val res     = new NodeImpl
         res.right   = res // so that we can safely call r infinitely
         res.down    = res // dito
         res
      }
      val tl    = {
         val res     = new NodeImpl
         res.key     = maxKey
         res.right   = res // so that we can safely call r infinitely
         res
      }
      var hd         = {
         val res     = new NodeImpl
         res.key     = maxKey
         res.down    = bottom
         res.right   = tl
         res
      }

      def top : Node[ A ] = hd.down
      def minGap : Int = 1

      class NodeImpl extends Node[ A ] {
         var key: A = _
         var right: NodeImpl = _
         var down: NodeImpl = _

         def isBottom   = this eq bottom
         def isTail     = this eq tl
      }

      // ---- set support ----

      /**
       * Searches for the Branch of a given key.
       *
       * @param   v  the key to search for
       * @return  `true` if the key is in the list, `false` otherwise
       */
      def contains( v: A ) : Boolean = {
         if( ordering.gteq( v, maxKey )) return false
         var x = hd
         while( !x.isBottom ) {
            while( ordering.gt( v, x.key )) x = x.right
            if( x.down.isBottom ) {
               return( ordering.equiv( v, x.key ))
            }
            x = x.down
         }
         false
      }

      def isomorphicQuery( compare: A => Int ) : A = {
         require( compare( maxKey ) >= 0, "Search key cannot be greater than maxKey" )
         var x = hd
         while( !x.isBottom ) {
            while( compare( x.key ) < 0 ) x = x.right
            if( x.down.isBottom ) return x.key
            x = x.down
         }
         maxKey
      }

      /**
       * Inserts a new key into the list.
       *
       * @param   v  the key to insert
       * @return  `true` if the key was successfully inserted,
       *          `false` if a node with the given key already existed
       */
      override def add( v: A ) : Boolean = {
         require( ordering.lt( v, maxKey ), "Cannot add key (" + v + ") greater or equal to maxKey" )
         var x       = hd
         bottom.key  = v
         var success = true
         while( !x.isBottom ) {
            while( ordering.gt( v, x.key )) x = x.right
            val d    = x.down
            val dr   = x.down.right
            val drr  = dr.right
            if( ordering.gt( x.key, drr.key )) { // this happens when the gap has size 3, or we at the lowest level
               val t = new NodeImpl
               t.right  = x.right
               t.down   = drr
               x.right  = t
               t.key    = x.key
               x.key    = dr.key
               if( !d.isBottom ) keyObserver.keyUp( x.key )
            } else {
               if( d.isBottom ) success = false
            }
            x = d
         }
         if( !hd.right.isTail ) { // need to increase list height
            val t    = new NodeImpl
            t.down   = hd
            t.right  = tl
            t.key    = maxKey
            hd       = t
         }
         success
      }

      def +=( elem: A ) : this.type = { add( elem ); this }
      def -=( elem: A ) : this.type = { remove( elem ); this }

      override def remove( v: A ) : Boolean = {
         // prevents infinite loop if user provided a surmountable maxKey, or if v == maxKey
         if( ordering.gteq( v, maxKey )) return false

//println( "\nremove " + v )

         var x             = hd.down
         var success       = !x.isBottom
         bottom.key        = v
         var xPred: NodeImpl = null
         var multiPrev     = maxKey    // this stores the key previous to v in the bottom level if v did exist in higher levels
         var lastAbove     = hd.key    // last key at level above, needed to determine if we drop into the last gap
         while( !x.isBottom ) {
            while( ordering.gt( v, x.key )) {   // find where you drop
               xPred = x   // keep track of the previous gap which might be needed for a 'left-merge/borrow'
               x     = x.right
            }
            val d       = x.down
            val dIsBot  = d.isBottom
            val xKey    = x.key
            // the following holds, if either we drop into gap G with size 1, or we reached the bottom level and v was found
            if( ordering.equiv( xKey, d.right.key )) {  // i.e., either gap size is 1, or (since bottom.key == v) we found the key in level 0
               if( !ordering.equiv( xKey, lastAbove )) { // if does NOT drop in last gap -> merge or borrow to the right
                  val xSucc = x.right // now the gap G is between x and xSucc
                  // if 1 elem in next gap G' (aka xSucc.down.right),
                  // or at bottom level --> merge G and G', by lowering the element
                  // between G and G', that is xSucc
                  if( dIsBot ) { // we found the key in level 0
//println( "---1 " + xKey )
                     x.right  = xSucc.right    // replace x by its...
                     x.key    = xSucc.key      // ... right neighbour
                  } else { // we're in level > 0, merge or borrow according to size of G''
                     keyObserver.keyDown( xKey )
                     val sd   = xSucc.down
                     val sdr  = sd.right
                     if( ordering.equiv( xSucc.key, sdr.key )) { // i.e. G' has size 1
//println( "---2 " + xKey )
                        x.right     = xSucc.right    // lower separator of G and G', i.e. remove xSucc from current level
                        x.key       = xSucc.key      // the new max key for dropping into (the merged) G
                     } else {	   // if >=2 elems in next gap G'
//println( "---3 " + xKey )
                        val upKey   = sd.key             // raise 1st elem in next gap & lower...
                        x.key       = upKey
                        xSucc.down  = sdr                // ... separator of current+next gap
                        keyObserver.keyUp( upKey )
                     }
                  }
               } else {    // if DOES drop in last gap --> merge or borrow to the left
                  val pdr     = xPred.down.right
                  val dnKey   = xPred.key
                  if( ordering.lteq( dnKey, pdr.key )) { // if only 1 elm in previous gap --> merge ; XXX could be ordering.equiv ?
                     if( dIsBot ) { // if del_Key is in elem of height > 1
//println( "---4 " + xKey )
                        multiPrev = dnKey  // predecessor of del_key at bottom level
                     } else {
//println( "---5 " + xKey )
                        keyObserver.keyDown( dnKey )
                     }
                     xPred.right = x.right      // lower separator of previous+current gap
                     xPred.key   = xKey
                     x           = xPred
                  } else {    // if >= 2 elems in previous gap
//println( "---6 " + xKey )
                     // tmp = last elem in previous gap
                     val pdrr    = pdr.right
                     val tmp     = if( ordering.equiv( dnKey, pdrr.key )) pdr else pdrr
                     val upKey   = tmp.key
                     xPred.key   = upKey        // raise last elem in previous gap & lower...
                     x.down      = tmp.right    // ... separator of previous+current gap
                     keyObserver.keyDown( dnKey )
                     keyObserver.keyUp( upKey )
                  }
               }
            // i.e. either a gap of >= 2 or reached bottom
            } else {
//println( "---7 " + xKey )
               if( dIsBot ) { // which means the key was not found
                  success = false
               }
            }
            lastAbove   = x.key
            x           = d
         }

         // we might need to remove v from higher levels in a second pass
         if( !ordering.equiv( multiPrev, maxKey )) {
            x = hd.down
            while( !x.isBottom ) {
               while( ordering.gt( v, x.key )) x = x.right
               val d = x.down
               if( ordering.equiv( v, x.key )) {
//println( "---8 " + x.key )
                  x.key = multiPrev
                  if( !d.isBottom ) {
                     keyObserver.keyDown( v )
                     keyObserver.keyUp( multiPrev )
                  }
               }
               x = d
            }
         }

         // lower header of DSL, if necessary
         x = hd.down
         if( x.right.isTail ) hd = x

         success
      }

      def iterator : Iterator[ A ] = new Iterator[ A ] {
         var x = {
            var n = hd // top
            while( !n.down.isBottom ) n = n.down
            n
         }

         def hasNext : Boolean = !x.right.isTail // ordering.equiv( x.key, maxKey )
         def next() : A = {
            val res = x.key
            x = x.right
            res
         }
      }

      /**
       * Queries the number of levels in the skip list. This includes
       * the dummy head level, so if you want to skip that, subtract
       * one and begin traversing from `head.down`.
       */
      def height : Int = {
         var x = top
         var i = 0; while( !x.isBottom ) { x = x.down; i += 1 }
         i
      }

      /**
       * Queries the number of keys in the skip list (the 'width'). This operation takes O(n) time.
       */
      override def size : Int = {
         var x = hd
         while( !x.down.isBottom ) x = x.down
         var i = -1; while( !x.isTail ) { x = x.right; i += 1 }
         i
      }
   }
}
trait LLSkipList[ /* @specialized( Int, Long ) */ A ] extends SkipList[ A ] {
   def top : LLSkipList.Node[ A ]
}