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

package de.sciss.tree

import sys.error

/**
 * A deterministic 1-3 skip list implemented using a linked list with
 * 'drawn back keys', as described by T. Papadakis. This is a rather
 * literal translation of his original C code, and serves as a basis
 * for developing the horizontal array technique. As a study, it is
 * limited to insertion of Ints.
 *
 * TODO: Add key observers
 */
object LLSkipList {
   def empty[ @specialized( Int, Long ) A : Ordering /* : Manifest */ ]
      ( maxKey: A, keyObserver: SkipList.KeyObserver[ A ] = new SkipList.NoKeyObserver[ A ]) : LLSkipList[ A ] =
      new Impl( maxKey, keyObserver )

   sealed trait Node[ @specialized( Int, Long ) A ] {
      def key: A
      def right: Node[ A ]
      def down: Node[ A ]
      def isBottom: Boolean
      def isTail: Boolean
   }

   private class Impl[ @specialized( Int, Long ) A ]( val maxKey: A, keyObserver: SkipList.KeyObserver[ A ])( implicit val ordering: Ordering[ A ])
   extends LLSkipList[ A ] {
      var hd            = new NodeImpl
      lazy val bottom   = new NodeImpl  // XXX fucking shit : scala has a specialization bug; we need to add lazy here!
      lazy val tl       = new NodeImpl  // XXX fucking shit : scala has a specialization bug; we need to add lazy here!

      def top : Node[ A ] = hd.down

      // initialize them
      hd.key      = maxKey
      hd.down     = bottom
      hd.right    = tl
      bottom.right= bottom // so that we can safely call r infinitely
      bottom.down = bottom // dito
      tl.key      = maxKey
      tl.right    = tl   // so that we can safely call r infinitely

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

      /**
       * Inserts a new key into the list.
       *
       * @param   v  the key to insert
       * @return  `true` if the key was successfully inserted,
       *          `false` if a node with the given key already existed
       */
      override def add( v: A ) : Boolean = {
         var x       = hd
         bottom.key  = v
         var success = true
         while( !x.isBottom ) {
            while( ordering.gt( v, x.key )) x = x.right
            val dr   = x.down.right
            val drr  = dr.right
            if( ordering.gt( x.key, drr.key )) { // this happens when the gap has size 3, or we at the lowest level
               val t = new NodeImpl
               t.right  = x.right
               t.down   = drr
               x.right  = t
               t.key    = x.key
               x.key    = dr.key
            } else {
               if( x.down.isBottom ) success = false
            }
            x = x.down
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
      def -=( elem: A ) : this.type = error( "Not yet implemented" )

      def iterator : Iterator[ A ] = new Iterator[ A ] {
         var x = {
            var n = top
            while( !n.down.isBottom ) n = n.down
            n
         }

         def hasNext : Boolean = !ordering.equiv( x.key, maxKey )
         def next : A = {
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
         var x = top
         while( !x.down.isBottom ) x = x.down
         var i = 0; while( !ordering.equiv( x.key, maxKey )) { x = x.right; i += 1 }
         i
      }
   }
}
trait LLSkipList[ @specialized( Int, Long ) A ] extends SkipList[ A ] {
   def top : LLSkipList.Node[ A ]
}