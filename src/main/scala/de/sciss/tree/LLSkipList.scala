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

import java.lang.Boolean

//trait SkipList[ A ] {
//   /**
//    * Searches for the Node of a given key.
//    *
//    * @param   v  the key to search for
//    * @return  `true` if the key is in the list, `false` otherwise
//    */
//   def contains( v: A ) : Boolean
//
//   /**
//    * Inserts a new key into the list.
//    *
//    * @param   v  the key to insert
//    * @return  `true` if the key was successfully inserted,
//    *          `false` if a node with the given key already existed
//    */
//   def add( v: A ) : Boolean
//
//   /**
//    * Queries the number of levels in the skip list.
//    */
//   def height : Int
//
//   /**
//    * Queries the number of keys in the skip list (the 'width'). This operation takes O(n) time.
//    */
//   def size : Int
//}

/**
 * A deterministic 1-3 skip list implemented using a linked list with
 * 'drawn back keys', as described by T. Papadakis. This is a rather
 * literal translation of his original C code, and serves as a basis
 * for developing the horizontal array technique. As a study, it is
 * limited to insertion of Ints.
 */
object LLSkipList {
   val MAX_KEY = 0x7FFFFFFF

   def empty : LLSkipList = new Impl

   trait Node {
      def key: Int
      def right: Node
      def down: Node
      def isBottom: Boolean
      def isTail: Boolean
   }

   private class Impl extends LLSkipList {
      class NodeImpl extends Node {
         var key: Int = _
         var right: NodeImpl = _
         var down: NodeImpl = _

         def isBottom   = this eq bottom
         def isTail     = this eq tail
      }

      var head    = new NodeImpl
      val bottom  = new NodeImpl
      val tail    = new NodeImpl

      def top : Node = head.down

      // initialize them
      head.key    = MAX_KEY
      head.down   = bottom
      head.right  = tail
      bottom.right= bottom // so that we can safely call r infinitely
      bottom.down = bottom // dito
      tail.key    = MAX_KEY
      tail.right  = tail   // so that we can safely call r infinitely

      /**
       * Searches for the Node of a given key.
       *
       * @param   v  the key to search for
       * @return  `true` if the key is in the list, `false` otherwise
       */
      def contains( v: Int ) : Boolean = {
         var x = head
         while( !x.isBottom ) {
            while( v > x.key ) x = x.right
            if( x.down.isBottom ) {
               return( v == x.key )
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
      def add( v: Int ) : Boolean = {
         var x       = head
         bottom.key  = v
         var success = true
         while( !x.isBottom ) {
            while( v > x.key ) x = x.right
            val dr   = x.down.right
            val drr  = dr.right
            if( x.key > drr.key ) { // this happens when the gap has size 3
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
         if( !head.right.isTail ) { // need to increase list height
            val t    = new NodeImpl
            t.down   = head
            t.right  = tail
            t.key    = MAX_KEY
            head     = t
         }
         success
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
      def size : Int = {
         var x = top
         while( !x.down.isBottom ) x = x.down
         var i = 0; while( x.key != MAX_KEY ) { x = x.right; i += 1 }
         i
      }
   }
}
trait LLSkipList extends SkipList[ Int ] {
   def top : LLSkipList.Node
}