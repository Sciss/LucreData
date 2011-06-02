package de.sciss.tree

import java.lang.Boolean

/**
 * A deterministic 1-3 skip list implemented using a linked list with
 * 'drawn back keys', as described by T. Papadakis. This is a rather
 * literal translation of his original C code, and serves as a basis
 * for developing the horizontal array technique. As a study, it is
 * limited to insertion of Ints.
 */
object LLSkipList {
   val MAX_KEY = 0x7FFFFFFF

   def empty = new LLSkipList()

   class Node { var key: Int = _; var right: Node = _; var down: Node = _ }
}
class LLSkipList private() {
   import LLSkipList._

   var head    = new Node
   val bottom  = new Node
   val tail    = new Node

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
      while( x ne bottom ) {
         while( v > x.key ) x = x.right
         if( x.down eq bottom ) {
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
      while( x ne bottom ) {
         while( v > x.key ) x = x.right
         val dr   = x.down.right
         val drr  = dr.right
         if( x.key > drr.key ) { // this happens when the gap has size 3
            val t = new Node
            t.right  = x.right
            t.down   = drr
            x.right  = t
            t.key    = x.key
            x.key    = dr.key
         } else {
            if( x.down eq bottom ) success = false
         }
         x = x.down
      }
      if( head.right ne tail ) { // need to increase list height
         val t    = new Node
         t.down   = head
         t.right  = tail
         t.key    = MAX_KEY
         head     = t
      }
      success
   }

   /**
    * Queries the number of levels in the skip list
    */
   def height : Int = {
      var x = head
      var i = 0; while( x ne bottom ) { x = x.down; i += 1 }
      i
   }

   /**
    * Queries the number of keys in the skip list (the 'width'). This operation takes O(n) time.
    */
   def size : Int = {
      var x = head.down
      while( x.down ne bottom ) x = x.down
      var i = 0; while( x.key != MAX_KEY ) { x = x.right; i += 1 }
      i
   }
}
