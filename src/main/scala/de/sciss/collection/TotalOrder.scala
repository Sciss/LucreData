/*
 *  TotalOrder.scala
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

/**
 * A data structure to maintain an ordered sequence of elements such
 * that two random elements can be compared in O(1).
 *
 * This uses an algorithm from the paper
 * Bender, M. and Cole, R. and Demaine, E. and Farach-Colton, M. and Zito, J.,
 * Two simplified algorithms for maintaining order in a list,
 * Algorithms—ESA 2002, pp. 219--223, 2002.
 *
 * The `relabel` method is based on the Python implementation by
 * David Eppstein, as published at http://www.ics.uci.edu/~eppstein/PADS/OrderedSequence.py
 *
 * Original note: "Due to rebalancing on the integer tags used to maintain order,
 * the amortized time per insertion in an n-item list is O(log n)."
 */
object TotalOrder extends /* SeqFactory[ TotalOrder ] */ {
//   type Tag = Int // Long

//   class Record[ T ]( private[ TotalOrder ] var v: Tag ) {
//      private[ TotalOrder ] var succ: Record[ T ] = null
//      private[ TotalOrder ] var pred: Record[ T ] = null
//   }

// We don't need to define an ordering -- this will already be implicitly
// derived from any TotalOrder
//   implicit def ordering[ V ] = new Ordering[ TotalOrder[ V ]] {
//      def compare( x: TotalOrder[ V ], y: TotalOrder[ V ]) = x.compare( y )
//   }

   def apply( relabelObserver: RelabelObserver = NoRelabelObserver ) : TotalOrder = new Impl( relabelObserver )

//   /**
//    * Returns a single element order corresponding to the tag ceiling. This
//    * can be used for comparison (using its `Ordered` trait).
//    *
//    * Note: you can not add elements before or after this one.
//    */
//   val max : TotalOrder = new Impl( new Size( NoRelabelObserver )) {
//      tag = Int.MaxValue
//      override def append() : TotalOrder = unsupportedOp
//      override def prepend() : TotalOrder = unsupportedOp
//      def unsupportedOp = error( "Operation not permitted" )
//   }

   sealed trait EntryLike extends Ordered[ EntryLike ] {
      def prev : EntryLike
      def next : EntryLike
      def tag : Int

      def isHead : Boolean = prev == null
      def isLast : Boolean = next.isEnd
      def isEnd : Boolean = next eq this

      /**
       * Compares the positions of x and y in the sequence
      */
      def compare( that: EntryLike ) : Int = {
         val thatTag = that.tag
         if( tag < thatTag ) -1 else if( tag > thatTag ) 1 else 0
//         tag compare that.tag
      }
   }

   trait RelabelObserver {
      def beforeRelabeling( first: EntryLike, num: Int ) : Unit
      def afterRelabeling( first: EntryLike, num: Int ) : Unit
   }

   object NoRelabelObserver extends RelabelObserver {
      def beforeRelabeling( first: EntryLike, num: Int ) {}
      def afterRelabeling( first: EntryLike, num: Int ) {}
   }

   private class Impl( val observer: RelabelObserver ) // (_t: Int)
   extends TotalOrder
}
sealed trait TotalOrder
extends Ordering[ TotalOrder.EntryLike ] {
   import TotalOrder._

   private var sizeVar : Int = 1 // root!

   protected def observer: RelabelObserver

   val root : Entry = {
      val empty   = new Entry()
      val head    = new Entry()
//      sz.inc
      head.next   = empty
      empty.prev  = head
      head
   }

   def max : Entry = {
      val e = new Entry()
      e.tag = Int.MaxValue
      e
   }

   /**
    * Returns the head element of the structure. Note that this
    * is O(n) worst case.
    */
   def head : Entry = {
      var e = root
      while( !e.isHead ) e = e.prev
      e
   }

   def compare( a: EntryLike, b: EntryLike ) : Int = a.compare( b )

   // important: maintain default equals (reference equality)
   final class Entry private[ TotalOrder ] () extends EntryLike {
      private var tagVar : Int = 0
      private var prevVar : Entry = _
      private var nextVar : Entry = this

      private def this( prev: Entry, next: Entry ) {
         this()
         sizeVar += 1 // sz.inc
//         this.elem      = elem
         prevVar = prev
         if( prev != null ) {
            require( !prev.isEnd && (prev.next eq next) )
            prev.next = this
         } else {
            require( next.isHead )
         }
         nextVar     = next
         next.prev   = this
      }

      private[ TotalOrder ] def tag_=( value: Int ) { tagVar = value }
      private[ TotalOrder ] def next_=( entry: Entry ) { nextVar = entry }
      private[ TotalOrder ] def prev_=( entry: Entry ) { prevVar = entry }

      def prev : Entry = prevVar
      def next : Entry = nextVar
      def tag : Int = tagVar

      /**
       * Inserts a new element after this node
       */
      def append() : Entry = {
         val rec     = new Entry( this, next )
         val nextTag = if( rec.isLast ) Int.MaxValue else rec.next.tag
         rec.tag     = tag + ((nextTag - tag + 1) >>> 1)
         if( rec.tag == nextTag ) relabel( rec )
         rec
      }

      /**
       * Inserts a new element before this node
       */
      def prepend() : Entry = {
         val rec     = new Entry( prev, this )
         val prevTag = if( rec.isHead ) 0 else rec.prev.tag
         rec.tag     = prevTag + ((tag - prevTag + 1) >>> 1)
         if( rec.tag == tag ) relabel( rec )
         rec
      }

      /**
       * Debugging method: Validates that the list from this entry
       * to the end has monotonically increasing
       * tags. Throws an assertion error if the
       * validation fails.
       */
      def validateToEnd {
         var prevTag = tag
         var entry   = next
         while( !entry.isEnd ) {
            assert( entry.tag > prevTag, "has tag " + entry.tag + ", while previous elem has tag " + prevTag )
            prevTag  = entry.tag
            entry    = entry.next
         }
      }

      /**
       * Debugging method: Returns a list of the tags
       * from this entry to the end of the list
       */
      def tagList : List[ Int ] = {
         val b       = List.newBuilder[ Int ]
         var entry : Entry = this
         while( !entry.isEnd ) {
            b += entry.tag
            entry    = entry.next
         }
         b.result()
      }
   }

//   /**
//    * Compares the positions of x and y in the sequence
//   */
//   def compare( that: TotalOrder ) : Int

   /**
    * Appends an element to the end of the sequence, and returns
    * that new sequence tail. Note that this operation takes O(n).
    *
    * @param   elem  the element to append
    * @return  the total order entry corresponding to the newly appended element
    */
//   def append() : TotalOrder

//   def max : TotalOrder

   def size : Int = sizeVar

   /**
    * Relabels from a this entry to clean up collisions with
    * its successors' tags.
    *
    * Original remark from Eppstein:
    * "At each iteration of the rebalancing algorithm, we look at
    * a contiguous subsequence of items, defined as the items for which
    * self._tag &~ mask == base.  We keep track of the first and last
    * items in the subsequence, and the number of items, until we find
    * a subsequence with sufficiently low density, at which point
    * we space the tags evenly throughout the available values.
    *
    * The multiplier controls the growth of the threshhold density;
    * it is 2/T for the T parameter described by Bender et al.
    * Large multipliers lead to fewer relabels, while small items allow
    * us to handle more items with machine integer tags, so we vary the
    * multiplier dynamically to allow it to be as large as possible
    * without producing integer overflows."
    */
   protected def relabel( _first: Entry ) {
      var base       = _first.tag
      var mask       = -1
      var thresh     = 1.0
      var first : Entry = _first
      var last : Entry = _first
      var num        = 1
//      val mul     = 2/((2*len(self))**(1/30.))
      val mul        = 2 / math.pow( size << 1, 1/30.0 )
//println( "relabel" )
      do {
//println( "   -mask " + -mask )
         while( !first.isHead && ((first.prev.tag & mask) == base) ) {
            first = first.prev
            num  += 1
         }
         while( !last.isLast && ((last.next.tag & mask) == base) ) {
            last = last.next
            num += 1
         }
//         val inc = (mask + 1) / num
         val inc = -mask / num
         if( inc >= thresh ) {   // found rebalanceable range
            observer.beforeRelabeling( first, num )
            var item = first
//            while( !(item eq last) ) {
            // Note: this was probably a bug in Eppstein's code
            // -- it ran for one iteration less which made
            // the test suite fail for very dense tags. it
            // seems now it is correct with the inclusion
            // of last in the tag updating.
            var cnt = 0; while( cnt < num ) {
               item.tag   = base
               item       = item.next
               base      += inc
               cnt += 1
            }
            observer.afterRelabeling( first, num )
            return
         }
//         mask     = (mask << 1) + 1    // expand to next power of two
         mask   <<= 1      // next coarse step
         base    &= mask
         thresh  *= mul
      } while( mask != 0 )
      throw new RuntimeException( "label overflow" )
   }
}