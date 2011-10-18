package de.sciss.collection.mutable

/*
 *  TotalOrder2.scala
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

import collection.mutable.{ Builder, DoubleLinkedListLike, LinearSeq => MLinearSeq}
import collection.generic.{SeqFactory, GenericCompanion, GenericTraversableTemplate}
import sys.error // suckers

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
object TotalOrder2 extends SeqFactory[ TotalOrder2 ] {
//   type Tag = Int // Long

//   class Record[ T ]( private[ TotalOrder2 ] var v: Tag ) {
//      private[ TotalOrder2 ] var succ: Record[ T ] = null
//      private[ TotalOrder2 ] var pred: Record[ T ] = null
//   }

// We don't need to define an ordering -- this will already be implicitly
// derived from any TotalOrder2
//   implicit def ordering[ V ] = new Ordering[ TotalOrder2[ V ]] {
//      def compare( x: TotalOrder2[ V ], y: TotalOrder2[ V ]) = x.compare( y )
//   }

   /**
    * Returns a single element order corresponding to the tag ceiling. This
    * can be used for comparison (using its `Ordered` trait).
    *
    * Note: you can not add elements before or after this one.
    */
   def max[ A ] : TotalOrder2[ A ] = new Impl[ A ]( new Size ) {
      tag = Int.MaxValue
      override def append( elem: A ) : T = unsupportedOp
      override def insertAfter( elem: A ) : T = unsupportedOp
      override def insertBefore( elem: A ) : T = unsupportedOp
      def unsupportedOp = error( "Operation not permitted" )
   }

   // ---- GenericCompanion ----
  def newBuilder[ A ] = new Builder[ A, TotalOrder2[ A ]] {
      def emptyList() : TotalOrder2[ A ] = new Impl[ A ]( new Size )
      var head = emptyList()
      var tail = head

      def +=( elem: A ) : this.type = {
         tail = tail.append( elem )
//
//         if( head.isEmpty ) {
//            head  = new TotalOrder2( elem, 0x3FFFFFFF, head /* emptyList() */ )
//            tail  = head
//         } else {
//            tail  = tail.append( elem )
////            val tag: Int = error( "TODO" )
////            current.append( new TotalOrder2( elem, tag, emptyList() ))
//         }
         this
      }

      def clear() {
         head = emptyList()
         tail = head
      }

      def result() = head
   }

   class Size {
      private var v: Int = 0
      def value : Int = v
      def inc { v += 1 }
   }

   private class Impl[ A ]( protected val totalSize: TotalOrder2.Size ) // (_t: Int)
   extends TotalOrder2[ A ] {
//      private type T = TotalOrder2[ A ]

      next = this
      var tag: Int = 0 // _t

      def this( sz: TotalOrder2.Size, elem: A, prev: TotalOrder2[ A ], next: TotalOrder2[ A ]) {
         this( sz )
         sz.inc
         this.elem      = elem
         this.prev      = prev
         if( prev != null ) {
            require( prev.nonEmpty && (prev.next eq next) )
            prev.next = this
         } else {
            require( next.isHead )
         }
         this.next      = next
         next.prev      = this
      }

   //   type Rec = Record[ T ]
   //
   //   private var base : Rec = null

      def isHead : Boolean = prev == null
      def isLast : Boolean = next.isEmpty

      /**
       * Compares the positions of x and y in the sequence
      */
      def compare( that: T ) : Int = tag compare that.tag

      /**
       * Appends an element to the end of the sequence, and returns
       * that new sequence tail. Note that this operation takes O(n).
       *
       * @param   elem  the element to append
       * @return  the total order entry corresponding to the newly appended element
       */
      def append( elem: A ) : T = {
         if( isEmpty ) {
            this.elem   = elem
            tag         = Int.MaxValue >> 1
            val empty : T = new Impl( totalSize )
            totalSize.inc
            next        = empty
            empty.prev  = this
            this
         } else {
            lastNode.insertAfter( elem )
         }
      }

      private def lastNode : T = {
         var res = next
         while( !res.isEmpty ) res = res.next
         res.prev
      }

      def insertAfter( elem: A ) : T = {
         val rec     = new Impl( totalSize, elem, this, next )
         val nextTag = if( rec.isLast ) Int.MaxValue else rec.next.tag
         rec.tag     = tag + ((nextTag - tag + 1) >>> 1)
         if( rec.tag == nextTag ) rec.relabel
         rec
      }

      def insertBefore( elem: A ) : T = {
//         if( isHead ) {
// THIS WAS A CRAPPY IDEA -- IT MEANS PREVIOUSLY RETRIEVED ENTRIES ARE UNSTABLE
//            // to maintain references to the 'head' of the list,
//            // in the case when an element is inserted at the
//            // head of the list, we instead change this entry's
//            // elem and tag, and a new successor is inserted
//            // after this head
//            val rec     = new Impl( totalSize, this.elem, this, next )
//            this.elem   = elem
//            rec.tag     = tag
//            tag         = (tag + 1) >> 1
//            if( tag == rec.tag ) this.relabel
//            this
//         } else {
            val rec     = new Impl( totalSize, elem, prev, this )
//         val prevTag = rec.prev.tag
            val prevTag = if( rec.isHead ) 0 else rec.prev.tag
            rec.tag     = prevTag + ((tag - prevTag + 1) >>> 1)
            if( rec.tag == tag ) rec.relabel
            rec
//         }
      }

//      /**
//       * Relabels from a this entry to clean up collisions with
//       * its successors' tags.
//       *
//       * Naive implementation.
//       */
//      protected def relabelSIMPLE {
//         var base = this.tag // 0
//         val inc  = (Int.MaxValue - base) / size
//         require( inc > 0, "label overflow" )
//   //println( "relabel (" + inc + ")" )
//         var entry = this
//         while( entry.nonEmpty ) {
//            entry.tag = base
//            base += inc
//            entry = entry.next
//         }
//      }

      protected def relabel {
         var base       = tag
         var mask       = -1
         var thresh     = 1.0
         var first : T  = this
         var last : T   = this
         var num        = 1
   //      val mul     = 2/((2*len(self))**(1/30.))
         val mul        = 2 / math.pow( totalSize.value << 1, 1/30.0 )
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
               return
            }
   //         mask     = (mask << 1) + 1    // expand to next power of two
            mask   <<= 1      // next coarse step
            base    &= mask
            thresh  *= mul
         } while( mask != 0 )
         throw new RuntimeException( "label overflow" )
      }

      def validateToEnd {
         var prevTag = tag
         var entry   = next
         while( entry.nonEmpty ) {
            assert( entry.tag > prevTag, "elem " + entry.elem + " at index " + indexOf( entry.elem ) + " has tag " + entry.tag +
               ", while previous elem has tag " + prevTag )
            prevTag  = entry.tag
            entry    = entry.next
         }
      }

      def tagList : List[ Int ] = {
         val b       = List.newBuilder[ Int ]
         var entry : T = this
         while( entry.nonEmpty ) {
            b += entry.tag
            entry    = entry.next
         }
         b.result()
      }
   }
}

sealed trait TotalOrder2[ A ]
extends MLinearSeq[ A ]
with GenericTraversableTemplate[ A, TotalOrder2 ]
with DoubleLinkedListLike[ A, TotalOrder2[ A ]] with Ordered[ TotalOrder2[ A ]] {
   def isHead : Boolean
   def isLast : Boolean

   private type T = TotalOrder2[ A ]

   // ---- GenericTraversableTemplate ----
   override def companion: GenericCompanion[ TotalOrder2 ] = TotalOrder2


   /**
    * Compares the positions of x and y in the sequence
   */
   def compare( that: T ) : Int

   /**
    * Appends an element to the end of the sequence, and returns
    * that new sequence tail. Note that this operation takes O(n).
    *
    * @param   elem  the element to append
    * @return  the total order entry corresponding to the newly appended element
    */
   def append( elem: A ) : T

   /**
    * Inserts a new element after this node
    */
   def insertAfter( elem: A ) : T

   /**
    * Inserts a new element before this node
    */
   def insertBefore( elem: A ) : T

   /**
    * Debugging method: Validates that the list from this entry
    * to the end has monotonically increasing
    * tags. Throws an assertion error if the
    * validation fails.
    */
   def validateToEnd : Unit

   /**
    * Debugging method: Returns a list of the tags
    * from this entry to the end of the list
    */
   def tagList : List[ Int ]

   protected def totalSize : TotalOrder2.Size

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
   protected def relabel : Unit

//   protected def tag: Int
   def tag: Int
   protected def tag_=( i: Int ) : Unit
}

object TotalOrder2Test extends App {
   val to    = TotalOrder2[ Int ]()
   val rnd   = new util.Random( 0 )
   val n     = 3042 // 3041
   var pred  = to.append( 0 )
   for( i <- 1 until n ) {
      if( i == n - 1 ) {
         println( "last" )
//         TotalOrder2.flonky = true
      }
      pred   = if( rnd.nextBoolean() ) {
         pred.insertAfter( i )
      } else {
         pred.insertBefore( i )
      }
   }
   to.validateToEnd
   println( "OK." )
}