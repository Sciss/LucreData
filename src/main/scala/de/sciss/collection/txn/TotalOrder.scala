///*
// *  TotalOrder.scala
// *  (TreeTests)
// *
// *  Copyright (c) 2011 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is free software; you can redistribute it and/or
// *  modify it under the terms of the GNU General Public License
// *  as published by the Free Software Foundation; either
// *  version 2, june 1991 of the License, or (at your option) any later version.
// *
// *  This software is distributed in the hope that it will be useful,
// *  but WITHOUT ANY WARRANTY; without even the implied warranty of
// *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
// *  General Public License for more details.
// *
// *  You should have received a copy of the GNU General Public
// *  License (gpl.txt) along with this software; if not, write to the Free Software
// *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.collection
//package txn
//
//import de.sciss.lucrestm.Sys
//
///**
// * A transactional data structure to maintain an ordered sequence of elements such
// * that two random elements can be compared in O(1).
// *
// * This uses an algorithm from the paper
// * Bender, M. and Cole, R. and Demaine, E. and Farach-Colton, M. and Zito, J.,
// * Two simplified algorithms for maintaining order in a list,
// * Algorithms—ESA 2002, pp. 219--223, 2002.
// *
// * The `relabel` method is based on the Python implementation by
// * David Eppstein, as published at http://www.ics.uci.edu/~eppstein/PADS/OrderedSequence.py
// * however a bug resulting in a relabel size of 1 was fixed.
// *
// * Original note: "Due to rebalancing on the integer tags used to maintain order,
// * the amortized time per insertion in an n-item list is O(log n)."
// */
//object TotalOrder {
//   def apply[ S <: Sys[ S ]]( relabelObserver: RelabelObserver[ S ] = NoRelabelObserver )
//                            ( implicit tx: S#Tx, system: S ) : TotalOrder[ S ] = {
//
//      new Impl( relabelObserver )
//   }
//
////   object Ordering extends de.sciss.collection.Ordering[ Entry ] {
////      /**
////       * Compares the positions of `a` and `b` in the sequence
////      */
////      def compare( a: Entry, b: Entry ) : Int = {
////         val atag = a.tag
////         val btag = b.tag
////         if( atag < btag ) -1 else if( atag > btag ) 1 else 0
////      }
////   }
//
//   object Entry {
//      implicit def Ordering[ S <: Sys[ S ]] : de.sciss.collection.Ordering[ Entry[ S ]] = new Ord[ S ]
//      private final class Ord[ S <: Sys[ S ]] extends de.sciss.collection.Ordering[ Entry[ S ]] {
//         /**
//          * Compares the positions of `a` and `b` in the sequence
//         */
//         def compare( a: Entry[ S ], b: Entry[ S ]) : Int = {
//            val atag = a.tag
//            val btag = b.tag
//            if( atag < btag ) -1 else if( atag > btag ) 1 else 0
//         }
//      }
//   }
//   sealed trait Entry[ S <: Sys[ S ]] /* extends Ordered[ Entry ] */ {
//      def prev : Entry[ S ]
//      def next : Entry[ S ]
//      def tag : Int
//
//      def isHead : Boolean
//      def isLast : Boolean
//      def isEnd  : Boolean
//
//      /**
//       * Inserts a new element after this node.
//       */
//      def append()( implicit tx: S#Tx ) : Entry[ S ]
//
//      /**
//       * Inserts a new element before this node.
//       */
//      def prepend()( implicit tx: S#Tx ) : Entry[ S ]
//
//      /**
//       * Removes and disposes this element from the order.
//       */
//      def remove() : Unit
//
//      /**
//       * Debugging method: Returns a list of the tags
//       * from this entry to the end of the list
//       */
//      def tagList : List[ Int ]
//   }
//
//   trait RelabelObserver[ S <: Sys[ S ]] {
//      def beforeRelabeling( first: Entry[ S ], num: Int )( implicit tx: S#Tx ) : Unit
//      def afterRelabeling( first: Entry[ S ], num: Int )( implicit tx: S#Tx ) : Unit
//   }
//
//   def NoRelabelObserver[ S <: Sys[ S ]] : RelabelObserver[ S ] = new NoRelabelObserver[ S ]
//   private final class NoRelabelObserver[ S <: Sys[ S ]] extends RelabelObserver[ S ] {
//      def beforeRelabeling( first: Entry[ S ], num: Int )( implicit tx: S#Tx ) {}
//      def afterRelabeling( first: Entry[ S ], num: Int )( implicit tx: S#Tx ) {}
//   }
//
//   private final class Impl[ S <: Sys[ S ]]( val observer: RelabelObserver[ S ])( implicit val system: S )
//   extends TotalOrder[ S ] {
////      private var sizeVar : Int = 1
//      private var sizeVar : Int = 1 // root!
//
//      val max : EntryImpl = {
//         val e = new EntryImpl()
//         e.tag = Int.MaxValue
//         e
//      }
//
//      val root : EntryImpl = {
//         val head    = new EntryImpl()
//         head.next   = max
//         max.prev    = head
//         head
//      }
//
//      def head( implicit tx: S#Tx ) : Entry[ S ] = {
//         var e = root
//         while( !e.isHead ) e = e.prev
//         e
//      }
//
//      def size( implicit tx: S#Tx ) : Int = sizeVar
//
//      /*
//       * Relabels from a this entry to clean up collisions with
//       * its successors' tags.
//       *
//       * Original remark from Eppstein:
//       * "At each iteration of the rebalancing algorithm, we look at
//       * a contiguous subsequence of items, defined as the items for which
//       * self._tag &~ mask == base.  We keep track of the first and last
//       * items in the subsequence, and the number of items, until we find
//       * a subsequence with sufficiently low density, at which point
//       * we space the tags evenly throughout the available values.
//       *
//       * The multiplier controls the growth of the threshhold density;
//       * it is 2/T for the T parameter described by Bender et al.
//       * Large multipliers lead to fewer relabels, while small items allow
//       * us to handle more items with machine integer tags, so we vary the
//       * multiplier dynamically to allow it to be as large as possible
//       * without producing integer overflows."
//       */
//      private def relabel( _first: EntryImpl )( implicit tx: S#Tx ) {
//         var base       = _first.tag
//         var mask       = -1
//         var thresh     = 1.0
//         var first : EntryImpl = _first
//         var last : EntryImpl = _first
//         var num        = 1
//   //      val mul     = 2/((2*len(self))**(1/30.))
//         val mul        = 2 / math.pow( size << 1, 1/30.0 )
//   //println( "relabel" )
//         do {
//   //println( "   -mask " + -mask )
//            while( !first.isHead && ((first.prev.tag & mask) == base) ) {
//               first = first.prev
//               num  += 1
//            }
//            while( !last.isLast && ((last.next.tag & mask) == base) ) {
//               last = last.next
//               num += 1
//            }
//   //         val inc = (mask + 1) / num
//            val inc = -mask / num
//
//            // important: we found a corner case where _first is the last
//            // element in the list with a value of 0x7FFFFFFF. in this
//            // case, if the predecessor is smaller in value, the original
//            // algorithm would immediately terminate with num == 1, which
//            // will obviously leave the tag unchanged! thus we must add
//            // the additional condition that num is greater than 1!
//            if( (inc >= thresh) && (num > 1) ) {   // found rebalanceable range
//               observer.beforeRelabeling( first, num )
//               var item = first
//   //            while( !(item eq last) ) {
//               // Note: this was probably a bug in Eppstein's code
//               // -- it ran for one iteration less which made
//               // the test suite fail for very dense tags. it
//               // seems now it is correct with the inclusion
//               // of last in the tag updating.
//               var cnt = 0; while( cnt < num ) {
//                  item.tag   = base
//                  item       = item.next
//                  base      += inc
//                  cnt += 1
//               }
//               observer.afterRelabeling( first, num )
//               return
//            }
//   //         mask     = (mask << 1) + 1    // expand to next power of two
//            mask   <<= 1      // next coarse step
//            base    &= mask
//            thresh  *= mul
//         } while( mask != 0 )
//         sys.error( "label overflow" )
//      }
//
//      private def insertEntry( prev: Entry, next: Entry )( implicit tx: S#Tx ) : EntryImpl = {
//         val prevRef = system.newRef( prev )
//         val nextRef = system.newref( next )
//         sizeVar    += 1
//         val res     = new EntryImpl( prevRef, nextRef )
//         res
//      }
//
//      private final case class EntryData( tag: Int, prev: Entry, next: Entry )
//
//// important: maintain default equals (reference equality)
//      private final class EntryImpl( tagRef: S#Ref[ Int ], prevRef: S#Ref[ Entry ], nextRef: S#Ref[ Entry ])
//      extends Entry[ S ] {
//         private var tagVar : Int = 0
//         private var prevVar : EntryImpl = _
//         private var nextVar : EntryImpl = this
//
//         private def this( prev: EntryImpl, next: EntryImpl ) {
//            this()
//            sizeVar += 1
//            prevVar = prev
//            if( prev != null ) {
//               require( !prev.isEnd && (prev.next eq next) )
//               prev.next = this
//            } else {
//               require( next.isHead )
//            }
//            nextVar     = next
//            next.prev   = this
//         }
//
//         def isHead : Boolean = prev == null
//         def isLast : Boolean = next.isEnd
//         def isEnd  : Boolean = next eq this
//
//         def remove() {
//            require( nextVar != null, "Entry already removed" )
//            next.prev = prevVar
//            if( prevVar != null ) {
//               prevVar.next   = nextVar
//               prevVar        = null
//            }
//            nextVar = null
//         }
//
////         /**
////          * Compares the positions of x and y in the sequence
////         */
////         def compare( that: Entry ) : Int = {
////            val thatTag = that.tag
////            if( tag < thatTag ) -1 else if( tag > thatTag ) 1 else 0
////         }
//
//         override def toString = "Tag(" + tagVar + ")"
//
//         def tag_=( value: Int ) { tagVar = value }
//         def next_=( entry: EntryImpl ) { nextVar = entry }
//         def prev_=( entry: EntryImpl ) { prevVar = entry }
//
//         def prev : EntryImpl = prevVar
//         def next : EntryImpl = nextVar
//         def tag  : Int       = tagVar
//
//         def append()( implicit tx: S#Tx ) : EntryImpl = {
//            require( !isEnd )
//            val rec     = new EntryImpl( this, next )
//            // this condition is now fulfilled by the list ending in `max` !
//   //         val nextTag = if( rec.isLast ) Int.MaxValue else rec.next.tag
//            val nextTag = rec.next.tag
//            rec.tag     = tag + ((nextTag - tag + 1) >>> 1)
//            if( rec.tag == nextTag ) relabel( rec )
//            rec
//         }
//
//         def prepend()( implicit tx: S#Tx ) : EntryImpl = {
//            val rec     = new EntryImpl( prev, this )
//            val prevTag = if( rec.isHead ) 0 else rec.prev.tag
//            rec.tag     = prevTag + ((tag - prevTag + 1) >>> 1)
//            if( rec.tag == tag ) relabel( rec )
//            rec
//         }
//
//         /**
//          * Debugging method: Validates that the list from this entry
//          * to the end has monotonically increasing
//          * tags. Throws an assertion error if the
//          * validation fails.
//          */
//         def validateToEnd {
//            var prevTag = tag
//            var entry   = next
//            while( !entry.isEnd ) {
//               assert( entry.tag > prevTag, "has tag " + entry.tag + ", while previous elem has tag " + prevTag )
//               prevTag  = entry.tag
//               entry    = entry.next
//            }
//         }
//
//         def tagList : List[ Int ] = {
//            val b       = List.newBuilder[ Int ]
//            var entry : EntryImpl = this
//            while( !entry.isEnd ) {
//               b += entry.tag
//               entry    = entry.next
//            }
//            b.result()
//         }
//      }
//   }
//}
//sealed trait TotalOrder[ S <: Sys[ S ]] {
//   import TotalOrder._
//
//   def system: S
//
//   /**
//    * Return the 'tail' of order, that is, the element which is higher
//    * than all user elements. It can be used for comparisons.
//    */
//   def max : Entry[ S ]
//
//   /**
//    * The initial element created from which you can start to append and prepend.
//    */
//   def root : Entry[ S ]
//
//   /**
//    * Returns the head element of the structure. Note that this
//    * is O(n) worst case.
//    */
//   def head( implicit tx: S#Tx ) : Entry[ S ]
//
//   /**
//    * The number of elements in the order. This is `1` for a newly
//    * created order (consisting only of the root element).
//    * You will rarely need this information except for debugging
//    * purpose. The operation is O(1).
//    */
//   def size( implicit tx: S#Tx ) : Int
//}