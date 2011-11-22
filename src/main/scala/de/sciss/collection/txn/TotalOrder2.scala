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
 */

package de.sciss.collection
package txn

import de.sciss.lucrestm.{Disposable, DataOutput, DataInput, Serializer, Sys}


/**
 * A transactional data structure to maintain an ordered sequence of elements such
 * that two random elements can be compared in O(1).
 *
 * This uses an algorithm from the paper
 * Bender, M. and Cole, R. and Demaine, E. and Farach-Colton, M. and Zito, J.,
 * Two simplified algorithms for maintaining order in a list,
 * Algorithms—ESA 2002, pp. 219--223, 2002.
 *
 * The `relabel` method is based on the Python implementation by
 * David Eppstein, as published at http://www.ics.uci.edu/~eppstein/PADS/OrderedSequence.py
 * however a bug resulting in a relabel size of 1 was fixed.
 *
 * Original note: "Due to rebalancing on the integer tags used to maintain order,
 * the amortized time per insertion in an n-item list is O(log n)."
 */
object TotalOrder2 {
   sealed trait Entry[ S <: Sys[ S ], Repr ] extends Disposable[ S#Tx ] /* extends Ordered[ Entry ] */ {
      def tag( implicit tx: S#Tx ) : Int

      def prev( implicit tx: S#Tx ) : Repr
      def next( implicit tx: S#Tx ) : Repr

      private[TotalOrder2] def tagRef  : S#Ref[ Int ]
      private[TotalOrder2] def prevRef : S#Ref[ S#Mut[ Repr ]]
      private[TotalOrder2] def nextRef : S#Ref[ S#Mut[ Repr ]]

      /**
       * Removes and disposes this element from the order.
       */
      def dispose()( implicit tx: S#Tx ) : Unit

      /**
       * Debugging method: Returns a list of the tags
       * from this entry to the end of the list
       */
      def tagList( implicit tx: S#Tx ) : List[ Int ]
   }

   sealed trait SetEntry[ S <: Sys[ S ]] extends Entry[ S, SetEntry[ S ]] {
      def append()(  implicit tx: S#Tx ) : S#Mut[ SetEntry[ S ]]
      def prepend()( implicit tx: S#Tx ) : S#Mut[ SetEntry[ S ]]
   }

   sealed trait AssocEntry[ S <: Sys[ S ], @specialized( Int, Long ) A ] extends Entry[ S, AssocEntry[ S, A ]] {
      /**
       * Returns the 'payload' of the node.
       */
      def value( implicit tx: S#Tx ) : A
//      /**
//       * Inserts a new element after this node.
//       *
//       * @param   a  the 'payload' for the new node
//       */
//      def append( value: A )( implicit tx: S#Tx ) : AssocEntry[ S, A ]
//      /**
//       * Inserts a new element before this node.
//       *
//       * @param   a  the 'payload' for the new node
//       */
//      def prepend( value: A )( implicit tx: S#Tx ) : AssocEntry[ S, A ]
   }

   trait RelabelObserver[ -Tx, -E ] {
      def beforeRelabeling( first: E, num: Int )( implicit tx: Tx ) : Unit
      def afterRelabeling(  first: E, num: Int )( implicit tx: Tx ) : Unit
   }

   object NoRelabelObserver extends RelabelObserver[ Any, Any ] {
      def beforeRelabeling( first: Any, num: Int )( implicit tx: Any ) {}
      def afterRelabeling(  first: Any, num: Int )( implicit tx: Any ) {}
   }

   private sealed trait BasicImpl[ S <: Sys[ S ], E <: Entry[ S, E ]] {
      me: TotalOrder2[ S, E ] =>

      protected def sizeRef : S#Ref[ Int ]
      protected def observer : RelabelObserver[ S#Tx, E ]

      final def size( implicit tx: S#Tx ) : Int = sizeRef.get

      final def head( implicit tx: S#Tx ) : E = {
         var e    = root.get
         var prev = e.prev
         while( prev != null ) {
            e     = prev
            prev  = prev.prev
         }
         e
      }

      final def tagList( _entry: E )( implicit tx: S#Tx ) : List[ Int ] = {
         val b       = List.newBuilder[ Int ]
         var entry   = _entry
         while( entry != null ) {
            b       += entry.tag
            entry    = entry.next
         }
         b.result()
      }

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
      protected final def relabel( _first: E )( implicit tx: S#Tx ) {
         var mask       = -1
         var thresh     = 1.0
         var num        = 1
   //      val mul     = 2/((2*len(self))**(1/30.))
         val mul        = 2 / math.pow( size << 1, 1/30.0 )
         var first      = _first
         var last       = _first
         var base       = _first.tag
         do {
            var prev    = first.prev
            while( (prev != null) && ((prev.tag & mask) == base) ) {
               first    = prev
               prev     = prev.prev
               num     += 1
            }
            var next    = last.next
            while( (next != null) && ((next.tag & mask) == base) ) {
               last     = next
               next     = next.next
               num     += 1
            }
   //         val inc = (mask + 1) / num
            val inc = -mask / num

            // important: we found a corner case where _first is the last
            // element in the list with a value of 0x7FFFFFFF. in this
            // case, if the predecessor is smaller in value, the original
            // algorithm would immediately terminate with num == 1, which
            // will obviously leave the tag unchanged! thus we must add
            // the additional condition that num is greater than 1!
            if( (inc >= thresh) && (num > 1) ) {   // found rebalanceable range
               observer.beforeRelabeling( first, num )
   //            while( !(item eq last) ) {
               // Note: this was probably a bug in Eppstein's code
               // -- it ran for one iteration less which made
               // the test suite fail for very dense tags. it
               // seems now it is correct with the inclusion
               // of last in the tag updating.
               next = first
               var cnt = 0; while( cnt < num ) {
                  next.tagRef.set( base )
                  next        = next.next
                  base       += inc
                  cnt        += 1
               }
               observer.afterRelabeling( first, num )
               return
            }
            mask   <<= 1      // next coarse step
            base    &= mask
            thresh  *= mul
         } while( mask != 0 )
         sys.error( "label overflow" )
      }
   }

   private final class SetEntryImpl[ S <: Sys[ S ]]( impl: SetImpl[ S ], private[TotalOrder2] val tagRef: S#Ref[ Int ],
                                                     private[TotalOrder2] val prevRef: S#Ref[ SetEntry[ S ]],
                                                     private[TotalOrder2] val nextRef: S#Ref[ SetEntry[ S ]])
   extends SetEntry[ S ] {
      private type E = SetEntry[ S ]

      def tag( implicit tx: S#Tx )  : Int = tagRef.get
      def prev( implicit tx: S#Tx ) : E   = prevRef.get
      def next( implicit tx: S#Tx ) : E   = nextRef.get

//      private[TotalOrder2] def tag_=( value: Int )( implicit tx: S#Tx ) { tagRef.set( value )}
//      private[TotalOrder2] def prev_=( e: E )( implicit tx: S#Tx ) { prevRef.set( e )}
//      private[TotalOrder2] def next_=( e: E )( implicit tx: S#Tx ) { nextRef.set( e )}

//      def append()(  implicit tx: S#Tx ) : S#Mut[ SetEntry[ S ]] = impl.append( this )
//      def prepend()( implicit tx: S#Tx ) : S#Mut[ SetEntry[ S ]] = impl.prepend( this )
//
//      def remove()( implicit tx: S#Tx ) { impl.remove( this )}
//
//      def tagList( implicit tx: S#Tx ) : List[ Int ] = impl.tagList( this )
   }

//   private final class AssocEntryViewImpl[ S <: Sys[ S ], @specialized( Int, Long ) A ]( val tag: Int, val value : A,
//                                                     private[TotalOrder2] val prevRef: S#Ref[ AssocEntry.View[ S, A ]],
//                                                     private[TotalOrder2] val nextRef: S#Ref[ AssocEntry.View[ S, A ]])
//   extends AssocEntry.View[ S, A ] {
//      def prev( implicit tx: S#Tx ) : AssocEntry.View[ S, A ] = prevRef.get
//      def next( implicit tx: S#Tx ) : AssocEntry.View[ S, A ] = nextRef.get
//   }
//
//   private final class AssocEntryImpl[ S <: Sys[ S ], @specialized( Int, Long ) A ]( impl: AssocImpl[ S ],
//                                                                                     ref: S#Ref[ AssocEntry.View[ S, A ]])
//   extends AssocEntry[ S, A ] {
//      def append( a: A )(  implicit tx: S#Tx ) : SetEntry[ S ] = impl.append( this, a )
//      def prepend( a: A )( implicit tx: S#Tx ) : SetEntry[ S ] = impl.prepend( this, a )
//   }

   private final class SetImpl[ S <: Sys[ S ]]( protected val observer: RelabelObserver[ S#Tx, SetEntry[ S ]],
                                                protected val sizeRef: S#Ref[ Int ],
                                                _rootFun: SetImpl[ S ] => SetEntry[ S ])
                                              ( implicit val system: S )
   extends Set[ S ] with BasicImpl[ S, SetEntry[ S ]] with Serializer[ SetEntry[ S ]] {

      private type E = SetEntry[ S ]

      val root = _rootFun( this )

      implicit def impl = this

      def read( in: DataInput ) : E = {
         if( in.readUnsignedByte() == 1 ) {
            val tagRef  = system.readRef[ Int ]( in )
            val prevRef = system.readRef[ E ](   in )
            val nextRef = system.readRef[ E ](   in )
            new SetEntryImpl[ S ]( this, tagRef, prevRef, nextRef )
         } else {
            null
         }
      }
      def write( e: E, out: DataOutput ) {
         if( e != null ) {
            out.writeUnsignedByte( 1 )
//            system.writeRef( e.tagRef,  out )
//            system.writeRef( e.prevRef, out )
//            system.writeRef( e.nextRef, out )
            e.tagRef.write( out )
            e.prevRef.write( out )
            e.nextRef.write( out )
         } else {
            out.writeUnsignedByte( 0 )
         }
      }

      def append( prevM: S#Mut[ E ])( implicit tx: S#Tx ) : S#Mut[ E ] = {
         val prev       = prevM.get
         val nextM      = prev.nextRef.get
         val next       = if( nextM == null ) null else nextM.get
         val nextTag    = if( next == null ) Int.MaxValue else next.tag
         val recPrevRef = system.newRef[ S#Mut[ E ]]( prevM )
         val recNextRef = system.newRef[ S#Mut[ E ]]( nextM )
         val prevTag    = prev.tag
         val recTag     = prevTag + ((nextTag - prevTag + 1) >>> 1)
         val recTagRef  = system.newRef[ Int ]( recTag )
         val rec        = new SetEntryImpl[ S ]( this, recTagRef, recPrevRef, recNextRef )
         val recM       = system.newMut[ E ]( rec )
         prev.nextRef.set( recM )
         if( next != null ) next.prevRef.set( recM )
         sizeRef.transform( _ + 1 )
         if( recTag == nextTag ) relabel( rec )
         recM
      }

      def prepend( nextM: S#Mut[ E ])( implicit tx: S#Tx ) : E = {
         val next       = nextM.get
         val prevM      = next.prevRef.get
         val prev       = if( prevM == null ) null else prevM.get
         val prevTag    = if( prev == null ) 0 else prev.tag
         val recPrevRef = system.newRef[ S#Mut[ E ]]( prevM )
         val recNextRef = system.newRef[ S#Mut[ E ]]( nextM )
         val nextTag    = next.tag
         val recTag     = prevTag + ((nextTag - prevTag + 1) >>> 1)
         val recTagRef  = system.newRef[ Int ]( recTag )
         val rec        = new SetEntryImpl[ S ]( this, recTagRef, recPrevRef, recNextRef )
         val recM       = system.newMut[ E ]( rec )
         next.prevRef.set( recM )
         if( prev != null ) prev.nextRef.set( recM )
         sizeRef.transform( _ + 1 )
         if( recTag == nextTag ) relabel( rec )
         rec
      }

      def remove( rec: E )( implicit tx: S#Tx ) {
         val prevM   = rec.prevRef.get
         val nextM   = rec.nextRef.get
         val prev    = if( prevM == null ) null else prevM.get
         val next    = if( nextM == null ) null else nextM.get
         if( prev != null ) prev.nextRef.set( nextM )
         if( next != null ) next.prevRef.set( prevM )
//         system.disposeRef( rec.tagRef )
//         system.disposeRef( rec.prevRef )
//         system.disposeRef( rec.nextRef )
         rec.tagRef.dispose()
         rec.prevRef.dispose()
         rec.nextRef.dispose()
         sizeRef.transform( _ - 1 )
      }
   }

////         /**
////          * Compares the positions of x and y in the sequence
////         */
////         def compare( that: Entry ) : Int = {
////            val thatTag = that.tag
////            if( tag < thatTag ) -1 else if( tag > thatTag ) 1 else 0
////         }

   type Set[ S <: Sys[ S ]]         = TotalOrder2[ S, SetEntry[ S ]]
//   type Assoc[ S <: Sys[ S ], A ]   = TotalOrder2[ S, AssocEntry[ S, A ]]

   def empty[ S <: Sys[ S ]]( relabelObserver: RelabelObserver[ S#Tx, SetEntry[ S ]] = NoRelabelObserver )
                            ( implicit tx: S#Tx, system: S ) : Set[ S ] = {

      new SetImpl[ S ]( relabelObserver, system.newRef( 1 ), { implicit impl =>
         val tagRef  = system.newRef[ Int ]( 0 )
         val prevRef = system.newRef[ SetEntry[ S ]]( null )
         val nextRef = system.newRef[ SetEntry[ S ]]( null )
         new SetEntryImpl[ S ]( impl, tagRef, prevRef, nextRef )
      })
   }

//   def emptyAssoc[ S <: Sys[ S ], A ]( relabelObserver: RelabelObserver[ S#Tx, AssocEntry[ S, A ]] = NoRelabelObserver )
//                                     ( implicit tx: S#Tx, system: S ) : Assoc[ S, A ] = sys.error( "TODO" )
}
sealed trait TotalOrder2[ S <: Sys[ S ], E ] {
   def system: S

//   /**
//    * Return the 'tail' of order, that is, the element which is higher
//    * than all user elements. It can be used for comparisons.
//    */
//   def max : E // Entry[ S ]

   /**
    * The initial element created from which you can start to append and prepend.
    */
   def root : S#Mut[ E ] // Entry[ S ]

   /**
    * Returns the head element of the structure. Note that this
    * is O(n) worst case.
    */
   def head( implicit tx: S#Tx ) : E // Entry[ S ]

   /**
    * The number of elements in the order. This is `1` for a newly
    * created order (consisting only of the root element).
    * You will rarely need this information except for debugging
    * purpose. The operation is O(1).
    */
   def size( implicit tx: S#Tx ) : Int
}