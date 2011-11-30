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
 */

package de.sciss.collection
package txn

import de.sciss.lucrestm.{MutableReader, DataInput, EmptyMutable, MutableOptionReader, MutableOption, DataOutput, Mutable, Sys}


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
object TotalOrder {
   private val SER_VERSION = 0

//   trait RelabelObserver[ -Tx, -E ] {
//      def beforeRelabeling( first: E, num: Int )( implicit tx: Tx ) : Unit
//      def afterRelabeling(  first: E, num: Int )( implicit tx: Tx ) : Unit
//   }
//
//   object NoRelabelObserver extends RelabelObserver[ Any, Any ] {
//      def beforeRelabeling( first: Any, num: Int )( implicit tx: Any ) {}
//      def afterRelabeling(  first: Any, num: Int )( implicit tx: Any ) {}
//   }

//   private type EOption[ S <: Sys[ S ]] = Set.EntryOption[ S ] with MutableOption[ S ]

   object Set {
      def empty[ S <: Sys[ S ]]( implicit tx: S#Tx, system: S ) : Set[ S ] = {
         new SetNew( system.newID, system.newInt( 1 ))
      }

      def reader[ S <: Sys[ S ]] /* ( relabelObserver: RelabelObserver[ S#Tx, Set.Entry[ S ]] = NoRelabelObserver ) */
                                ( implicit system: S ) : MutableReader[ S, Set[ S ]] =
         new SetReader[ S ] // ( relabelObserver )
   }

   private final class SetReader[ S <: Sys[ S ]] /* ( relabelObserver: RelabelObserver[ S#Tx, Set.Entry[ S ]]) */
                                                ( implicit system: S )
   extends MutableReader[ S, Set[ S ]] {
      def readData( in: DataInput, id: S#ID ) : Set[ S ] = {
         val version = in.readUnsignedByte()
         require( version == SER_VERSION, "Incompatible serialized version (found " + version +
            ", required " + SER_VERSION + ")." )
         val sizeVal = system.readInt( in )

         new SetRead[ S ]( id, sizeVal, in )
      }
   }

   private final class SetRead[ S <: Sys[ S ]]( val id: S#ID, protected val sizeVal: S#Val[ Int ], in: DataInput )
                                              ( implicit val system: S ) extends Set[ S ] {

      val root = system.readMut[ Entry ]( in )( EntryReader )
   }

   private final class SetNew[ S <: Sys[ S ]]( val id: S#ID, protected val sizeVal: S#Val[ Int ])
                                             ( implicit tx: S#Tx, val system: S ) extends Set[ S ] {
      val root: Entry = {
         val tagVal  = system.newInt( 0 )
         val prevRef = system.newOptionRef[ EOpt ]( EmptyEntry )( tx, EntryOptionReader )
         val nextRef = system.newOptionRef[ EOpt ]( EmptyEntry )( tx, EntryOptionReader )
         new Entry( system.newID, tagVal, prevRef, nextRef )
      }
   }

//   private sealed trait SetImpl[ S <: Sys[ S ]] extends Set[ S ] with MutableReader[ S, Set[ S ]#Entry ] {
//      def readEntry( in: DataInput ) : Entry = system.readMut[ Entry ]( in )( this )
//   }

   sealed trait Set[ S <: Sys[ S ]] extends Mutable[ S ] /* extends TotalOrder[ S ] */ /* with Reader[ Set[ S ]#E ] */ {
      protected type EOpt = EntryOption with MutableOption[ S ]

      protected def sizeVal: S#Val[ Int ]
//      protected implicit def optReader: MutableOptionReader[ S, EOpt ]
      def system: S
      def root: Entry

      final def readEntry( in: DataInput ) : Entry = system.readMut[ Entry ]( in )( EntryReader )

      protected implicit object EntryReader extends MutableReader[ S, Entry ] {
         def readData( in: DataInput, id: S#ID ) : Entry = {
            val tagVal  = system.readInt( in )
            val prevRef = system.readOptionRef[ EOpt ]( in )
            val nextRef = system.readOptionRef[ EOpt ]( in )
            new Entry( id, tagVal, prevRef, nextRef )
         }
      }

      protected implicit object EntryOptionReader extends MutableOptionReader[ S, EOpt ] {
         def read( in: DataInput ) : EOpt = system.readOptionMut[ EOpt ]( in )

         def empty = EmptyEntry
         def readData( in: DataInput, id: S#ID ) : Entry = EntryReader.readData( in, id )
      }

      sealed trait EntryOption {
//         def toOption: Option[ Entry ]
         def tag( implicit tx: S#Tx ) : Int
         private[Set] def updatePrev( e: EOpt )( implicit tx: S#Tx ) : Unit
         private[Set] def updateNext( e: EOpt )( implicit tx: S#Tx ) : Unit
         private[Set] def updateTag( value: Int )( implicit tx: S#Tx ) : Unit
         def orNull : Entry
      }

      sealed trait EmptyEntryLike extends EntryOption with EmptyMutable {
         private[Set] final def updatePrev( e: EOpt )( implicit tx: S#Tx ) {}
         private[Set] final def updateNext( e: EOpt )( implicit tx: S#Tx ) {}
         final def orNull : Entry = null
         private[Set] final def updateTag( value: Int )( implicit tx: S#Tx ) {
            sys.error( "Internal error - shouldn't be here" )
         }
         final def tag( implicit tx: S#Tx ) = Int.MaxValue
      }
      case object EmptyEntry extends EmptyEntryLike

      final class Entry private[TotalOrder]( val id: S#ID, tagVal: S#Val[ Int ], prevRef: S#Ref[ EOpt ], nextRef: S#Ref[ EOpt ])
      extends EntryOption with Mutable[ S ] {
//private var disposed = false
//private var removed = false

         def tag( implicit tx: S#Tx ) : Int = tagVal.get

         def prev( implicit tx: S#Tx ) : EOpt = prevRef.get
         def next( implicit tx: S#Tx ) : EOpt = prevRef.get
         private[Set] def prevOrNull( implicit tx: S#Tx ) : Entry = prevRef.get.orNull
         private[Set] def nextOrNull( implicit tx: S#Tx ) : Entry = nextRef.get.orNull
         def orNull : Entry = this

         private[Set] def updatePrev( e: EOpt )( implicit tx: S#Tx ) { prevRef.set( e )}
         private[Set] def updateNext( e: EOpt )( implicit tx: S#Tx ) { nextRef.set( e )}
         private[Set] def updateTag( value: Int )( implicit tx: S#Tx ) { tagVal.set( value )}

         protected def writeData( out: DataOutput ) {
            tagVal.write( out )
            prevRef.write( out )
            nextRef.write( out )
         }

         protected def disposeData()( implicit tx: S#Tx ) {
//require( !disposed, "DUPLICATE DISPOSAL" )
////require( removed, "DISPOSAL WITHOUT REMOVAL" )
//disposed = true
            // first unlink this node
            val p = prev
            val n = next
            p.updateNext( n )
            n.updatePrev( p )
            sizeVal.transform( _ - 1 )

            // then free the refs
            prevRef.dispose()
            nextRef.dispose()
            tagVal.dispose()
         }

         def append()( implicit tx: S#Tx ) : Entry = {
            val p          = this
            val n          = next
            val nextTag    = n.tag // if( next == null ) Int.MaxValue else next.tag
            val recPrevRef = system.newOptionRef[ EOpt ]( p )
            val recNextRef = system.newOptionRef[ EOpt ]( n )
            val prevTag    = p.tag
            val recTag     = prevTag + ((nextTag - prevTag + 1) >>> 1)
            val recTagVal  = system.newInt( recTag )
            val rec        = new Entry( system.newID, recTagVal, recPrevRef, recNextRef )
            p.updateNext( rec )
            n.updatePrev( rec )
            sizeVal.transform( _ + 1 )
            if( recTag == nextTag ) relabel( rec )
            rec
         }

         def prepend()( implicit tx: S#Tx ) : Entry = {
            val n          = this
            val p          = n.prev
            val prevTag    = p.tag
            val recPrevRef = system.newOptionRef[ EOpt ]( p )
            val recNextRef = system.newOptionRef[ EOpt ]( n )
            val nextTag    = n.tag
            val recTag     = prevTag + ((nextTag - prevTag + 1) >>> 1)
            val recTagVal  = system.newInt( recTag )
            val rec        = new Entry( system.newID, recTagVal, recPrevRef, recNextRef )
            n.updatePrev( rec )
            p.updateNext( rec )
            sizeVal.transform( _ + 1 )
            if( recTag == nextTag ) relabel( rec )
            rec
         }

         def remove()( implicit tx: S#Tx ) {
//require( !removed, "DUPLICATE REMOVAL" )
//removed = true
            val p = prev
            val n = next
            p.updateNext( n )
            n.updatePrev( p )
            sizeVal.transform( _ - 1 )

//            dispose()
         }

         def removeAndDispose()( implicit tx: S#Tx ) {
            remove()
            dispose()
         }
      }

      final protected def disposeData()( implicit tx: S#Tx ) {
//         val r = root
//         var m = r.prevOrNull
//         while( m ne null ) {
//            val t = m
//            m = m.prevOrNull
//            t.dispose()
//         }
//         m = r
//         do {
//            val t = m
//            m = m.prevOrNull
//            t.dispose()
//         } while( m ne null )
         root.dispose()
         sizeVal.dispose()
      }

      final protected def writeData( out: DataOutput ) {
         out.writeUnsignedByte( SER_VERSION )
         sizeVal.write( out )
         root.write( out )
      }

      final def size( implicit tx: S#Tx ) : Int = sizeVal.get

      final def head( implicit tx: S#Tx ) : Entry = {
         var e = root
         var p = e.prevOrNull
         while( p ne null ) {
            e = p
            p = p.prevOrNull
         }
         e
      }

      final def tagList( from: Entry )( implicit tx: S#Tx ) : List[ Int ] = {
         val b       = List.newBuilder[ Int ]
         var entry   = from
         while( entry ne null ) {
            b       += entry.tag
            entry    = entry.nextOrNull
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
      private def relabel( _first: Entry )( implicit tx: S#Tx ) {
         var mask       = -1
         var thresh     = 1.0
         var num        = 1
   //      val mul     = 2/((2*len(self))**(1/30.))
         val mul        = 2 / math.pow( size << 1, 1/30.0 )
         var first      = _first
         var last       = _first
         var base       = _first.tag
         do {
            var prev    = first.prevOrNull
            while( (prev ne null) && ((prev.tag & mask) == base) ) {
               first    = prev
               prev     = prev.prevOrNull
               num     += 1
            }
            var next    = last.nextOrNull
            while( (next ne null) && ((next.tag & mask) == base) ) {
               last     = next
               next     = next.nextOrNull
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
//               observer.beforeRelabeling( first, num )
//sys.error( "TODO" )

   //            while( !(item eq last) ) {
               // Note: this was probably a bug in Eppstein's code
               // -- it ran for one iteration less which made
               // the test suite fail for very dense tags. it
               // seems now it is correct with the inclusion
               // of last in the tag updating.
               next = first
               var cnt = 0; while( cnt < num ) {
                  next.updateTag( base )
                  next        = next.nextOrNull
                  base       += inc
                  cnt        += 1
               }
//sys.error( "TODO" )
//               observer.afterRelabeling( first, num )
               return
            }
            mask   <<= 1      // next coarse step
            base    &= mask
            thresh  *= mul
         } while( mask != 0 )
         sys.error( "label overflow" )
      }
   }

//   private final class SetImpl[ S <: Sys[ S ]]( val id: S#ID,
//                                                protected val observer: RelabelObserver[ S#Tx, _ /* Set.Entry[ S ] */ ],
//                                                protected val sizeVal: S#Val[ Int ] /*, _rootFun: SetImpl[ S ] => Set.Entry[ S ]*/)
//                                              ( implicit val system: S )
//   extends Set[ S ] with MutableOptionReader[ S, Set.EntryOption[ S ] with MutableOption[ S ]] {
//      object RootReader extends MutableReader[ S, Entry ] {
//
//      }
//
//      private type EOpt = Set.EntryOption[ S ] with MutableOption[ S ]
//
////      val root = _rootFun( this )
//      val root: Entry = null //XXX
//
//      implicit def impl = this
//
//      def read( in: DataInput ) : EOpt = system.readOptionMut[ EOpt ]( in )
//
//      def readData( in: DataInput, id: S#ID ) : Entry = {
//         val tagVal  = system.readInt( in )
//         val prevRef = system.readOptionRef[ EOpt ]( in )
//         val nextRef = system.readOptionRef[ EOpt ]( in )
//         new EntryImpl( id, tagVal, prevRef, nextRef )
//      }
//   }
}
//sealed trait TotalOrder[ S <: Sys[ S ]] extends Disposable[ S#Tx ] /* with Reader[ E ] */ with Mutable[ S ] {
//   def system: S
//
//   type E
//
//   /**
//    * The initial element created from which you can start to append and prepend.
//    */
//   def root : E // Entry[ S ]
//
//   /**
//    * Returns the head element of the structure. Note that this
//    * is O(n) worst case.
//    */
//   def head( implicit tx: S#Tx ) : E // Entry[ S ]
//
//   /**
//    * The number of elements in the order. This is `1` for a newly
//    * created order (consisting only of the root element).
//    * You will rarely need this information except for debugging
//    * purpose. The operation is O(1).
//    */
//   def size( implicit tx: S#Tx ) : Int
//
//   def tagList( from: E )( implicit tx: S#Tx ) : List[ Int  ]
//}
