package de.sciss.tree

import collection.mutable.{ Builder, DoubleLinkedListLike, LinearSeq => MLinearSeq, Seq => MSeq }
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
 * Due to rebalancing on the integer tags used to maintain order,
 * the amortized time per insertion in an n-item list is O(log n).
 */
object TotalOrder extends SeqFactory[ TotalOrder ] {
   type Tag = Int // Long

//   var flonky = false

//   class Record[ T ]( private[ TotalOrder ] var v: Tag ) {
//      private[ TotalOrder ] var succ: Record[ T ] = null
//      private[ TotalOrder ] var pred: Record[ T ] = null
//   }

   // ---- GenericCompanion ----
  def newBuilder[ V ] = new Builder[ V, TotalOrder[ V ]] {
      def emptyList() = new TotalOrder[ V ]( new Size )
      var head = emptyList()
      var tail = head

      def +=( elem: V ) : this.type = {
         tail = tail.append( elem )
//
//         if( head.isEmpty ) {
//            head  = new TotalOrder( elem, 0x3FFFFFFF, head /* emptyList() */ )
//            tail  = head
//         } else {
//            tail  = tail.append( elem )
////            val tag: Int = error( "TODO" )
////            current.append( new TotalOrder( elem, tag, emptyList() ))
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
}

object TotalOrderTest extends App {
   val to    = TotalOrder[ Int ]()
   val rnd   = new util.Random( 0 )
   val n     = 3042 // 3041
   var pred  = to.append( 0 )
   for( i <- 1 until n ) {
      if( i == n - 1 ) {
         println( "last" )
//         TotalOrder.flonky = true
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

final class TotalOrder[ V ] private ( protected val totalSize: TotalOrder.Size ) // (_t: Int)
extends MLinearSeq[ V ]
with GenericTraversableTemplate[ V, TotalOrder ]
with DoubleLinkedListLike[ V, TotalOrder[ V ]] with Ordered[ TotalOrder[ V ]] {
   private type TV = TotalOrder[ V ]

   next = this
   protected var tag: Int = 0 // _t

   private def this( sz: TotalOrder.Size, elem: V, prev: TotalOrder[ V ], next: TotalOrder[ V ]) {
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

   // ---- GenericTraversableTemplate ----
   override def companion: GenericCompanion[ TotalOrder ] = TotalOrder

//   type Rec = Record[ T ]
//
//   private var base : Rec = null

   def isHead : Boolean = prev == null
   def isLast : Boolean = next.isEmpty

   /**
    * Compares the positions of x and y in the sequence
   */
   def compare( that: TV ) : Int = tag compare that.tag

   /**
    * Appends an element to the end of the sequence, and returns
    * that new sequence tail. Note that this operation takes O(n).
    *
    * @param   elem  the element to append
    * @return  the total order entry corresponding to the newly appended element
    */
   def append( elem: V ) : TV = {
      if( isEmpty ) {
         this.elem   = elem
         tag         = Int.MaxValue >> 1
         val empty   = new TV( totalSize )
         totalSize.inc
         next        = empty
         empty.prev  = this
         this
      } else {
         lastNode.insertAfter( elem )
      }
   }

   private def lastNode : TV = {
      var res = next
      while( !res.isEmpty ) res = res.next
      res.prev
   }

   /**
    * Inserts a new element after this node
    */
   def insertAfter( elem: V ) : TV = {
      val rec     = new TV( totalSize, elem, this, next )
      val nextTag = if( rec.next.isEmpty ) Int.MaxValue else rec.next.tag
      rec.tag     = tag + ((nextTag - tag + 1) >> 1)
      if( rec.tag == nextTag ) rec.relabel
      rec
   }

   /**
    * Inserts a new element before this node
    */
   def insertBefore( elem: V ) : TV = {
      if( isHead ) {
         // to maintain references to the 'head' of the list,
         // in the case when an element is inserted at the
         // head of the list, we instead change this entry's
         // elem and tag, and a new successor is inserted
         // after this head
         val rec     = new TV( totalSize, this.elem, this, next )
         this.elem   = elem
         rec.tag     = tag
         tag         = (tag + 1) >> 1
         if( tag == rec.tag ) this.relabel
         this
      } else {
         val rec     = new TV( totalSize, elem, prev, this )
         val prevTag = rec.prev.tag
         rec.tag     = prevTag + ((tag - prevTag + 1) >> 1)
         if( rec.tag == tag ) rec.relabel
         rec
      }
   }

   /**
    * Relabels from a this entry to clean up collisions with
    * its successors' tags.
    *
    * Naive implementation.
    */
   protected def relabelSIMPLE {
      var base = this.tag // 0
      val inc  = (Int.MaxValue - base) / size
      require( inc > 0, "label overflow" )
//println( "relabel (" + inc + ")" )
      var entry = this
      while( entry.nonEmpty ) {
         entry.tag = base
         base += inc
         entry = entry.next
      }
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
   protected def relabel {
      var base    = tag
      var mask    = -1
      var thresh  = 1.0
      var first   = this
      var last    = this
      var num     = 1
//      val mul     = 2/((2*len(self))**(1/30.))
      val mul     = 2 / math.pow( totalSize.value << 1, 1/30.0 )
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
//var cnt = 0
//            while( !(item eq last) ) {
var cnt = 0; while( cnt < num ) {

                item.tag   = base
cnt += 1
                item       = item.next
                base      += inc
            }
//if( TotalOrder.flonky ) println( "num relabeled : " + cnt + " / num = " + num )
            return
         }
//         mask     = (mask << 1) + 1    // expand to next power of two
         mask   <<= 1      // next coarse step
         base    &= mask
         thresh  *= mul
      } while( mask != 0 )
      throw new RuntimeException( "label overflow" )
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
      while( entry.nonEmpty ) {
         assert( entry.tag > prevTag, "elem " + entry.elem + " at index " + indexOf( entry.elem ) + " has tag " + entry.tag +
            ", while previous elem has tag " + prevTag )
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
      var entry   = this
      while( entry.nonEmpty ) {
         b += entry.tag
         entry    = entry.next
      }
      b.result()
   }
}