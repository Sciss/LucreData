package de.sciss.tree

import collection.mutable.{ Builder, DoubleLinkedListLike, LinearSeq => MLinearSeq, Seq => MSeq }
import collection.generic.{SeqFactory, GenericCompanion, GenericTraversableTemplate}
import sys.error // suckers

/**
 * A data structure to maintain an ordered sequence of elements such
 * that two random elements can be compared in O(1).
 */
object TotalOrder extends SeqFactory[ TotalOrder ] {
   type Tag = Int // Long

//   class Record[ T ]( private[ TotalOrder ] var v: Tag ) {
//      private[ TotalOrder ] var succ: Record[ T ] = null
//      private[ TotalOrder ] var pred: Record[ T ] = null
//   }

   // ---- GenericCompanion ----
  def newBuilder[ V ] = new Builder[ V, TotalOrder[ V ]] {
      def emptyList() = new TotalOrder[ V ]()
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
}
final class TotalOrder[ V ] private () // (_t: Int)
extends MLinearSeq[ V ]
with GenericTraversableTemplate[ V, TotalOrder ]
with DoubleLinkedListLike[ V, TotalOrder[ V ]] with Ordered[ TotalOrder[ V ]] {
//   import TotalOrder._
   private type TV = TotalOrder[ V ]

   next = this
   protected var tag: Int = 0 // _t

   private def this( elem: V, prev: TotalOrder[ V ], next: TotalOrder[ V ]) {
      this()
      this.elem      = elem
      this.prev      = prev
      if( prev != null ) {
         require( prev.nonEmpty && (prev.next eq next) )
         prev.next = this
      } else {
         require( next.prev == null )
      }
      this.next      = next
      next.prev      = this
   }

   // ---- GenericTraversableTemplate ----
   override def companion: GenericCompanion[ TotalOrder ] = TotalOrder

//   type Rec = Record[ T ]
//
//   private var base : Rec = null

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
         val empty   = new TV()
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
      val rec     = new TV( elem, this, next )
      val nextTag = if( rec.next.isEmpty ) Int.MaxValue else rec.next.tag
      rec.tag     = tag + ((nextTag - tag + 1) >> 1)
      if( rec.tag == nextTag ) rec.relabel
      rec
   }

   /**
    * Inserts a new element before this node
    */
   def insertBefore( elem: V ) : TV = {
      if( prev == null ) {
         // to maintain references to the 'head' of the list,
         // in the case when an element is inserted at the
         // head of the list, we instead change this entry's
         // elem and tag, and a new successor is inserted
         // after this head
         val rec     = new TV( this.elem, this, next )
         this.elem   = elem
         rec.tag     = tag
         tag         = (tag + 1) >> 1
         if( tag == rec.tag ) this.relabel
         this
      } else {
         val rec     = new TV( elem, prev, this )
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
   protected def relabel {
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
    * Debugging method: Validates that the list from this entry
    * to the end has monotonically increasing
    * tags. Throws an assertion error if the
    * validation fails.
    */
   def validateToEnd {
      var prevTag = tag
      var entry   = next
      while( entry.nonEmpty ) {
         assert( entry.tag > prevTag )
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