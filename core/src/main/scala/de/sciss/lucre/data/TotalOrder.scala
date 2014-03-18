/*
 *  TotalOrder.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package lucre
package data

import annotation.{switch, tailrec}
import stm.{Mutable, Sys}
import serial.{Writable, Serializer, DataInput, DataOutput}

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
  private final val SER_VERSION = 84  // 'T'

  // ---- Set ----

  object Set {
    def empty[S <: Sys[S]](implicit tx: S#Tx): Set[S] = empty()

    def empty[S <: Sys[S]](rootTag: Int = 0)(implicit tx: S#Tx): Set[S] = {
      val id = tx.newID()
      new SetNew[S](id, rootTag, tx.newIntVar(id, 1), tx)
    }

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Set[S] =
      new SetRead(in, access, tx)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Set[S]] =
      new SetSerializer[S]

    // ( relabelObserver )

    sealed trait EntryOption[S <: Sys[S]] {
      protected type E    = Entry[S]
      protected type EOpt = EntryOption[S] /* with MutableOption[ S ] */

      //         def tag( implicit tx: S#Tx ) : Int
      private[Set] def tagOr      (empty: Int)(implicit tx: S#Tx): Int
      private[Set] def updatePrev (e: EOpt)   (implicit tx: S#Tx): Unit
      private[Set] def updateNext (e: EOpt)   (implicit tx: S#Tx): Unit
      private[Set] def updateTag  (value: Int)(implicit tx: S#Tx): Unit

      def orNull: E
      def isDefined: Boolean
      def isEmpty: Boolean
    }

    final class EmptyEntry[S <: Sys[S]] private[TotalOrder]() extends EntryOption[S] /* with EmptyMutable */ {
      private[Set] def updatePrev(e: EOpt)(implicit tx: S#Tx) = ()
      private[Set] def updateNext(e: EOpt)(implicit tx: S#Tx) = ()

      def orNull: E = null

      private[Set] def updateTag(value: Int)(implicit tx: S#Tx): Unit =
        sys.error("Internal error - shouldn't be here")

      private[Set] def tagOr(empty: Int)(implicit tx: S#Tx) = empty

      def isDefined = false
      def isEmpty   = true

      override def toString = "<empty>"
    }

    final class Entry[S <: Sys[S]] private[TotalOrder](val id: S#ID, set: Set[S], tagVal: S#Var[Int],
                                                       prevRef: S#Var[EntryOption[S] /* with MutableOption[ S ] */ ],
                                                       nextRef: S#Var[EntryOption[S] /* with MutableOption[ S ] */ ])
      extends EntryOption[S] with Mutable.Impl[S] with Ordered[S#Tx, Entry[S]] {

      override def toString() = "Set.Entry" + id

      def compare(that: Entry[S])(implicit tx: S#Tx): Int = {
        val thisTag = tag
        val thatTag = that.tag
        if (thisTag < thatTag) -1 else if (thisTag > thatTag) 1 else 0
      }

      def tag                           (implicit tx: S#Tx): Int  = tagVal()
      private[Set] def tagOr(empty: Int)(implicit tx: S#Tx)       = tagVal()

      def prev(implicit tx: S#Tx): EOpt = prevRef()
      def next(implicit tx: S#Tx): EOpt = nextRef()

      private[Set] def prevOrNull(implicit tx: S#Tx): E = prevRef().orNull
      private[Set] def nextOrNull(implicit tx: S#Tx): E = nextRef().orNull

      def orNull: E = this

      def isDefined = true
      def isEmpty   = false

      private[Set] def updatePrev(e: EOpt)(implicit tx: S#Tx): Unit = prevRef() = e
      private[Set] def updateNext(e: EOpt)(implicit tx: S#Tx): Unit = nextRef() = e

      private[Set] def updateTag(value: Int)(implicit tx: S#Tx): Unit = tagVal() = value

      protected def writeData(out: DataOutput): Unit = {
        tagVal .write(out)
        prevRef.write(out)
        nextRef.write(out)
      }

      protected def disposeData()(implicit tx: S#Tx): Unit = {
        prevRef.dispose()
        nextRef.dispose()
        tagVal .dispose()
      }

      def remove()(implicit tx: S#Tx): Unit = set.remove(this)

      def append   ()(implicit tx: S#Tx): E = set.insertAfter   (this)
      def appendMax()(implicit tx: S#Tx): E = set.insertMaxAfter(this)
      def prepend  ()(implicit tx: S#Tx): E = set.insertBefore  (this)

      def removeAndDispose()(implicit tx: S#Tx): Unit = {
        remove()
        dispose()
      }

      def validate(msg: => String)(implicit tx: S#Tx): Unit = {
        val recTag = tag
        if (prev.isDefined) {
          val prevTag = prev.orNull.tag
          assert(prevTag < recTag, "prev " + prevTag + " >= rec " + recTag + " - " + msg)
        }
        if (next.isDefined) {
          val nextTag = next.orNull.tag
          assert(recTag < nextTag, "rec " + recTag + " >= next " + nextTag + " - " + msg)
        }
      }
    }
  }

  private final class SetSerializer[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Set[S]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Set[S] =
      new SetRead[S](in, access, tx)

    def write(v: Set[S], out: DataOutput): Unit = v.write(out)

    override def toString = "Set.serializer"
  }

  private final class SetRead[S <: Sys[S]](in: DataInput, access: S#Acc, tx0: S#Tx)
    extends Set[S] with Mutable.Impl[S] {

    val id = tx0.readID(in, access)

    {
      val version = in.readByte()
      require(version == SER_VERSION, "Incompatible serialized version (found " + version +
        ", required " + SER_VERSION + ").")
    }

    val sizeVal = tx0.readIntVar(id, in)
    val root    = EntrySerializer.read(in, access)(tx0)
  }

  private final class SetNew[S <: Sys[S]](val id: S#ID, rootTag: Int, protected val sizeVal: S#Var[Int], tx0: S#Tx)
    extends Set[S] with Mutable.Impl[S] {
    me =>

    val root: E = {
      val rootID  = tx0.newID()
      val tagVal  = tx0.newIntVar(rootID, rootTag)
      val prevRef = tx0.newVar[EOpt](id, empty)(EntryOptionSerializer)
      val nextRef = tx0.newVar[EOpt](id, empty)(EntryOptionSerializer)
      new E(rootID, me, tagVal, prevRef, nextRef)
    }
  }

  sealed trait Set[S <: Sys[S]] extends TotalOrder[S] {
    me =>

    final type           E    = Set.Entry[S]
    protected final type EOpt = Set.EntryOption[S] /* with MutableOption[ S ] */

    protected def sizeVal: S#Var[Int]

    protected final val empty = new Set.EmptyEntry[S]

    // def root: E

    override def toString = "Set" + id

    final def readEntry(in: DataInput, access: S#Acc)(implicit tx: S#Tx): E = EntrySerializer.read(in, access)

    protected implicit object EntrySerializer extends Serializer[S#Tx, S#Acc, E] {
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): E = {
        val id      = tx.readID(in, access)
        val tagVal  = tx.readIntVar(id, in)
        val prevRef = tx.readVar[EOpt](id, in)(EntryOptionSerializer)
        val nextRef = tx.readVar[EOpt](id, in)(EntryOptionSerializer)
        new E(id, me, tagVal, prevRef, nextRef)
      }

      def write(v: E, out: DataOutput): Unit = v.write(out)
    }

    protected implicit object EntryOptionSerializer extends Serializer[S#Tx, S#Acc, EOpt] {
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): EOpt = {
        (in.readByte(): @switch) match {
          case 0 => me.empty
          case 1 => EntrySerializer.read(in, access)
          case cookie => sys.error("Unexpected cookie " + cookie)
        }
      }

      def write(v: EOpt, out: DataOutput): Unit = {
        val e = v.orNull
        if (e == null) {
          out.writeByte(0)
        } else {
          out.writeByte(1)
          e.write(out)
        }
      }
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      root.dispose()
      sizeVal.dispose()
    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeByte(SER_VERSION)
      sizeVal.write(out)
      root.write(out)
    }

    final private[TotalOrder] def insertMaxAfter(prev: E)(implicit tx: S#Tx): E = {
      val next        = prev.next
      val nextTag     = next.tagOr(Int.MinValue)  // Int.MinValue - 1 == Int.MaxValue !
      val prevTag     = prev.tag
      val n1          = nextTag - 1
      val recTag      = if(prevTag == n1) nextTag else n1
      insert(prev = prev, next = next, nextTag = nextTag, recTag = recTag)
    }

    final private[TotalOrder] def insertAfter(prev: E)(implicit tx: S#Tx): E = {
      val next        = prev.next
      val nextTag     = next.tagOr(Int.MaxValue)
      val prevTag     = prev.tag
      val recTag      = prevTag + ((nextTag - prevTag + 1) >>> 1)
      insert(prev = prev, next = next, nextTag = nextTag, recTag = recTag)
    }

    final private[TotalOrder] def insertBefore(next: E)(implicit tx: S#Tx): E = {
      val prev        = next.prev
      val prevTag     = prev.tagOr(0)
      val nextTag     = next.tag
      val recTag      = prevTag + ((nextTag - prevTag + 1) >>> 1)
      insert(prev = prev, next = next, nextTag = nextTag, recTag = recTag)
    }

    private def insert(prev: EOpt, next: EOpt, nextTag: Int, recTag: Int)(implicit tx: S#Tx): E = {
      val recID       = tx.newID()
      val recPrevRef  = tx.newVar[EOpt](recID, prev)
      val recNextRef  = tx.newVar[EOpt](recID, next)
      val recTagVal   = tx.newIntVar(recID, recTag)
      val rec         = new E(recID, this, recTagVal, prevRef = recPrevRef, nextRef = recNextRef)
      prev.updateNext(rec)
      next.updatePrev(rec)
      sizeVal.transform(_ + 1)
      if (recTag == nextTag) relabel(rec)
      rec
    }

    final private[TotalOrder] def remove(entry: E)(implicit tx: S#Tx): Unit = {
      val p = entry.prev
      val n = entry.next
      p.updateNext(n)
      n.updatePrev(p)
      sizeVal.transform(_ - 1)
    }

    final def size(implicit tx: S#Tx): Int = sizeVal()

    final def head(implicit tx: S#Tx): E = {
      var e = root
      var p = e.prevOrNull
      while (p ne null) {
        e = p
        p = p.prevOrNull
      }
      e
    }

    final def tagList(from: E)(implicit tx: S#Tx): List[Int] = {
      val b = List.newBuilder[Int]
      var entry = from
      while (entry ne null) {
        b += entry.tag
        entry = entry.nextOrNull
      }
      b.result()
    }

    /** Relabels from a this entry to clean up collisions with
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
    private def relabel(_first: E)(implicit tx: S#Tx): Unit = {
      var mask    = -1
      var thresh  = 1.0
      var num     = 1
      // val mul     = 2/((2*len(self))**(1/30.))
      val mul     = 2 / math.pow(size << 1, 1 / 30.0)
      var first   = _first
      var last    = _first
      var base    = _first.tag
      do {
        var prev = first.prevOrNull
        while ((prev ne null) && (prev.tag & mask) == base) {
          first = prev
          prev  = prev.prevOrNull
          num  += 1
        }
        var next = last.nextOrNull
        while ((next ne null) && (next.tag & mask) == base) {
          last = next
          next = next.nextOrNull
          num += 1
        }
        //         val inc = (mask + 1) / num
        val inc = -mask / num

        // important: we found a corner case where _first is the last
        // element in the list with a value of 0x7FFFFFFF. in this
        // case, if the predecessor is smaller in value, the original
        // algorithm would immediately terminate with num == 1, which
        // will obviously leave the tag unchanged! thus we must add
        // the additional condition that num is greater than 1!
        if (inc >= thresh && num > 1) {
          // found rebalanceable range
          //               observer.beforeRelabeling( first, num )
          //sys.error( "TODO" )

          //            while( !(item eq last) ) {
          // Note: this was probably a bug in Eppstein's code
          // -- it ran for one iteration less which made
          // the test suite fail for very dense tags. it
          // seems now it is correct with the inclusion
          // of last in the tag updating.
          next = first
          var cnt = 0
          while (cnt < num) {
            next.updateTag(base)
            next  = next.nextOrNull
            base += inc
            cnt  += 1
          }
          //sys.error( "TODO" )
          //               observer.afterRelabeling( first, num )
          return
        }
        mask  <<= 1 // next coarse step
        base   &= mask
        thresh *= mul
      } while (mask != 0)
      sys.error("label overflow")
    }
  }

  // ---- Map ----

  object Map {
    def empty[S <: Sys[S], A](relabelObserver: Map.RelabelObserver[S#Tx, A], entryView: A => Map.Entry[S, A],
                              rootTag: Int = 0)
                             (implicit tx: S#Tx, keySerializer: Serializer[S#Tx, S#Acc, A]): Map[S, A] = {
      val id = tx.newID()
      new MapNew[S, A](id, tx.newIntVar(id, 1), relabelObserver, entryView, rootTag, tx)
    }

    def read[S <: Sys[S], A](in: DataInput, access: S#Acc, relabelObserver: Map.RelabelObserver[S#Tx, A],
                             entryView: A => Map.Entry[S, A])
                            (implicit tx: S#Tx, keySerializer: Serializer[S#Tx, S#Acc, A]): Map[S, A] =
      new MapRead[S, A](relabelObserver, entryView, in, access, tx)


    implicit def serializer[S <: Sys[S], A](relabelObserver: Map.RelabelObserver[S#Tx, A],
                                            entryView: A => Map.Entry[S, A])
                                           (implicit keySerializer: Serializer[S#Tx, S#Acc, A]): Serializer[S#Tx, S#Acc, Map[S, A]] =
      new MapSerializer[S, A](relabelObserver, entryView)

    /**
     * A `RelabelObserver` is notified before and after a relabeling is taking place due to
     * item insertions. The iterator passed to it contains all the items which are relabelled,
     * excluding the one that has caused the relabelling action.
     *
     * Note that there is a tricky case, when an object creates more than one total order entry,
     * and then calls `placeBefore` or `placeAfter` successively on these entries: For the first
     * entry, the iterator will not contain the inserted element, but when the second entry is
     * inserted, the iterator will contain the first entry, with the potential of causing trouble
     * as the entry may be contained in an incompletely initialized object.
     *
     * For example, in the case of storing pre-head, pre-tail and post elements in two orders, make
     * sure that the pre-tail insertion comes last. Because this will happen:
     *
     * (1) pre-head, post and pre-tail entries created (their tags are -1)
     * (2) pre-head placed, may cause relabelling, but then will be excluded from the iterator
     * (3) post placed, may cause relabelling, but then will be excluded from the iterator
     * (4) pre-tail placed, may cause relabelling, and while the pre-tail view will be excluded
     * from the iterator, the previously placed pre-head '''will''' be included in the iterator,
     * showing the item with pre-tail tag of `-1` in `beforeRelabeling`, however, fortunately,
     * with assigned tag in `afterRelabeling`.
     */
    trait RelabelObserver[Tx /* <: Txn[ _ ] */ , -A] {
      /**
       * This method is invoked right before relabelling starts. That is, the items in
       * the `dirty` iterator are about to be relabelled, but at the point of calling
       * this method the tags still carry their previous values.
       */
      def beforeRelabeling(/* inserted: A, */ dirty: Iterator[Tx, A])(implicit tx: Tx): Unit

      /**
       * This method is invoked right after relabelling finishes. That is, the items in
       * the `clean` iterator have been relabelled and the tags carry their new values.
       */
      def afterRelabeling(/* inserted: A, */ clean: Iterator[Tx, A])(implicit tx: Tx): Unit
    }

    final class NoRelabelObserver[Tx /* <: Txn[ _ ] */ , A]
      extends RelabelObserver[Tx, A] {

      def beforeRelabeling(/* inserted: A, */ dirty: Iterator[Tx, A])(implicit tx: Tx) = ()
      def afterRelabeling (/* inserted: A, */ clean: Iterator[Tx, A])(implicit tx: Tx) = ()

      override def toString = "NoRelabelObserver"
    }

    final class Entry[S <: Sys[S], A] private[TotalOrder](map: Map[S, A], val id: S#ID,
                                                          tagVal:  S#Var[Int],
                                                          prevRef: S#Var[KeyOption[S, A]],
                                                          nextRef: S#Var[KeyOption[S, A]])
      extends Mutable.Impl[S] with Ordered[S#Tx, Entry[S, A]] {

      private type E    = Entry[S, A]
      private type KOpt = KeyOption[S, A]

      def tag(implicit tx: S#Tx): Int = tagVal()

      def validate(msg: => String)(implicit tx: S#Tx): Unit = {
        val recTag = tag
        if (prev.isDefined) {
          val prevTag = map.entryView(prev.get).tag
          assert(prevTag < recTag, "prev " + prevTag + " >= rec " + recTag + " - " + msg)
        }
        if (next.isDefined) {
          val nextTag = map.entryView(next.get).tag
          assert(recTag < nextTag, "rec " + recTag + " >= next " + nextTag + " - " + msg)
        }
      }

      override def toString = "Map.Entry" + id

      private[TotalOrder] def prev(implicit tx: S#Tx): KOpt = prevRef()
      private[TotalOrder] def next(implicit tx: S#Tx): KOpt = nextRef()

      // private[TotalOrder] def prevOrNull( implicit tx: S#Tx ) : A = prevRef.get.orNull
      // private[TotalOrder] def nextOrNull( implicit tx: S#Tx ) : A = nextRef.get.orNull
      // def orNull : E = this

      private[TotalOrder] def updatePrev(e: KOpt)(implicit tx: S#Tx): Unit = prevRef() = e
      private[TotalOrder] def updateNext(e: KOpt)(implicit tx: S#Tx): Unit = nextRef() = e

      private[TotalOrder] def updateTag(value: Int)(implicit tx: S#Tx): Unit = tagVal() = value

      // ---- Ordered ----

      def compare(that: E)(implicit tx: S#Tx): Int = {
        val thisTag = tag
        val thatTag = that.tag
        if (thisTag < thatTag) -1 else if (thisTag > thatTag) 1 else 0
      }

      protected def writeData(out: DataOutput): Unit = {
        tagVal .write(out)
        prevRef.write(out)
        nextRef.write(out)
      }

      protected def disposeData()(implicit tx: S#Tx): Unit = {
        prevRef.dispose()
        nextRef.dispose()
        tagVal .dispose()
      }

      def remove()(implicit tx: S#Tx): Unit = map.remove(this)

      def removeAndDispose()(implicit tx: S#Tx): Unit = {
        remove()
        dispose()
      }
    }
  }

  private[TotalOrder] sealed trait KeyOption[S <: Sys[S], A] extends Writable {
    def orNull: Map.Entry[S, A]

    def isDefined: Boolean
    def isEmpty  : Boolean

    def get: A
  }

  private[TotalOrder] final class EmptyKey[S <: Sys[S], A]
    extends KeyOption[S, A] /* with EmptyMutable */ {

    def isDefined: Boolean = false
    def isEmpty  : Boolean = true

    def get: A = throw new NoSuchElementException("EmptyKey.get")

    def tag(implicit tx: S#Tx) = Int.MaxValue

    def orNull: Map.Entry[S, A] = null

    def write(out: DataOutput): Unit = out.writeByte(0)

    override def toString = "<empty>"
  }

  private[TotalOrder] final class DefinedKey[S <: Sys[S], A](map: Map[S, A], val get: A)
    extends KeyOption[S, A] {

    def isDefined: Boolean = true
    def isEmpty  : Boolean = false

    def orNull: Map.Entry[S, A] = map.entryView(get)

    def write(out: DataOutput): Unit = {
      out.writeByte(1)
      map.keySerializer.write(get, out)
    }

    override def toString = get.toString
  }

  private final class MapSerializer[S <: Sys[S], A](relabelObserver: Map.RelabelObserver[S#Tx, A],
                                                    entryView: A => Map.Entry[S, A])
                                                   (implicit keySerializer: Serializer[S#Tx, S#Acc, A])
    extends Serializer[S#Tx, S#Acc, Map[S, A]] {

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Map[S, A] =
      new MapRead[S, A](relabelObserver, entryView, in, access, tx)

    def write(v: Map[S, A], out: DataOutput): Unit = v.write(out)

    override def toString = "Map.serializer"
  }

  private final class MapRead[S <: Sys[S], A](protected val observer: Map.RelabelObserver[S#Tx, A],
                                              val entryView: A => Map.Entry[S, A],
                                              in: DataInput, access: S#Acc, tx0: S#Tx)
                                             (implicit private[TotalOrder] val keySerializer: Serializer[S#Tx, S#Acc, A])
    extends Map[S, A] with Mutable.Impl[S] {

    val id = tx0.readID(in, access)

    {
      val version = in.readByte()
      require(version == SER_VERSION, "Incompatible serialized version (found " + version +
        ", required " + SER_VERSION + ").")
    }

    val sizeVal = tx0.readIntVar(id, in)
    val root    = EntrySerializer.read(in, access)(tx0)
  }

  private final class MapNew[S <: Sys[S], A](val id: S#ID, protected val sizeVal: S#Var[Int],
                                             protected val observer: Map.RelabelObserver[S#Tx, A],
                                             val entryView: A => Map.Entry[S, A], rootTag: Int, tx0: S#Tx)
                                            (implicit private[TotalOrder] val keySerializer: Serializer[S#Tx, S#Acc, A])
    extends Map[S, A] with Mutable.Impl[S] {

    val root: E = {
      val rootID  = tx0.newID()
      val tagVal  = tx0.newIntVar(rootID, rootTag)
      val prevRef = tx0.newVar[KOpt](rootID, emptyKey)
      val nextRef = tx0.newVar[KOpt](rootID, emptyKey)
      new Map.Entry[S, A](this, rootID, tagVal, prevRef, nextRef)
    }
  }

  private final class MapEntrySerializer[S <: Sys[S], A](map: Map[S, A])
    extends Serializer[S#Tx, S#Acc, Map.Entry[S, A]] {

    private type E    = Map.Entry[S, A]
    private type KOpt = KeyOption[S, A]

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): E = {
      import map.keyOptionSer
      val id      = tx.readID(in, access)
      val tagVal  = tx.readIntVar(id, in)
      val prevRef = tx.readVar[KOpt](id, in)(keyOptionSer)
      val nextRef = tx.readVar[KOpt](id, in)(keyOptionSer)
      new Map.Entry[S, A](map, id, tagVal, prevRef, nextRef)
    }

    def write(v: E, out: DataOutput): Unit = v.write(out)
  }

  private final class KeyOptionSerializer[S <: Sys[S], A](map: Map[S, A])
    extends Serializer[S#Tx, S#Acc, KeyOption[S, A]] {

    private type KOpt = KeyOption[S, A]

    def write(v: KOpt, out: DataOutput): Unit = v.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): KOpt = {
      if (in.readByte() == 0) map.emptyKey
      else {
        val key = map.keySerializer.read(in, access)
        new DefinedKey(map, key)
      }
    }
  }

  /*
    * A special iterator used for the relabel observer.
    */
  private final class RelabelIterator[S <: Sys[S], A](recOff: Int, num: Int, recE: Map.Entry[S, A],
                                                      firstK: KeyOption[S, A],
                                                      entryView: A => Map.Entry[S, A])
    extends Iterator[S#Tx, A] {

    private var currK: KeyOption[S, A] = firstK
    private var cnt = 0

    def hasNext(implicit tx: S#Tx): Boolean = cnt < num

    def next()(implicit tx: S#Tx): A = {
      if (cnt == num) endReached()
      val res = currK.get
      cnt += 1
      // if we're reaching the recE, skip it.
      // that is to say, `num` is the returned iteration sequence,
      // while skipping the newly inserted entry (`recE`).
      // there may be the case that the last returned element
      // (`res`) has a `next` field pointing to `EmptyKey`.
      // This is the reason, why we _must not_ call `get` on the
      // value read from `next`. Instead, we store the key _option_
      // in `currK` and resolve it if there is another valid call
      // to `iterator.next()`.
      val currE = if (cnt == recOff) recE else entryView(res)
      currK = currE.next // .get
      res
    }

    def reset(): Unit = {
      currK = firstK
      cnt = 0
    }
  }

  sealed trait Map[S <: Sys[S], A] extends TotalOrder[S] {
    map =>

    override def toString = "Map" + id

    final type           E    = Map.Entry[S, A]
    final protected type KOpt = KeyOption[S, A]

    final private[TotalOrder] val emptyKey: KOpt = new EmptyKey[S, A]
    final implicit val EntrySerializer: Serializer[S#Tx, S#Acc, E] = new MapEntrySerializer[S, A](this)
    final private[TotalOrder] implicit val keyOptionSer: Serializer[S#Tx, S#Acc, KOpt] = new KeyOptionSerializer[S, A](this)

    protected def sizeVal: S#Var[Int]

    protected def observer: Map.RelabelObserver[S#Tx, A]

    private[TotalOrder] def keySerializer: Serializer[S#Tx, S#Acc, A]

    def entryView: A => E

    def root: E

    final def readEntry(in: DataInput, access: S#Acc)(implicit tx: S#Tx): E = EntrySerializer.read(in, access)

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      root   .dispose()
      sizeVal.dispose()
    }

    final protected def writeData(out: DataOutput): Unit = {
      out.writeByte(SER_VERSION)
      sizeVal.write(out)
      root   .write(out)
    }

    /** Creates a new _unlinked_ entry in the order. The actual insertion (linking)
      * must be done with a successive call to either `placeAfter` or `placeBefore`!
      */
    def insert()(implicit tx: S#Tx): E = {
      val id          = tx.newID()
      val recTagVal   = tx.newIntVar(id, -1)
      val recPrevRef  = tx.newVar[KOpt](id, emptyKey)
      val recNextRef  = tx.newVar[KOpt](id, emptyKey)
      new Map.Entry(this, id, recTagVal, recPrevRef, recNextRef)
    }

    def placeAfter(prev: A, key: A)(implicit tx: S#Tx): Unit = {
      val prevE = entryView(prev)
      val nextO = prevE.next
      placeBetween(prevE, new DefinedKey[S, A](map, prev), nextO.orNull, nextO, key)
    }

    def placeBefore(next: A, key: A)(implicit tx: S#Tx): Unit = {
      val nextE = entryView(next)
      val prevO = nextE.prev
      placeBetween(prevO.orNull, prevO, nextE, new DefinedKey[S, A](map, next), key)
    }

    private[TotalOrder] def placeBetween(prevE: E, prevO: KOpt, nextE: E, nextO: KOpt, key: A)
                                        (implicit tx: S#Tx): Unit = {
      val prevTag = if (prevE ne null) prevE.tag else 0 // could use Int.MinValue+1, but that collides with Octree max space
      val nextTag = if (nextE ne null) nextE.tag else Int.MaxValue

      // This assertion does _not_ hold: If we repeatedly prepend to the order,
      // we might end up with a next element having tag 0, which is the same
      // as prev if prev is empty
      //assert( prevTag < nextTag, "placeBetween - prev is " + prevTag + ", while next is " + nextTag )

      val recTag  = prevTag + ((nextTag - prevTag + 1) >>> 1)
      val recE    = entryView(key)

      require(recE.tag == -1 && prevTag >= 0 && nextTag >= 0, {
        val msg = new StringBuilder()
        if (recE.tag != -1) msg.append("Placed key was already placed before. ")
        if (prevTag < 0) msg.append("Predecessor of placed key has not yet been placed. ")
        if (nextTag < 0) msg.append("Successor of placed key has not yet been placed. ")
        msg.toString()
      })

      recE.updateTag(recTag)
      recE.updatePrev(prevO)
      recE.updateNext(nextO)
      val defK = new DefinedKey[S, A](this, key)
      if (prevE ne null) prevE.updateNext(defK)
      if (nextE ne null) nextE.updatePrev(defK)
      sizeVal.transform(_ + 1)
      if (recTag == nextTag) relabel(key, recE)
    }

    private[TotalOrder] def remove(e: E)(implicit tx: S#Tx): Unit = {
      val p = e.prev
      val n = e.next
      if (p.isDefined) p.orNull.updateNext(n)
      if (n.isDefined) n.orNull.updatePrev(p)
      sizeVal.transform(_ - 1)
    }

    final def size(implicit tx: S#Tx): Int = sizeVal()

    final def head(implicit tx: S#Tx): E = {
      @tailrec def step(e: E): E = {
        val prevO = e.prev
        if (prevO.isEmpty) e else step(prevO.orNull)
      }
      step(root)
    }

    final def tagList(from: E)(implicit tx: S#Tx): List[Int] = {
      val b = List.newBuilder[Int]
      @tailrec def step(e: E): List[Int] = {
        b += e.tag
        val nextO = e.next
        if (nextO.isEmpty) b.result()
        else {
          step(nextO.orNull)
        }
      }
      step(from)
    }

    /*
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
     * The multiplier controls the growth of the threshold density;
     * it is 2/T for the T parameter described by Bender et al.
     * Large multipliers lead to fewer relabels, while small items allow
     * us to handle more items with machine integer tags, so we vary the
     * multiplier dynamically to allow it to be as large as possible
     * without producing integer overflows."
     */
    private def relabel(recK: A, recE: E)(implicit tx: S#Tx): Unit = {
      var mask    = -1
      var thresh  = 1.0
      var num     = 1
      // val mul     = 2/((2*len(self))**(1/30.))
      val mul     = 2 / math.pow(size << 1, 1 / 30.0)
      var firstE  = recE
      var firstK  = recK
      var lastE   = recE
      var base    = recE.tag
      var recOff  = 0

      do {
        @tailrec def stepLeft(): Unit = {
          val prevO = firstE.prev
          if (prevO.isDefined) {
            val prevK = prevO.get
            val prevE = entryView(prevK)
            if ((prevE.tag & mask) == base) {
              firstE  = prevE
              firstK  = prevK
              num    += 1
              recOff += 1
              stepLeft()
            }
          }
        }
        stepLeft()

        @tailrec def stepRight(): Unit = {
          val nextO = lastE.next
          if (nextO.isDefined) {
            val nextE = entryView(nextO.get)
            if ((nextE.tag & mask) == base) {
              lastE = nextE
              num  += 1
              stepRight()
            }
          }
        }
        stepRight()

        if (num > 1) {
          val inc = -mask / num

          // important: we found a corner case where _first is the last
          // element in the list with a value of 0x7FFFFFFF. in this
          // case, if the predecessor is smaller in value, the original
          // algorithm would immediately terminate with num == 1, which
          // will obviously leave the tag unchanged! thus we must add
          // the additional condition that num is greater than 1!
          if (inc >= thresh) {
            // found rebalanceable range
            val numM1 = num - 1
            val relabelIter = if (recOff == 0) {
              new RelabelIterator(-1, numM1, recE, firstE.next, entryView)
            } else {
              new RelabelIterator(recOff, numM1, recE, new DefinedKey(this, firstK), entryView)
            }
            observer.beforeRelabeling(/* recK, */ relabelIter)

            // Note: this was probably a bug in Eppstein's code
            // -- it ran for one iteration less which made
            // the test suite fail for very dense tags. it
            // seems now it is correct with the inclusion
            // of last in the tag updating.
            var curr = firstE
            var cnt  = 0
            while (cnt < numM1) {
              curr.updateTag(base)
              val nextK = curr.next.get
              base     += inc
              cnt      += 1
              curr      = if (cnt == recOff) recE else entryView(nextK)
            }
            curr.updateTag(base) // last one
            relabelIter.reset()
            observer.afterRelabeling(/* recK, */ relabelIter)
            return
          }
        }
        mask  <<= 1 // next coarse step
        base   &= mask
        thresh *= mul
      } while (mask != 0)
      sys.error("label overflow")
    }
  }
}
sealed trait TotalOrder[S <: Sys[S]] extends Mutable[S#ID, S#Tx] {
  type E

  /**
   * The initial element created from which you can start to append and prepend.
   */
  def root: E

  /**
   * Returns the head element of the structure. Note that this
   * is O(n) worst case.
   */
  def head(implicit tx: S#Tx): E

  /**
   * The number of elements in the order. This is `1` for a newly
   * created order (consisting only of the root element).
   * You will rarely need this information except for debugging
   * purpose. The operation is O(1).
   */
  def size(implicit tx: S#Tx): Int

  def tagList(from: E)(implicit tx: S#Tx): List[Int]
}
