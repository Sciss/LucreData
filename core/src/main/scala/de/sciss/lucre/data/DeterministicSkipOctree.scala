/*
 *  DeterministicSkipOctree.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU Lesser General Public License v2.1+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package lucre
package data

import collection.immutable.{IndexedSeq => Vec}
import collection.mutable.{PriorityQueue => MPriorityQueue, Queue => MQueue}
import scala.annotation.{elidable, switch, tailrec}
import geom.{QueryShape, DistanceMeasure, Space}
import stm.{Identifiable, Sys, Mutable}
import serial.{Writable, DataInput, DataOutput, Serializer}
import java.io.PrintStream
import de.sciss.serial.impl.ByteArrayOutputStream

/** A transactional deterministic skip octree as outlined in the paper by Eppstein et al.
  * It is constructed from a given space (dimensions) and a skip-gap parameter
  * which determines the kind of skip list which is used to govern the
  * level decimation.
  *
  * The tree is a mutable data structure which supports lookup, insertion and removal
  * in O(log n), as well as efficient range queries and nearest neighbour search.
  *
  * The current implementation, backed by `impl.SkipOctreeImpl`, uses the types of
  * the `geom` package, assuming that coordinates are integers, with the maximum
  * root hyper-cube given by a span from `0` to `0x7FFFFFFF` (e.g. in `Space.IntTwoDim`,
  * this is `IntSquare( 0x40000000, 0x40000000, 0x40000000 )`.
  */
object DeterministicSkipOctree {
  private final val SER_VERSION = 79  // 'O'

  private var stat_rounds = 0
  private var stat_pq_add = 0
  private var stat_pq_rem = 0
  private val stat_print  = false

  @volatile private var sanitizing = false

  @elidable(elidable.CONFIG) private def stat_reset(): Unit = {
    stat_rounds = 0
    stat_pq_add = 0
    stat_pq_rem = 0
  }

  @elidable(elidable.CONFIG) private def stat_debug(what: => String): Unit =
    if (stat_print) println(s"<stat> $what")

  @elidable(elidable.CONFIG) private def stat_report() = ()
    // println(s"NN took $stat_rounds rounds, adding $stat_pq_add and removing $stat_pq_rem times to/from PQ")

  @elidable(elidable.CONFIG) private def stat_rounds1(obj: Any): Unit = {
    stat_rounds += 1
    if (stat_print) println(s"<stat> round max: $obj")
  }

  @elidable(elidable.CONFIG) private def stat_pq_add1(obj: Any): Unit = {
    stat_pq_add += 1
    if (stat_print) println(s"<stat> add    pq: $obj")
  }

  @elidable(elidable.CONFIG) private def stat_pq_rem1(obj: Any): Unit = {
    stat_pq_rem += 1
    if (stat_print) println(s"<stat> remove pq: $obj")
  }

  def empty[S <: Sys[S], D <: Space[D], A](hyperCube: D#HyperCube, skipGap: Int = 2)
                                          (implicit view: (A, S#Tx) => D#PointLike, tx: S#Tx, space: D,
                                           keySerializer: Serializer[S#Tx, S#Acc, A]): DeterministicSkipOctree[S, D, A] =
    new ImplNew[S, D, A](skipGap, tx.newID(), hyperCube, view, tx)

  def read[S <: Sys[S], D <: Space[D], A](in: DataInput, access: S#Acc)(
      implicit tx: S#Tx, view: (A, S#Tx) => D#PointLike, space: D,
      keySerializer: Serializer[S#Tx, S#Acc, A]): DeterministicSkipOctree[S, D, A] =
    new ImplRead[S, D, A](view, in, access, tx)

  implicit def serializer[S <: Sys[S], D <: Space[D], A](
      implicit view: (A, S#Tx) => D#PointLike, space: D,
      keySerializer: Serializer[S#Tx, S#Acc, A]): Serializer[S#Tx, S#Acc, DeterministicSkipOctree[S, D, A]] =
    new OctreeSerializer[S, D, A]

  private final class OctreeSerializer[S <: Sys[S], D <: Space[D], A](
    implicit view: (A, S#Tx) => D#PointLike, space: D, keySerializer: Serializer[S#Tx, S#Acc, A])
    extends Serializer[S#Tx, S#Acc, DeterministicSkipOctree[S, D, A]] {

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): DeterministicSkipOctree[S, D, A] = {
      new ImplRead[S, D, A](view, in, access, tx)
    }

    override def toString = "DeterministicSkipOctree.serializer"

    def write(v: DeterministicSkipOctree[S, D, A], out: DataOutput): Unit = v.write(out)
  }

  private final class ImplRead[S <: Sys[S], D <: Space[D], A](val pointView: (A, S#Tx) => D#PointLike, in: DataInput,
                                                              access: S#Acc, tx0: S#Tx)
                                                             (implicit val space: D,
                                                              val keySerializer: Serializer[S#Tx, S#Acc, A])
    extends DeterministicSkipOctree[S, D, A] {

    {
      val version = in.readByte()
      require(version == SER_VERSION,
        s"Incompatible serialized version (found $version, required $SER_VERSION).")
    }

    val id          = tx0.readID(in, access)
    val hyperCube   = space.hyperCubeSerializer.read(in, access)(tx0)
    val skipList    = {
      implicit val ord  = LeafOrdering
      implicit val r1   = LeafSerializer
      HASkipList.Set.serializer[S, LeafImpl](KeyObserver).read(in, access)(tx0)
    }
    val head        = LeftTopBranchSerializer.read(in, access)(tx0)
    val lastTreeRef = {
      implicit val r4 = TopBranchSerializer
      tx0.readVar[TopBranch](id, in)
    }
  }

  private final class ImplNew[S <: Sys[S], D <: Space[D], A](skipGap: Int, val id: S#ID, val hyperCube: D#HyperCube,
                                                             val pointView: (A, S#Tx) => D#PointLike, tx0: S#Tx)
                                                            (implicit val space: D,
                                                             val keySerializer: Serializer[S#Tx, S#Acc, A])
    extends DeterministicSkipOctree[S, D, A] {

    val skipList    = HASkipList.Set.empty[S, LeafImpl](skipGap, KeyObserver)(tx0, LeafOrdering, LeafSerializer)
    val head = {
      val sz  = numOrthants
      val ch  = tx0.newVarArray[LeftChildOption](sz)
      val cid = tx0.newID()
      implicit val r1 = LeftChildOptionSerializer
      var i = 0
      while (i < sz) {
        ch(i) = tx0.newVar[LeftChildOption](cid, EmptyValue)
        i += 1
      }
      implicit val r2 = RightOptionReader
      val headRight   = tx0.newVar[NextOption](cid, EmptyValue)
      new LeftTopBranch(cid, children = ch, nextRef = headRight)
    }
    val lastTreeRef = {
      implicit val r3 = TopBranchSerializer
      tx0.newVar[TopBranch](id, head)
    }
  }

  private def opNotSupported : Nothing = sys.error( "Operation not supported" )

  /* A debugging facility to inpect an octree only (not the skip list) for internal structural consistency.
   *
   * @param tree       the tree to inspect.
   * @param reportOnly if `true`, merely reports anomalies but does not try to resolve them. If `false` attempts to
   *                   fix corrupt entries.
   * @return           empty if there were no inconsistencies found, otherwise a list of textual descriptions
   *                   of the problems found
   */
  private def verifyOctreeConsistency[S <: Sys[S], D <: Space[D], A](tree: DeterministicSkipOctree[S, D, A],
                                                                     reportOnly: Boolean)
                                                                    (implicit tx: S#Tx): Vec[String] = {
    val q               = tree.hyperCube
    var level           = tree.numLevels
    var h: tree.Branch  = tree.lastTreeImpl
    var currUnlinkedOcs = Set.empty[D#HyperCube]
    var currPoints      = Set.empty[tree.LeafImpl]
    var errors          = Vec.empty[String]
    val repair          = !reportOnly

    do {
      if (h.hyperCube != q) {
        errors :+= s"Root level quad is ${h.hyperCube} while it should be $q in level $level"
      }
      val nextUnlinkedOcs = currUnlinkedOcs

      val nextPoints  = currPoints
      currUnlinkedOcs = Set.empty
      currPoints      = Set.empty

      def checkChildren(n: tree.Branch, depth: Int): Unit = {
        def assertInfo = s"in level $level / depth $depth"

        var i = 0
        while (i < tree.numOrthants) {
          n.child(i) match {
            case cb: tree.ChildBranch =>
              if (cb.parent != n) {
                errors :+= s"Child branch $cb has invalid parent ${cb.parent}, expected: $n $assertInfo"
                if (repair) {
                  (n, cb) match {
                    case (pl: tree.LeftBranch , cbl: tree.LeftChildBranch ) => cbl.parent = pl
                    case (pr: tree.RightBranch, cbr: tree.RightChildBranch) => cbr.parent = pr
                  }
                }
              }
              val nq = n.hyperCube.orthant(i)
              val cq = cb.hyperCube
              if (!nq.contains(cq)) {
                errors :+= s"Node has invalid hyper-cube ($cq), expected: $nq $assertInfo"
              }
              if (n.hyperCube.indexOf(cq) != i) {
                errors :+= s"Mismatch between index-of and used orthant ($i), with parent ${n.hyperCube} and $cq"
              }
              cb.nextOption match {
                case Some(next) =>
                  if (next.prevOption != Some(cb)) {
                    errors :+= s"Asymmetric next link $cq $assertInfo"
                  }
                  if (next.hyperCube != cq) {
                    errors :+= s"Next hyper-cube does not match ($cq vs. ${next.hyperCube}) $assertInfo"
                  }
                case None =>
                  if (nextUnlinkedOcs.contains(cq)) {
                    errors :+= s"Double missing link for $cq $assertInfo"
                  }
              }
              cb.prevOption match {
                case Some(prev) =>
                  if (prev.nextOption != Some(cb)) {
                    errors :+= s"Asymmetric prev link $cq $assertInfo"
                  }
                  if (prev.hyperCube != cq) {
                    errors :+= s"Next hyper-cube do not match ($cq vs. ${prev.hyperCube}) $assertInfo"
                  }
                case None => currUnlinkedOcs += cq
              }
              checkChildren(cb, depth + 1)

            case l: tree.LeafImpl =>
              currPoints += l // .value

            case _ =>
          }
          i += 1
        }
      }

      checkChildren(h, 0)
      val pointsOnlyInNext = nextPoints.filterNot(currPoints.contains)
      if (pointsOnlyInNext.nonEmpty) {
        errors :+= s"Points in next which aren't in current (${pointsOnlyInNext.take(10).map(_.value)}); in level $level"
        if (repair && level == 1) {
          assert(h.prevOption.isEmpty)

          def newNode(b: tree.LeftBranch, qidx: Int, iq: D#HyperCube)(implicit tx: S#Tx): tree.LeftChildBranch = {
            val sz  = tree.numOrthants // b.children.length
            val ch  = tx.newVarArray[tree.LeftChildOption](sz)
            val cid = tx.newID()
            var i = 0
            while (i < sz) {
              ch(i) = tx.newVar[tree.LeftChildOption](cid, tree.EmptyValue)(tree.LeftChildOptionSerializer)
              i += 1
            }
            val parentRef   = tx.newVar[tree.LeftBranch](cid, b)(tree.LeftBranchSerializer)
            val rightRef    = tx.newVar[tree.NextOption](cid, tree.EmptyValue)(tree.RightOptionReader)
            val n           = new tree.LeftChildBranch(
              cid, parentRef, iq, children = ch, nextRef = rightRef
            )
            b.updateChild(qidx, n)
            n
          }

          def insert(b: tree.LeftBranch, point: D#PointLike, leaf: tree.LeafImpl)(implicit tx: S#Tx): Unit = {
            val qidx = b.hyperCube.indexOf(point)
            b.child(qidx) match {
              case tree.EmptyValue =>
                b.updateChild(qidx, leaf)

              case old: tree.LeftNonEmptyChild =>
                // define the greatest interesting square for the new node to insert
                // in this node at qidx:
                val qn2 = old.union(b.hyperCube.orthant(qidx), point)
                // create the new node (this adds it to the children!)
                val n2 = newNode(b, qidx, qn2)
                val oidx = old.orthantIndexIn(qn2)
                n2.updateChild(oidx, old)
                val lidx = qn2.indexOf(point)
                assert(oidx != lidx)
                // This is a tricky bit! And a reason
                // why should eventually try to do without
                // parent pointers at all. Since `old`
                // may be a leaf whose parent points
                // to a higher level tree, we need to
                // check first if the parent is `this`,
                // and if so, adjust the parent to point
                // to the new intermediate node `ne`!
                if (old.parent == this) old.updateParentLeft(n2)
                n2.updateChild(lidx, leaf)
            }
          }

          h match {
            case lb: tree.LeftBranch =>
              pointsOnlyInNext.foreach { leaf =>
                val point = tree.pointView(leaf.value, tx)

                def goDown(b: tree.LeftBranch): Unit = {
                  val idx   = b.hyperCube.indexOf(point)
                  if (idx < 0) {
                    errors :+= s"Can't repair because $point is not in $lb"
                  } else {
                    b.child(idx) match {
                      case lb1: tree.LeftBranch => goDown(lb1)
                      case _ =>
                        insert(b, point, leaf)
                    }
                  }
                }

                goDown(lb)
              }

            case _ =>
              errors +:= "Can't repair because not in left branch !?"
          }
        }
      }
      h = h.prevOption.orNull
      level -= 1
    } while (h != null)

    errors
  }

  /** Checks the tree for correctness.
    *
    * @param reportOnly if `true` simply scans the tree, if `false` it will apply corrections if necessary
    * @return  empty if no problems were found, otherwise a list of strings describing the problems found
    */
  def verifyConsistency[S <: Sys[S], D <: Space[D], A](tree: DeterministicSkipOctree[S, D, A], reportOnly: Boolean)
                                                      (implicit tx: S#Tx): Vec[String] = {
    var errors    = Vec.empty[String]
    var repair    = !reportOnly

    val treeOnlyErrors = verifyOctreeConsistency(tree, reportOnly = reportOnly)
    errors ++= treeOnlyErrors
    if (treeOnlyErrors.nonEmpty) {
      repair    = false // stay on the safe side; require that consistency within tree is repaired first
    }

    // Take skip list as reference. Find if octree levels do not match skip list levels,
    // or whether points in the skip list are not found in the octree.
    tree.skipList.iterator.foreach { leaf =>
      val pv = tree.pointView(leaf.value, tx)

      @tailrec def findLeaf(b: tree.BranchLike = tree.lastTreeImpl,
                            lvl: Int = tree.numLevels, doPrint: Boolean = false): Option[(tree.BranchLike, Int)] = {
        if (doPrint) errors :+= s"...checking $b in level $lvl"
        val idx = b.hyperCube.indexOf(pv)
        b.child(idx) match {
          case `leaf` => Some(b -> lvl)

          case cb: tree.BranchLike if cb.hyperCube.contains(pv) =>
            findLeaf(cb, lvl = lvl, doPrint = doPrint)

          case _ =>
            b.prevOption match {
              case Some(pb: tree.BranchLike) =>
                findLeaf(pb, lvl = lvl - 1, doPrint = doPrint)

              case _ => None
            }
        }
      }

      findLeaf() match {
        case None =>
          val foundLevelSkip = HASkipList.debugFindLevel(tree.skipList, leaf)
          errors :+= s"Severe problem with $leaf - in skip list (level $foundLevelSkip) but octree does not find it"

          if (repair && foundLevelSkip == 1) { // this one is fixable
            try {
              sanitizing = true
              tree.skipList.remove(leaf)
            } finally {
              sanitizing = false
              return errors // hacky!!! skipList iterator possibly invalid, thus abort straight after removal
            }
          }

        case Some((foundParent, foundLevel)) =>
          val foundLevelSkip = HASkipList.debugFindLevel(tree.skipList, leaf)
          if (foundLevel != foundLevelSkip) {
            errors :+= s"Severe problem with $leaf - is in skip list level $foundLevelSkip versus octree level $foundLevel"
          }

          val parent  = leaf.parent
          val idx     = parent.hyperCube.indexOf(pv)
          if (idx < 0) {
            errors :+= s"Severe problem with $leaf - reported parent is $parent which doesn't contain the point $pv"
          } else {
            val saw   = parent.child(idx)
            if (saw != leaf) {
              errors :+= s"$leaf with point $pv reported parent $parent but in orthant $idx we see $saw"

              findLeaf(doPrint = true) match {
                case Some((b, _)) =>
                  errors :+= s"...that is the correct parent!"
                  if (repair) {
                    leaf.parent = b
                  }

                case None => errors :+= s"...this is bad. can't locate leaf!"
              }
            }
          }
        }
    }

    // Take octree as reference and see if it contains any points not in the skip list.
    val inSkipList = tree.skipList.toSet

    def checkInTreeLevel(b: tree.BranchLike, lvl: Int): Unit = {
      val sz = tree.numOrthants
      var i = 0
      while (i < sz) {
        b.child(i) match {
          case l: tree.LeafImpl if !inSkipList(l) =>
            errors :+= s"Only in octree level $lvl but not skip list: $l"
            if (repair) {
              println(s"\n============== BEFORE REMOVING $l ==============")
              println(tree.debugPrint())
              b.demoteLeaf(tree.pointView(l.value, tx), l)
              println(s"\n============== AFTER REMOVING $l ==============")
              println(tree.debugPrint())
              return  // XXX dirty - but if there is more than one wrong leaf, continuing may reinstall a lonley parent
            }

          case cb: tree.BranchLike =>
            checkInTreeLevel(cb, lvl)

          case _ =>
        }
        i += 1
      }
    }

    def checkInTree(t: tree.BranchLike, lvl: Int): Unit = {
      checkInTreeLevel(t, lvl)
      t.prevOption.foreach {
        case p: tree.BranchLike => checkInTree(p, lvl - 1)
      }
    }

    checkInTree(tree.lastTreeImpl, tree.numLevels)

    errors
  }
}

sealed trait DeterministicSkipOctree[S <: Sys[S], D <: Space[D], A]
  extends SkipOctree[S, D, A] {
  octree =>

  import DeterministicSkipOctree.{SER_VERSION, opNotSupported,
    stat_reset, stat_rounds1, stat_pq_add1, stat_pq_rem1, stat_report, stat_debug}

  private type Order = TotalOrder.Set.Entry[S]

  // ---- abstract types and methods ----

  implicit def space: D
  implicit def keySerializer: Serializer[S#Tx, S#Acc, A]

  protected def skipList: HASkipList.Set[S, LeafImpl]
  protected def head: LeftTopBranch
  protected def lastTreeRef: S#Var[TopBranch]

  // ----

  override def toString = s"Octree-${space.dim}d$id"

  protected object LeafOrdering extends Ordering[S#Tx, LeafImpl] {
    /** Leafs are ordered by the tree's in-order traversal,
      * where the quadrants I+II and III+IV can be thought
      * of as dummy nodes to binarize the octree. That is
      * to say, in a node, the child order corresponds to
      * their quadrant indices (I < II < III < IV).
      */
    def compare(a: LeafImpl, b: LeafImpl)(implicit tx: S#Tx): Int = {
      val pa = pointView(a.value, tx)
      val pb = pointView(b.value, tx)
      space.lexicalOrder.compare(pa, pb)
    }
  }

  implicit protected object RightBranchSerializer extends Serializer[S#Tx, S#Acc, RightBranch] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): RightBranch = {
      val cookie = in.readByte()
      val id = tx.readID(in, access)
      (cookie: @switch) match {
        case 4 => readRightTopBranch(in, access, id)
        case 5 => readRightChildBranch(in, access, id)
        case _ => sys.error("Unexpected cookie " + cookie)
      }
    }

    def write(v: RightBranch, out: DataOutput): Unit = v.write(out)
  }

  implicit protected object BranchSerializer extends Serializer[S#Tx, S#Acc, BranchLike] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): BranchLike = {
      val cookie = in.readByte()
      val id = tx.readID(in, access)
      (cookie: @switch) match {
        case 2 => readLeftTopBranch   (in, access, id)
        case 3 => readLeftChildBranch (in, access, id)
        case 4 => readRightTopBranch  (in, access, id)
        case 5 => readRightChildBranch(in, access, id)
        case _ => sys.error("Unexpected cookie " + cookie)
      }
    }

    def write(v: BranchLike, out: DataOutput): Unit = v.write(out)
  }

  protected object TopBranchSerializer extends Serializer[S#Tx, S#Acc, TopBranch] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): TopBranch = {
      val cookie = in.readByte()
      val id = tx.readID(in, access)
      (cookie: @switch) match {
        case 2 => readLeftTopBranch (in, access, id)
        case 4 => readRightTopBranch(in, access, id)
        case _ => sys.error("Unexpected cookie " + cookie)
      }
    }

    def write(v: TopBranch, out: DataOutput): Unit = v.write(out)
  }

  protected object LeftChildOptionSerializer extends Serializer[S#Tx, S#Acc, LeftChildOption] {
    //      def empty = EmptyValue
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): LeftChildOption = {
      val cookie = in.readByte()
      if (cookie == 0) return EmptyValue
      val id = tx.readID(in, access)
      (cookie: @switch) match {
        case 1 => readLeaf(in, access, id)
        case 3 => readLeftChildBranch(in, access, id)
        case _ => sys.error("Unexpected cookie " + cookie)
      }
    }

    def write(v: LeftChildOption, out: DataOutput): Unit = v.write(out)
  }

  implicit protected object LeftBranchSerializer extends Serializer[S#Tx, S#Acc, LeftBranch] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): LeftBranch = {
      val cookie = in.readByte()
      val id = tx.readID(in, access)
      (cookie: @switch) match {
        case 2 => readLeftTopBranch  (in, access, id)
        case 3 => readLeftChildBranch(in, access, id)
        case _ => sys.error("Unexpected cookie " + cookie)
      }
    }

    def write(v: LeftBranch, out: DataOutput): Unit = v.write(out)
  }

  implicit protected object RightChildOptionSerializer extends Serializer[S#Tx, S#Acc, RightChildOption] {
    //      def empty = EmptyValue
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): RightChildOption = {
      val cookie = in.readByte()
      if (cookie == 0) return EmptyValue
      val id = tx.readID(in, access)
      (cookie: @switch) match {
        case 1 => readLeaf(in, access, id)
        case 5 => readRightChildBranch(in, access, id)
        case _ => sys.error("Unexpected cookie " + cookie)
      }
    }

    def write(v: RightChildOption, out: DataOutput): Unit = v.write(out)
  }

  implicit protected object LeftTopBranchSerializer extends Serializer[S#Tx, S#Acc, LeftTopBranch] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): LeftTopBranch = {
      val cookie = in.readByte()
      require(cookie == 2, "Unexpected cookie " + cookie)
      val id = tx.readID(in, access)
      readLeftTopBranch(in, access, id)
    }

    def write(v: LeftTopBranch, out: DataOutput): Unit = v.write(out)
  }

  protected object RightOptionReader extends Serializer[S#Tx, S#Acc, NextOption] {
    //      def empty = EmptyValue
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): NextOption = {
      val cookie = in.readByte()
      if (cookie == 0) return EmptyValue
      val id = tx.readID(in, access)
      (cookie: @switch) match {
        case 4 => readRightTopBranch(in, access, id)
        case 5 => readRightChildBranch(in, access, id)
        case _ => sys.error("Unexpected cookie " + cookie)
      }
    }

    def write(v: NextOption, out: DataOutput): Unit = v.write(out)
  }

  protected object LeafSerializer extends Serializer[S#Tx, S#Acc, LeafImpl] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): LeafImpl = {
      val cookie = in.readByte()
      require(cookie == 1, "Unexpected cookie " + cookie)
      val id = tx.readID(in, access)
      readLeaf(in, access, id)
    }

    def write(l: LeafImpl, out: DataOutput): Unit = l.write(out)
  }

  implicit protected object KeyObserver extends SkipList.KeyObserver[S#Tx, LeafImpl] {
    def keyUp(l: LeafImpl)(implicit tx: S#Tx): Unit = {
      //println( "up : " + l )
      // "To insert x into Qi+1 we go from xi to pi(x) in Qi,
      //  then traverse upwards in Qi until we find the lowest
      //  ancestor q of x which is also interesting in Qi+1.
      //  (This is the reversed process of searching x in Qi
      //  with q = pi,start = pi+1,end so it takes at most 6
      //  steps by Lemma 5.) Then we go to the same square q
      //  in Qi+1 and insert x."

      /** The reverse process of `findP0`: Finds the lowest
        * common ancestor interesting node of this node
        * which is also contained in Qi+1. Returns this node
        * in Qi+1, or empty if no such node exists.
        */
      @tailrec def findPN(b: BranchLike): NextOption = b match {
        case tb: TopBranch    => tb.next
        case cb: ChildBranch  => cb.next match {
          case nb: BranchLike   => nb
          case EmptyValue       => findPN(cb.parent)
        }
      }

      val pNext = findPN(l.parent) match {
        case EmptyValue => // create new level
          val sz  = numOrthants
          val ch  = tx.newVarArray[RightChildOption](sz)
          val cid = tx.newID()
          var i   = 0
          while (i < sz) {
            ch(i) = tx.newVar[RightChildOption](cid, EmptyValue)
            i += 1
          }
          val nextRef   = tx.newVar[NextOption](cid, EmptyValue)(RightOptionReader)
          val prev      = lastTreeImpl
          val res       = new RightTopBranch(cid, prev, ch, nextRef)
          prev.next     = res
          lastTreeImpl  = res
          res
        case r: RightBranch => r
      }
      pNext.insert(pointView(l.value, tx), l)
    }

    def keyDown(l: LeafImpl)(implicit tx: S#Tx): Unit = {
      //println( "down : " + l )
      // "To delete x from Qi we go from xi to the smallest interesting
      //  square pi(x) containing x in Qi following the pointers. Then
      //  the deletion given pi(x) is as described in Section 2.3."

      l.parent.demoteLeaf(pointView(l.value, tx), l)
    }
  }

  final def numOrthants: Int = 1 << space.dim

  // 4 for R2, 8 for R3, 16 for R4, etc.

  sealed trait Child

  sealed trait Branch extends Child {
    def hyperCube: D#HyperCube
    def nextOption(implicit tx: S#Tx): Option[Branch]
    def prevOption: Option[Branch]
    def child(idx: Int)(implicit tx: S#Tx): Child
  }

  sealed trait Leaf extends Child {
    def value: A
  }

  sealed trait Empty extends Child

  final def headTree: Branch = head
  final def lastTree(implicit tx: S#Tx): Branch = lastTreeImpl

  final def write(out: DataOutput): Unit = {
    out.writeByte(SER_VERSION)
    id          .write(out)
    space.hyperCubeSerializer.write(hyperCube, out)
    skipList    .write(out)
    head        .write(out)
    lastTreeRef .write(out)
  }

  final def clear()(implicit tx: S#Tx): Unit = {
    val sz = numOrthants
    @tailrec def removeAllLeaves(b: BranchLike): Unit = {
      @tailrec def stepB(down: BranchLike, i: Int): ChildOption = {
        if (i == sz) down
        else b.child(i) match {
          case l: LeafImpl =>
            removeLeaf(pointView(l.value, tx), l)
            lastTreeImpl
          case _ => stepB(down, i + 1)
        }
      }

      @tailrec def step(i: Int): ChildOption = {
        if (i == sz) EmptyValue
        else b.child(i) match {
          case cb: BranchLike => stepB(cb, i + 1)
          case l: LeafImpl =>
            removeLeaf(pointView(l.value, tx), l)
            lastTreeImpl
          case _ => step(i + 1)
        }
      }

      step(0) match {
        case _: LeafOrEmpty   =>
        case next: BranchLike => removeAllLeaves(next)
      }
    }
    removeAllLeaves(lastTreeImpl)
  }

  final def dispose()(implicit tx: S#Tx): Unit = {
    //      val sz = numOrthants
    //
    //      def disposeBranch( b: BranchLike ): Unit = {
    //         var i = 0; while( i < sz ) {
    //            b.child( i ) match {
    //               case l: LeafImpl => l.remove()
    //               case b: BranchLike => disposeBranch( b )
    //               case _ =>
    //            }
    //         i += 1 }
    //         b.remove()
    //      }
    //
    //      @tailrec def disposeTree( b: BranchLike ): Unit = {
    //         disposeBranch( b )
    //         b match {
    //            case _: LeftBranch   =>
    //            case rb: RightBranch => disposeTree( rb.prev )
    //         }
    //      }
    //      disposeTree( lastTreeImpl )
    id          .dispose()
    lastTreeRef .dispose()
    head        .dispose()
    skipList    .dispose()
  }

  final def lastTreeImpl                   (implicit tx: S#Tx): TopBranch = lastTreeRef()
  final def lastTreeImpl_=(node: TopBranch)(implicit tx: S#Tx): Unit      = lastTreeRef() = node

  final def size(implicit tx: S#Tx): Int = skipList.size

  final def add(elem: A)(implicit tx: S#Tx): Boolean =
    insertLeaf(elem) match {
      case EmptyValue => true
      case oldLeaf: LeafImpl => oldLeaf.value != elem
    }

  final def update(elem: A)(implicit tx: S#Tx): Option[A] =
    insertLeaf(elem) match {
      case EmptyValue         => None
      case oldLeaf: LeafImpl  => Some(oldLeaf.value)
    }

  final def remove(elem: A)(implicit tx: S#Tx): Boolean =
    removeLeafAt(pointView(elem, tx)) != EmptyValue

  final def removeAt(point: D#PointLike)(implicit tx: S#Tx): Option[A] =
    removeLeafAt(point) match {
      case EmptyValue         => None
      case oldLeaf: LeafImpl  => Some(oldLeaf.value)
    }

  final def contains(elem: A)(implicit tx: S#Tx): Boolean = {
    val point = pointView(elem, tx)
    if (!hyperCube.contains(point)) return false
    findAt(point) match {
      case l: LeafImpl  => l.value == elem
      case _            => false
    }
  }

  final def isDefinedAt(point: D#PointLike)(implicit tx: S#Tx): Boolean = {
    if (!hyperCube.contains(point)) return false
    findAt(point) != EmptyValue
  }

  final def get(point: D#PointLike)(implicit tx: S#Tx): Option[A] = {
    if (!hyperCube.contains(point)) return None
    findAt(point) match {
      case l: LeafImpl  => Some(l.value)
      case _            => None
    }
  }

  final def nearestNeighbor[M](point: D#PointLike, metric: DistanceMeasure[M, D])
                                                 (implicit tx: S#Tx): A = {
    val nn = new NN(point, metric).find()
    stat_report()
    nn match {
      case EmptyValue   => throw new NoSuchElementException("nearestNeighbor on an empty tree")
      case l: LeafImpl  => l.value
    }
  }

  final def nearestNeighborOption[M](point: D#PointLike, metric: DistanceMeasure[M, D])
                                                       (implicit tx: S#Tx): Option[A] = {
    val nn = new NN(point, metric).find()
    stat_report()
    nn match {
      case EmptyValue => None
      case l: LeafImpl => Some(l.value)
    }
  }

  final def isEmpty(implicit tx: S#Tx): Boolean = {
    val n = head
    val sz = numOrthants
    @tailrec def step(i: Int): Boolean = if (i == sz) true
    else n.child(i) match {
      case n: NonEmptyChild => false
      case _ => step(i + 1)
    }
    step(0)
  }

  final def numLevels(implicit tx: S#Tx): Int = {
    @tailrec def step(b: BranchLike, num: Int): Int = {
      b.next match {
        case EmptyValue => num
        case n: BranchLike => step(n, num + 1)
      }
    }
    step(head, 1)
  }

  final def +=(elem: A)(implicit tx: S#Tx): this.type = {
    insertLeaf(elem)
    //      match {
    //         case oldLeaf: LeafImpl => oldLeaf.dispose()
    //         case _ =>
    //      }
    this
  }

  final def -=(elem: A)(implicit tx: S#Tx): this.type = {
    removeLeafAt(pointView(elem, tx))
    //      match {
    //         case oldLeaf: LeafImpl => oldLeaf.dispose()
    //         case _ =>
    //      }
    this
  }

  final def rangeQuery[Area](qs: QueryShape[Area, D])(implicit tx: S#Tx): Iterator[S#Tx, A] = {
    val q = new RangeQuery(qs)
    q.findNextValue()
    q
  }

  final def toIndexedSeq(implicit tx: S#Tx): Vec[A] = iterator.toIndexedSeq
  final def toList(implicit tx: S#Tx): List[A] = iterator.toList

  // note that `iterator.toSeq` produces a `Stream` !!
  final def toSeq(implicit tx: S#Tx): Seq[A] = iterator.toIndexedSeq

  final def toSet(implicit tx: S#Tx): Set[A] = iterator.toSet

  private def findAt(point: D#PointLike)(implicit tx: S#Tx): LeafOrEmpty = {
    val p0 = findP0(point) // lastTreeImpl.findP0( point )
    findLeafInP0(p0, point) // p0.findImmediateLeaf( point )
  }

  // OBSOLETE: the caller _must not call dispose_
  //
  // (( WARNING: if the returned oldLeaf is defined, the caller is
  // responsible for disposing it (after extracting useful information such as its value) ))
  private def insertLeaf(elem: A)(implicit tx: S#Tx): LeafOrEmpty = {
    val point = pointView(elem, tx)
    require(hyperCube.contains(point), point.toString + " lies out of root hyper-cube " + hyperCube)

    val p0  = findP0(point) // lastTreeImpl.findP0( point )
    val res = findLeafInP0(p0, point)

    res match {
      case EmptyValue =>
        val leaf = p0.insert(point, elem)
        skipList.add(leaf)

      case oldLeaf: LeafImpl =>
        // remove previous leaf
        removeLeaf(point, oldLeaf)
        // search anew
        val p0b = findP0(point) // lastTreeImpl.findP0( point )
        assert(findLeafInP0(p0b, point) == EmptyValue)
        val leaf = p0b.insert(point, elem)
        skipList.add(leaf)
    }

    res
  }

  // WARNING: if the returned oldLeaf is defined, the caller is
  // responsible for disposing it (after extracting useful information such as its value)
  private def removeLeafAt(point: D#PointLike)(implicit tx: S#Tx): LeafOrEmpty = {
    if (!hyperCube.contains(point)) return EmptyValue

    // "To insert or delete a point y into or from S, we first search the
    // quadtree structure to locate y in each Qi ..."
    val p0 = findP0(point) // lastTreeImpl.findP0( point )

    // "... Then we insert or delete y
    // in the binary Q0 and update our total order."

    val res = findLeafInP0(p0, point) // p0.findImmediateLeaf( point )

    res match {
      case l: LeafImpl  => removeLeaf(point, l)
      case _            =>
    }

    res
  }

  def transformAt(point: D#PointLike)(fun: Option[A] => Option[A])(implicit tx: S#Tx): Option[A] = {
    require(hyperCube.contains(point), s"$point lies out of root hyper-cube $hyperCube")

    val p0 = findP0(point)
    findLeafInP0(p0, point) match {
      case EmptyValue =>
        val res = None
        fun(res).foreach { elem =>
          val leaf = p0.insert(point, elem)
          skipList.add(leaf)
        }
        res

      case oldLeaf: LeafImpl =>
        // it's not possible currently to update a leaf's value...
        // remove previous leaf
        val res = Some(oldLeaf.value)
        removeLeaf(point, oldLeaf)
        fun(res).foreach {
          elem =>
          // search anew
            val p0b = findP0(point)
            assert(findLeafInP0(p0b, point) == EmptyValue)
            val leaf = p0b.insert(point, elem)
            skipList.add(leaf)
        }
        res
    }
  }

  /*
   * After arriving at this node from a `findP0` call, this resolves
   * the given point to an actual leaf.
   *
   * @return  the `Leaf` child in this node associated with the given
   *          `point`, or `empty` if no such leaf exists.
   */
  private def findLeafInP0(b: LeftBranch, point: D#PointLike)(implicit tx: S#Tx): LeafOrEmpty = {
    val qidx = b.hyperCube.indexOf(point)
    b.child(qidx) match {
      case l: LeafImpl if pointView(l.value, tx) == point => l
      case _ => EmptyValue
    }
  }

  /*
   * Finds to smallest interesting hyper-cube
   * in Q0, containing a given point. This method
   * traverses downwards into its children, or,
   * if the "bottom" has been reached, tries to
   * continue in Qi-1.
   *
   * @return  the node defined by the given search `point`, or `empty`
   *          if no such node exists.
   */
  private def findP0(point: D#PointLike)(implicit tx: S#Tx): LeftBranch = {
    @tailrec def stepLeft(lb: LeftBranch): LeftBranch = {
      val qidx = lb.hyperCube.indexOf(point)
      lb.child(qidx) match {
        case _: LeafOrEmpty => lb
        case cb: LeftBranch =>
          if (!cb.hyperCube.contains(point)) lb else stepLeft(cb)
      }
    }

    @tailrec def step(b: BranchLike): LeftBranch = b match {
      case lb: LeftBranch => stepLeft(lb)
      case rb: RightBranch =>
        val qidx = rb.hyperCube.indexOf(point)
        val n = rb.child(qidx) match {
          case cb: BranchLike if cb.hyperCube.contains(point) => cb
          case _ => rb.prev
        }
        step(n)
    }

    step(lastTreeImpl)
  }

  private def removeLeaf(point: D#PointLike, l: LeafImpl)(implicit tx: S#Tx): Unit = {
    // this will trigger removals from upper levels
    val skipOk = skipList.remove(l)
    assert(skipOk, s"Leaf $l with point $point was not found in skip list")
    // now l is in P0. demote it once more (this will dispose the leaf)
    l.parent.demoteLeaf(point /* pointView( l.value ) */ , l)
  }

  final def iterator(implicit tx: S#Tx): Iterator[S#Tx, A] = skipList.iterator.map(_.value)

  private final class NNIter[M](val bestLeaf: LeafOrEmpty, val bestDist: M, val rmax: M)

  private final class NN[M](point: D#PointLike, metric: DistanceMeasure[M, D])
    extends scala.math.Ordering[VisitedNode[M]] {

    stat_reset()

    // NOTE: `sz` must be protected and not private, otherwise
    // scala's specialization blows up
    protected val sz = numOrthants
    private val acceptedChildren = new Array[Branch](sz)
    //      private val acceptedDists     = {
    //         implicit val mf = metric.manifest
    //         new Array[ M ]( sz )
    //      }
    private val acceptedMinDists = metric.newArray(sz)
    // private val acceptedMaxDists = metric.newArray(sz)

  /* @tailrec */ private def findNNTailOLD(n0: /* Left */ Branch, pri: MPriorityQueue[VisitedNode[M]],
                                  _bestLeaf: LeafOrEmpty, _bestDist: M, _rmax: M)
                                 (implicit tx: S#Tx): NNIter[M] = {
    stat_rounds1(_rmax)
    var numAccepted   = 0
    var acceptedQidx  = 0

    var bestLeaf  = _bestLeaf
    var bestDist  = _bestDist
    var rmax      = _rmax

    var i = 0
    while (i < sz) {
      n0.child(i) match {
        case l: LeafImpl =>
          val ldist = metric.distance(point, pointView(l.value, tx))
          if (metric.isMeasureGreater(bestDist, ldist)) {   // found a point that is closer than previously known best result
            bestDist = ldist
            bestLeaf = l
            if (metric.isMeasureGreater(rmax, bestDist)) {  // update minimum required distance if necessary
              rmax = bestDist // note: we'll re-check acceptedChildren at the end of the loop
            }
          }
        case c: LeftBranch =>
          val cq = c.hyperCube
          val cMinDist = metric.minDistance(point, cq)
          if (!metric.isMeasureGreater(cMinDist, rmax)) {   // is less than or equal to minimum required distance
                                                            // (otherwise we're out already)
            val cMaxDist = metric.maxDistance(point, cq)
            if (metric.isMeasureGreater(rmax, cMaxDist)) {
              rmax = cMaxDist                               // found a new minimum required distance
            }
            acceptedChildren(numAccepted) = c
            acceptedMinDists(numAccepted) = cMinDist
            // acceptedMaxDists(numAccepted) = cMaxDist
            numAccepted += 1
            acceptedQidx = i                                // this will be used only if numAccepted == 1
          }
        case _ => // ignore empty orthants
      }
      i += 1
    }

    if (rmax != _rmax) {
      // recheck
      var j = 0
      while (j < numAccepted) {
        if (metric.isMeasureGreater(acceptedMinDists(j), rmax)) {
          // immediately kick it out
          numAccepted -= 1
          var k = j
          while (k < numAccepted) {
            val k1 = k + 1
            acceptedChildren(k) = acceptedChildren(k1)
            acceptedMinDists(k) = acceptedMinDists(k1)
            // acceptedMaxDists(k) = acceptedMaxDists(k1)
            k = k1
          }
        }
        j += 1
      }
    }

    // Unless exactly one child is accepted, round is over
//      if (numAccepted != 1) {
      /* var */ i = 0
      while (i < numAccepted) { // ...and the children are added to the priority queue
        val vn = new VisitedNode[M](acceptedChildren(i), acceptedMinDists(i) /*, acceptedMaxDists(i) */)
        stat_pq_add1(vn)
        pri += vn
        i += 1
      }
      new NNIter[M](bestLeaf, bestDist, rmax)

//      } else {
      // Otherwise find corresponding node in highest level, and descend
//        val dn0 = acceptedChildren(0)
      //        val qdn = dn0.hyperCube
      //
      //        @tailrec def findRight(cand: BranchLike, prev: BranchLike): BranchLike = {
      //          prev.next match {
      //            case EmptyValue => cand
      //            case next: BranchLike =>
      //              next.child(acceptedQidx) match {
      //                case _: LeafOrEmpty => cand
      //                case cb: BranchLike =>
      //                  if (cb.hyperCube != qdn) cand else findRight(cb, next)
      //              }
      //          }
      //        }
      //
      //        val dn = findRight(dn0, n0)
      //
      //        // now go left
      //        @tailrec def findLeft(n: BranchLike): LeftBranch = n match {
      //          case lb: LeftBranch   => lb
      //          case rb: RightBranch  => findLeft(rb.prev)
      //        }
      //
      //        val dnl = findLeft(dn)
      //        assert(dnl == dn0)  // if this assertion holds, that would make the whole process of going right/left useless
//        findNNTailOLD(dn0 /* dnl */, pri, bestLeaf, bestDist, rmax)
//      }
  }

    private def findNNTail(p0: Branch, pMinDist: M, pri: MPriorityQueue[VisitedNode[M]],
                           _bestLeaf: LeafOrEmpty, _bestDist: M, _rmax: M)
                          (implicit tx: S#Tx): NNIter[M] = {
      stat_rounds1(_rmax)

      var bestLeaf  = _bestLeaf
      var bestDist  = _bestDist
      var rmax      = _rmax

      def inspectLeaf(l: LeafImpl): Unit = {
        val ldist = metric.distance(point, pointView(l.value, tx))
        if (metric.isMeasureGreater(bestDist, ldist)) {   // found a point that is closer than previously known best result
          bestDist = ldist
          bestLeaf = l
          if (metric.isMeasureGreater(rmax, bestDist)) {  // update minimum required distance if necessary
            stat_debug(s"better NN candidate ${l.value}")
            rmax = bestDist // note: we'll re-check acceptedChildren at the end of the loop
          }
        }
      }

      // pMinDist = parent's minDist
      def scanChildren(parent: Branch): Int = {
        var numAccepted = 0
        var i           = 0
        while (i < sz) {
          parent.child(i) match {
            case l: LeafImpl => inspectLeaf(l)

            case c: Branch =>
              val cq        = c.hyperCube
              val cMinDist  = metric.minDistance(point, cq)
              if (cMinDist == pMinDist) {                       // equistabbing
                stat_debug(s"... scanChild $cq has minDist $cMinDist == pMinDist ($pMinDist)")
                // val cMaxDist  = metric.maxDistance(point, cq)
                acceptedChildren(numAccepted) = c
                acceptedMinDists(numAccepted) = cMinDist
                // acceptedMaxDists(numAccepted) = cMaxDist
                numAccepted += 1
              }

            case _ =>
          }
          i += 1
        }

        numAccepted
      }

      // TODO: this needs to use the skip structure
      def ancestorOf(b: Branch): Branch = {
        val h   = b.hyperCube

        @tailrec def loop(q: Branch): Branch = {
          val idx = q.hyperCube.indexOf(h)
          q.child(idx) match {
            case `b`        => q
            case c: Branch  => loop(c)
            case other      => sys.error(s"Unexpected child $other")
          }
        }
        loop(p0)
      }

      def pushChildren(b: Branch, skip: Int): Unit = {
        stat_debug(s"pushing child nodes of ${b.hyperCube} to priority queue")
        assert(b.isInstanceOf[LeftBranch])
        var i = 0
        while (i < sz) {
          if (i != skip) b.child(i) match {
            case l: LeafImpl => inspectLeaf(l)

            case c: Branch =>
              // val cq        = c.hyperCube

              // val cMinDist = metric.minDistance(point, cq)
              // val cMinDist  = {
              var j = 0
              // var min = metric.maxValue
              while (j < sz) {
                c.child(j) match {
                  case l: LeafImpl => inspectLeaf(l)
                  case _ =>
                }
                j += 1
              }
              j = 0
              while (j < sz) {
                c.child(j) match {
                  case cc: Branch =>
                    val cch   = cc.hyperCube
                    val cmin  = metric.minDistance(point, cch)
                    //                      if (!metric.isMeasureGreater(cmin, rmax) &&
                    //                           metric.isMeasureGreater( min, cmin)) min = cmin
                    if (!metric.isMeasureGreater(cmin, rmax)) {
                      // val cmax = metric.maxDistance(point, cch)
                      val vn = new VisitedNode(cc, cmin /* , cmax */)
                      pri += vn
                      stat_pq_add1(cch)
                    }

                  case _ =>
                }
                j += 1
              }

            // min
            // }

            //              // ---- added filter ----
            //              // if (!metric.isMeasureGreater(cMinDist, rmax)) {
            //              if (cMinDist != metric.maxValue) {
            //                val cMaxDist = metric.maxDistance(point, cq)  // ---- added ----
            //                val vn = new VisitedNode(c, cMinDist, cMaxDist)
            //                pri += vn
            //                stat_pq_add1(cq)
            //              }

            case _ =>
          }
          i += 1
        }
      }

      def finish(b: Branch): NNIter[M] = {
        val b0      = inLowestLevel(b)
        val numAnc  = 1 // XXX TODO needs to be retrieved from metric and numDim

        @tailrec def checkendorfer(b: Branch, si: Int, _anc: Int): Unit = {
          val a   = ancestorOf(b)
          val ai  = a.hyperCube.indexOf(b.hyperCube)
          var anc = _anc
          if (ai == si) {
            // pushChildren(a, si)
            var i = 0
            while (i < sz) {
              a.child(i) match {
                case l: LeafImpl  => inspectLeaf(l)
                case c: Branch    => if (i != si) pushChildren(c, -1)
                case _            =>
              }
              i += 1
            }
            anc -= 1
          }

          if (anc > 0 && a != p0) {
            checkendorfer(a, si, anc)
          }
        }

        pushChildren(b0, -1) // TODO: do not need to inspect leaves again!
        if (b0 != p0) metric.stabbingDirections(point, p0.hyperCube, b0.hyperCube).foreach { si =>
          stat_debug(s"searching for ancestor in ${if (si == 0) "SW" else if (si == 1) "SE" else if (si == 2) "NE" else if (si == 3) "NW"} direction")
          checkendorfer(b0, si, numAnc)
        }
        //                val cMinDist  = metric.minDistance(point, c.hyperCube)
        //                val cv  = new VisitedNode(c, cMinDist, null.asInstanceOf[M])
        //                pri += cv

        new NNIter(bestLeaf, bestDist, rmax)
      }

      @tailrec def loop(b: Branch): NNIter[M] = {
        val num = scanChildren(b)
        if (num >= 2) {
          stat_debug(s"found $num equistabbing children. stop here")
          finish(b)
        } else if (num == 1) {
          stat_debug(s"found 1 equistabbing child ${acceptedChildren(0).hyperCube}, minDist = ${acceptedMinDists(0)}. descend")
          loop(acceptedChildren(0))
        } else {
          b.prevOption match {
            case Some(prev) =>
              stat_debug(s"found no equistabbing children. go to previous tree ${prev.hyperCube}")
              loop(prev)
            case _ =>
              stat_debug("found no equistabbing children and ended in Q0. stop here")
              finish(b)
          }
        }
      }

      val ph = inHighestLevel(p0)
      stat_debug(s"begin round in ${ph.hyperCube}, minDist = $pMinDist")
      loop(ph)
    }

    @tailrec private def inHighestLevel(b: Branch)(implicit tx: S#Tx): Branch = b.nextOption match {
      case Some(n)  => inHighestLevel(n)
      case _        => b
    }

    @tailrec private def inLowestLevel(b: Branch)(implicit tx: S#Tx): Branch = b.prevOption match {
      case Some(n)  => inLowestLevel(n)
      case _        => b
    }

    //    private def findNNTailEquiPotency(n0: Branch, pri: MPriorityQueue[VisitedNode[M]],
    //                           _bestLeaf: LeafOrEmpty, _bestDist: M, _rmax: M)
    //                          (implicit tx: S#Tx): NNIter[M] = {
    //      stat_rounds1(_rmax)
    //
    //      var bestLeaf  = _bestLeaf
    //      var bestDist  = _bestDist
    //      var rmax      = _rmax
    //
    //      def inspectLeaf(l: LeafImpl): Unit = {
    //        val ldist = metric.distance(point, pointView(l.value, tx))
    //        if (metric.isMeasureGreater(bestDist, ldist)) {   // found a point that is closer than previously known best result
    //          bestDist = ldist
    //          bestLeaf = l
    //          if (metric.isMeasureGreater(rmax, bestDist)) {  // update minimum required distance if necessary
    //            rmax = bestDist // note: we'll re-check acceptedChildren at the end of the loop
    //          }
    //        }
    //      }
    //
    //      def scanChildren(b: Branch): Int = {
    //        var numAccepted = 0
    //        val oldRMax     = rmax
    //        var i           = 0
    //        while (i < sz) {
    //          b.child(i) match {
    //            case l: LeafImpl => inspectLeaf(l)
    //
    //            case c: Branch =>
    //              val cq = c.hyperCube
    //              val cMinDist = metric.minDistance(point, cq)
    //              if (!metric.isMeasureGreater(cMinDist, rmax)) {   // is less than or equal to minimum required distance
    //                                                                // (otherwise we're out already)
    //                val cMaxDist = metric.maxDistance(point, cq)
    //                if (metric.isMeasureGreater(rmax, cMaxDist)) {
    //                  rmax = cMaxDist                               // found a new minimum required distance
    //                }
    //                acceptedChildren(numAccepted) = c
    //                acceptedMinDists(numAccepted) = cMinDist
    //                acceptedMaxDists(numAccepted) = cMaxDist
    //                numAccepted += 1
    //                // acceptedQidx = i                                // this will be used only if numAccepted == 1
    //              }
    //
    //            case _ =>
    //          }
    //          i += 1
    //        }
    //
    //        if (rmax != oldRMax) {
    //          // recheck
    //          var j = 0
    //          while (j < numAccepted) {
    //            if (metric.isMeasureGreater(acceptedMinDists(j), rmax)) {
    //              // immediately kick it out
    //              numAccepted -= 1
    //              var k = j
    //              while (k < numAccepted) {
    //                val k1 = k + 1
    //                acceptedChildren(k) = acceptedChildren(k1)
    //                acceptedMinDists(k) = acceptedMinDists(k1)
    //                acceptedMaxDists(k) = acceptedMaxDists(k1)
    //                k = k1
    //              }
    //            }
    //            j += 1
    //          }
    //        }
    //
    //        numAccepted
    //      }
    //
    //      @tailrec def loop(b: Branch): NNIter[M] = {
    //        val num = scanChildren(b)
    //
    //        @tailrec def findEquipotent(i: Int): Int = {
    //          if (i == num) -1 else {
    //            val res = metric.isEquipotent(v = point, rmax = rmax, parent = b.hyperCube,
    //              child = acceptedChildren(i).hyperCube)
    //            if (res) i else findEquipotent(i + 1)
    //          }
    //        }
    //
    //        val eqi = findEquipotent(0)
    //        if (eqi >= 0) loop(acceptedChildren(eqi))
    //        else b.prevOption match {
    //          case Some(prev) => loop(prev)
    //          case _ =>
    //            var i = 0
    //            while (i < num) {
    //              val vn = new VisitedNode[M](acceptedChildren(i), acceptedMinDists(i), acceptedMaxDists(i))
    //              stat_pq_add1(vn)
    //              pri += vn
    //              i   += 1
    //            }
    //            new NNIter(bestLeaf, bestDist, rmax)
    //        }
    //      }
    //
    //      val nh  = inHighestLevel(n0)
    //      loop(nh)
    //    }

    //    private def findNNTailNO(n0: Branch, pri: MPriorityQueue[VisitedNode[M]],
    //                              _bestLeaf: LeafOrEmpty, _bestDist: M, _rmax: M)
    //                             (implicit tx: S#Tx): NNIter[M] = {
    //      stat_rounds1()
    //
    //      var numAccepted   = 0
    //      // var acceptedQidx  = 0
    //
    //      var bestLeaf  = _bestLeaf
    //      var bestDist  = _bestDist
    //      var rmax      = _rmax
    //
    //      def fall(parent: Branch, qidx: Int): Unit = {
    //        parent.child(qidx) match {
    //          case l: LeafImpl =>
    //            val ldist = metric.distance(point, pointView(l.value, tx))
    //            if (metric.isMeasureGreater(bestDist, ldist)) {   // found a point that is closer than previously known best result
    //              bestDist = ldist
    //              bestLeaf = l
    //              if (metric.isMeasureGreater(rmax, bestDist)) {  // update minimum required distance if necessary
    //                rmax = bestDist // note: we'll re-check acceptedChildren at the end of the loop
    //              }
    //            }
    //            parent.prevOption match {
    //              case Some(prev) => fall(prev, qidx)
    //              case _ =>
    //            }
    //
    //          case EmptyValue =>
    //            parent.prevOption match {
    //              case Some(prev) => fall(prev, qidx)
    //              case _ =>
    //            }
    //
    //          case c: Branch =>
    //            val cq = c.hyperCube
    //            val cMinDist = metric.minDistance(point, cq)
    //            if (!metric.isMeasureGreater(cMinDist, rmax)) {   // is less than or equal to minimum required distance
    //                                                              // (otherwise we're out already)
    //              val cMaxDist = metric.maxDistance(point, cq)
    //              if (metric.isMeasureGreater(rmax, cMaxDist)) {
    //                rmax = cMaxDist                               // found a new minimum required distance
    //              }
    //              acceptedChildren(numAccepted) = c
    //              acceptedMinDists(numAccepted) = cMinDist
    //              acceptedMaxDists(numAccepted) = cMaxDist
    //              numAccepted += 1
    //              // acceptedQidx = i                                // this will be used only if numAccepted == 1
    //            }
    //        }
    //      }
    //
    //      var i = 0
    //      while (i < sz) {
    //        fall(n0, i)
    //        i += 1
    //      }
    //
    //      if (rmax != _rmax) {
    //        // recheck
    //        var j = 0
    //        while (j < numAccepted) {
    //          if (metric.isMeasureGreater(acceptedMinDists(j), rmax)) {
    //            // immediately kick it out
    //            numAccepted -= 1
    //            var k = j
    //            while (k < numAccepted) {
    //              val k1 = k + 1
    //              acceptedChildren(k) = acceptedChildren(k1)
    //              acceptedMinDists(k) = acceptedMinDists(k1)
    //              acceptedMaxDists(k) = acceptedMaxDists(k1)
    //              k = k1
    //            }
    //          }
    //          j += 1
    //        }
    //      }
    //
    //      i = 0
    //      while (i < numAccepted) { // ...and the children are added to the priority queue
    //        val vn = new VisitedNode[M](acceptedChildren(i), acceptedMinDists(i), acceptedMaxDists(i))
    //        stat_pq_add1(vn)
    //        pri += vn
    //        i   += 1
    //      }
    //      new NNIter[M](bestLeaf, bestDist, rmax)
    //    }

    def find()(implicit tx: S#Tx): LeafOrEmpty = {
      val pri = MPriorityQueue.empty[VisitedNode[M]](this)
      @tailrec def step(p0: Branch, pMinDist: M, bestLeaf: LeafOrEmpty, bestDist: M, rmax: M): LeafOrEmpty = {
        val res = findNNTailOLD(p0, /* pMinDist, */ pri, bestLeaf, bestDist, rmax)
        if (metric.isMeasureZero(res.bestDist)) {
          res.bestLeaf   // found a point exactly at the query position, so stop right away
        } else {
          if (pri.isEmpty) res.bestLeaf
          else {
            val vis = pri.dequeue()
            stat_pq_rem1(vis.n.hyperCube)
            // if (!metric.isMeasureGreater(vis.minDist, res.rmax)) vis.n else pop()

            // because the queue is sorted by smallest minDist, if we find an element
            // whose minimum distance is greater than the maximum distance allowed,
            // we are done and do not need to process the remainder of the priority queue.

            if (metric.isMeasureGreater(vis.minDist, res.rmax)) res.bestLeaf else {
              val lb = vis.n
              step(lb, vis.minDist, res.bestLeaf, res.bestDist, res.rmax)
            }
          }
        }
      }

      val mmax      = metric.maxValue
      val p         = headTree // lastTree
      val pMinDist  = metric.minDistance(point, hyperCube)  // XXX could have metric.zero
      step(p, pMinDist, EmptyValue, mmax, mmax)
    }

    def compare(a: VisitedNode[M], b: VisitedNode[M]) = {
      val min = metric.compareMeasure(b.minDist, a.minDist)
      min // if (min != 0) min else metric.compareMeasure(b.maxDist, a.maxDist)
    }
  }

  private final class VisitedNode[M](val n: Branch, val minDist: M /*, val maxDist: M */) {
    override def toString = s"($n, min = $minDist" // , max = $maxDist)"
  }

  // note: Iterator is not specialized, hence we can safe use the effort to specialize in A anyway
  private final class RangeQuery[Area](qs: QueryShape[Area, D]) extends Iterator[S#Tx, A] {
    val sz          = numOrthants
    val stabbing    = MQueue.empty[(BranchLike, Area)]
    // Tuple2 is specialized for Long, too!
    val in          = MQueue.empty[NonEmptyChild]
    var current: A  = _
    // overwritten by initial run of `findNextValue`
    var hasNextVar  = true // eventually set to `false` by `findNextValue`

    stabbing += head -> qs.overlapArea(head.hyperCube)

    //      findNextValue()

    override def toString = octree.toString + ".rangeQuery(" + qs + ")"

    def hasNext(implicit tx: S#Tx): Boolean = hasNextVar

    // search downwards:
    // "At each square q ∈ Qi we either go to a child square in Qi
    // that covers the same area of R ∪ A as p does, if such a child
    // square exists, or jump to the next level q ∈ Qi−1."
    @tailrec private def findEquiStabbingTail(node: BranchLike, area: Area)(implicit tx: S#Tx): LeftBranch = {
      var pi = node
      var i = 0
      while (i < sz) {
        pi.child(i) match {
          case pic: BranchLike =>
            val a2 = qs.overlapArea(pic.hyperCube)
            if (a2 == area) {
              pi = pic
              i = 0 // start over in child
            } else {
              i += 1
            }
          case _ => i += 1
        }
      }
      // ... or jump to the next (previous) level
      pi match {
        case lb: LeftBranch => lb
        case rb: RightBranch => findEquiStabbingTail(rb.prev, area)
      }
    }

    // the movement from Q0 to Qj
    // "assuming that p is not critical in Q0, we promote to Qj where Qj is the highest
    // level in which p is not a critical square"
    //
    // definition of critical square:
    // "a stabbing node of Qi whose child nodes are either not stabbing, or still
    // stabbing but cover less volume of R than p does."
    // ; bzw. umgedreht: eine unkritische node ist eine, in der es mindestens eine stabbing node
    // mit derselben ueberlappungsflaeche gibt!
    //
    // definition stabbing: 0 < overlap-area < area-of-p
    @tailrec def findHighestUncritical(p0: BranchLike, area: Area)(implicit tx: S#Tx): BranchLike = {
      @tailrec def isCritical(b: BranchLike, i: Int): Boolean = {
        i < sz && (b.child(i) match {
          // if there is any child which has the same overlap area, it means the node is uncritical
          case ci: BranchLike if qs.overlapArea(ci.hyperCube) == area => true
          case _ => isCritical(b, i + 1)
        })
      }

      p0.next match {
        case EmptyValue => p0
        case pi: BranchLike => if (isCritical(pi, 0)) p0 else findHighestUncritical(pi, area)
      }
    }

    def next()(implicit tx: S#Tx): A = {
      if (!hasNextVar) endReached()
      val res = current
      findNextValue()
      res
    }

    def findNextValue()(implicit tx: S#Tx): Unit = {
      while (true) {
        if (in.isEmpty) {
          if (stabbing.isEmpty) {
            hasNextVar = false
            return
          }
          val tup = stabbing.dequeue()
          val ns  = tup._1 // stabbing node
          val as  = tup._2 // overlapping area with query shape
          val hi  = findHighestUncritical(ns, as) // find highest uncritical hyper-cube of the stabbing node
          val nc  = findEquiStabbingTail (hi, as) // now traverse towards Q0 to find the critical square

          var i = 0
          while (i < sz) {
            nc.child(i) match {
              case cl: LeafImpl =>
                if (qs.contains(pointView(cl.value, tx))) in += cl
              case cn: ChildBranch =>
                val q   = cn.hyperCube
                val ao  = qs.overlapArea(q)
                // test for stabbing or inclusion:
                // inclusion: overlap-area == area-of-p
                // stabbing: 0 < overlap-area < area-of-p
                if (qs.isAreaNonEmpty(ao)) {
                  // q is _not_ out
                  if (qs.isAreaGreater(q, ao)) {
                    // q is stabbing
                    stabbing += cn -> ao
                  } else {
                    // q is in
                    in += cn
                  }
                }
              case _ =>
            }
            i += 1
          }

        } else {
          // XXX scalac currently complains that this match doesn't account
          // for LeftChildBranch and RightChildBranch. but both are
          // captured by BranchLike, so this seems to be a bug.
          (in.dequeue(): @unchecked) match {
            case l: LeafImpl =>
              current = l.value
              return
            case n: BranchLike =>
              var i = 0
              while (i < sz) {
                n.child(i) match {
                  case cc: NonEmptyChild => in += cc // sucky `enqueue` creates intermediate Seq because of varargs
                  case _ =>
                }
                i += 1
              }
          }
        }
      }
    }
  }

  final protected type ChildOption = Child with Writable

  /** A node is an object that can be stored in a orthant of a branch. */
  protected sealed trait NonEmpty extends Identifiable[S#ID] /* extends Down with Child */ {
    protected def shortString: String

    override def toString = shortString + id

    override def equals(that: Any): Boolean = that match {
      case n: Identifiable[_] => id == n.id   // do _not_ match against n: NonEmpty because that's an inner class!!!
      case _                  => super.equals(that)
    }

    override def hashCode = id.hashCode()

    /** Computes the greatest interesting hyper-cube within
      * a given hyper-cube `mq` so that this (leaf's or node's)
      * hyper-cube and the given point will be placed in
      * separated orthants of this resulting hyper-cube.
      */
    def union(mq: D#HyperCube, point: D#PointLike)(implicit tx: S#Tx): D#HyperCube

    /**
     * Queries the orthant index for this (leaf's or node's) hyper-cube
     * with respect to a given outer hyper-cube `iq`.
     */
    def orthantIndexIn(iq: D#HyperCube)(implicit tx: S#Tx): Int
  }

  /** A tree element in Q0 has markers for the
    * in-order traversal. NOT ANYMORE
    */
  protected sealed trait LeftNonEmpty extends Left with NonEmpty

  protected sealed trait Left
  protected sealed trait LeftChild extends Left with Child
  protected type LeftChildOption = LeftChild with Writable

  /** A common trait used in pattern matching, comprised of `Leaf` and `LeftChildBranch`. */
  protected sealed trait LeftNonEmptyChild extends LeftNonEmpty with NonEmptyChild with LeftChild with Writable /* Mutable[ S ] */ {
    def updateParentLeft(p: LeftBranch)(implicit tx: S#Tx): Unit
  }

  protected sealed trait RightChild extends Child
  protected type RightChildOption = RightChild with Writable

  /** A common trait used in pattern matching, comprised of `Leaf` and `RightChildBranch`. */
  protected sealed trait RightNonEmptyChild extends RightChild with NonEmptyChild with Writable {
    def updateParentRight(p: RightBranch)(implicit tx: S#Tx): Unit
  }

  /*
   * Serialization-id: 1
   */
  private def readLeaf(in: DataInput, access: S#Acc, id: S#ID)(implicit tx: S#Tx): LeafImpl = {
    val value     = keySerializer.read(in, access)
    val parentRef = tx.readVar[BranchLike](id, in)
    new LeafImpl(id, value, parentRef)
  }

  /** A leaf in the octree, carrying a map entry
    * in the form of a point and associated value.
    * Note that a single instance of a leaf is used
    * across the levels of the octree! That means
    * that multiple child pointers may go to the
    * same leaf, while the parent of a leaf always
    * points into the highest level octree that
    * the leaf resides in, according to the skiplist.
    */
  protected final class LeafImpl(val id: S#ID, val value: A, /* val point: D#PointLike, */ parentRef: S#Var[BranchLike])
    extends LeftNonEmptyChild with RightNonEmptyChild with LeafOrEmpty with Leaf {

    def updateParentLeft (p: LeftBranch )(implicit tx: S#Tx): Unit = parent_=(p)
    def updateParentRight(p: RightBranch)(implicit tx: S#Tx): Unit = parent_=(p)

    def parent                 (implicit tx: S#Tx): BranchLike  = parentRef()
    def parent_=(p: BranchLike)(implicit tx: S#Tx): Unit        = parentRef() = p

    def isLeaf                = true
    def isBranch              = false
    def asLeaf: LeafImpl      = this
    def asBranch: BranchLike  = opNotSupported

    def dispose()(implicit tx: S#Tx): Unit = {
      id.dispose()
      parentRef.dispose()
    }

    def write(out: DataOutput): Unit = {
      out.writeByte(1)
      id.write(out)
      keySerializer.write(value, out)
      parentRef.write(out)
    }

    def union(mq: D#HyperCube, point2: D#PointLike)(implicit tx: S#Tx): D#HyperCube =
      mq.greatestInteresting(pointView(value, tx), point2)

    def orthantIndexIn(iq: D#HyperCube)(implicit tx: S#Tx): Int =
      iq.indexOf(pointView(value, tx))

    def shortString = s"Leaf($value)"

    def remove()(implicit tx: S#Tx): Unit = dispose()
  }

  /** Nodes are defined by a hyperCube area as well as a list of children,
    * as well as a pointer `next` to the corresponding node in the
    * next highest tree. A `Branch` also provides various search methods.
    */
  protected sealed trait BranchLike extends NonEmpty with Writable /* Mutable[ S ] */ with Branch {
    /** Returns the child for a given orthant index. */
    def child(idx: Int)(implicit tx: S#Tx): ChildOption

    /** Assuming that the given `leaf` is a child of this node,
      * removes the child from this node's children. This method
      * will perform further clean-up such as merging this node
      * with its parent if it becomes uninteresting as part of the
      * removal.
      */
    def demoteLeaf(point: D#PointLike, leaf: LeafImpl)(implicit tx: S#Tx): Unit

    /** Returns the hyper-cube covered by this node. */
    def hyperCube: D#HyperCube

    /** Returns the corresponding interesting
      * node in Qi+1, or `empty` if no such
      * node exists.
      */
    final def next(implicit tx: S#Tx): NextOption = nextRef()

    final def nextOption(implicit tx: S#Tx): Option[BranchLike] = next match {
      case EmptyValue     => None
      case b: BranchLike  => Some(b)
    }

    /** Sets the corresponding interesting
      * node in Qi+1.
      */
    final def next_=(node: NextOption)(implicit tx: S#Tx): Unit = nextRef() = node

    protected def nextRef: S#Var[NextOption]

    final def union(mq: D#HyperCube, point2: D#PointLike)(implicit tx: S#Tx): D#HyperCube = {
      val q = hyperCube
      mq.greatestInteresting(q, point2)
    }

    final def orthantIndexIn(iq: D#HyperCube)(implicit tx: S#Tx): Int = iq.indexOf(hyperCube)

    /** Called when a leaf has been removed from the node.
      * The node may need to cleanup after this, e.g. promote
      * an under-full node upwards.
      */
    protected def leafRemoved()(implicit tx: S#Tx): Unit

    protected def nodeName: String

    final protected def shortString = s"$nodeName($hyperCube)"

    final def isLeaf                = false
    final def isBranch              = true
    final def asBranch: BranchLike  = this
    final def asLeaf: LeafImpl      = opNotSupported
  }

  /** An inner non empty tree element has a mutable parent node. */
  sealed trait NonEmptyChild extends NonEmpty with Child {
    def parent(implicit tx: S#Tx): BranchLike
  }

  protected sealed trait LeafOrEmpty extends LeftChild

  // fix for deserialization equality problem thanks to
  // Eugene Yokota
  // (http://stackoverflow.com/questions/9893522/fixing-case-object-identity-pattern-matching-under-serialization/9894036#9894036)
  case object EmptyValue extends LeftChild with RightChild with Next with LeafOrEmpty with Empty with Writable /* EmptyMutable */ {
    override def toString = "<empty>"

    def write(out: DataOutput): Unit = out.writeByte(0)

    override def hashCode: Int = 0

    // grmpff....

    import language.existentials

    override def equals(that: Any): Boolean =
      that.isInstanceOf[x.EmptyValue.type forSome {val x: DeterministicSkipOctree[_, _, _]}]
  }

  /** Utility trait which elements the rightward search `findPN`. */
  protected sealed trait ChildBranch extends BranchLike with NonEmptyChild

  protected sealed trait Next

  final protected type NextOption = Next with Writable

  /** A right tree node implementation provides more specialized child nodes
    * of type `RightChild`. It furthermore defines the node in Qi-1 via the
    * `prev` method.
    */
  protected sealed trait RightBranch extends Next with BranchLike {
    protected def children: Array[S#Var[RightChildOption]]

    final def prevOption: Option[Branch] = Some(prev: Branch)
    def prev: BranchLike

    final def child      (idx: Int)                     (implicit tx: S#Tx): RightChildOption = children(idx)()
    final def updateChild(idx: Int, c: RightChildOption)(implicit tx: S#Tx): Unit             = children(idx)() = c

    /** Promotes a leaf that exists in Qi-1 to this
      * tree, by inserting it into this node which
      * is its interesting node in Qi.
      *
      * If the result of insertion is a new child node
      * below this node, this intermediate node will
      * be connected to Qi by looking for the corresponding
      * hyper-cube in the given search path that led here
      * (i.e. that was constructed in `findPN`).
      *
      * This method also sets the parent of the leaf
      * accordingly.
      */
    final def insert(point: D#PointLike, leaf: LeafImpl)(implicit tx: S#Tx): Unit = {
      //         val point   = pointView( leaf.value )
      val qidx = hyperCube.indexOf(point)
      child(qidx) match {
        case EmptyValue =>
          updateChild(qidx, leaf)
          leaf.parent = this
        case old: RightNonEmptyChild =>
          // determine the greatest interesting square for the new
          // intermediate node to create
          val qn2 = old.union(hyperCube.orthant(qidx), point)
          // find the corresponding node in the lower tree
          @tailrec def findInPrev(b: BranchLike): BranchLike = {
            if (b.hyperCube == qn2) b
            else {
              val idx = b.hyperCube.indexOf(point)
              b.child(idx) match {
                case _: LeafOrEmpty => sys.error("Internal error - cannot find sub-cube in prev")
                case cb: BranchLike => findInPrev(cb)
              }
            }
          }
          val pPrev = findInPrev(prev)
          val n2    = newNode(qidx, pPrev, qn2)
          val oidx  = old.orthantIndexIn(qn2)
          n2.updateChild(oidx, old)
          // This is a tricky bit! And a reason
          // why should eventually try to do without
          // parent pointers at all. Since `old`
          // may be a leaf whose parent points
          // to a higher level tree, we need to
          // check first if the parent is `this`,
          // and if so, adjust the parent to point
          // to the new intermediate node `ne`!
          if (old.parent == this) old.updateParentRight(n2)
          val lIdx = qn2.indexOf(point)
          n2.updateChild(lIdx, leaf)
          leaf.parent = n2
      }
    }

    /*
     * Instantiates an appropriate
     * sub-node whose parent is this node, and whose predecessor
     * in the lower octree is given.
     *
     * @param   qidx  the orthant index in this node where the node belongs
     * @param   prev  the new node's prev field, i.e. its correspondant in
     *                Qi-1
     * @param   iq    the hyper-cube for the new node
     * @return  the new node which has already been inserted into this node's
     *          children at index `qidx`.
     */
    @inline private def newNode(qidx: Int, prev: BranchLike, iq: D#HyperCube)
                               (implicit tx: S#Tx): RightChildBranch = {
      val sz  = children.length
      val ch  = tx.newVarArray[RightChildOption](sz)
      val cid = tx.newID()
      var i = 0
      while (i < sz) {
        ch(i) = tx.newVar[RightChildOption](cid, EmptyValue)
        i += 1
      }
      val parentRef = tx.newVar[RightBranch](cid, this)
      val rightRef  = tx.newVar[NextOption](cid, EmptyValue)(RightOptionReader)
      val n         = new RightChildBranch(cid, parentRef, prev, iq, ch, rightRef)
      prev.next     = n
      updateChild(qidx, n)
      n
    }

    final def demoteLeaf(point: D#PointLike, leaf: LeafImpl)(implicit tx: S#Tx): Unit = {
      val qIdx = hyperCube.indexOf(point)
      assert(child(qIdx) == leaf)
      updateChild(qIdx, EmptyValue)

      @tailrec def findParent(b: BranchLike, idx: Int): BranchLike = b.child(idx) match {
        case sl: LeafImpl   => assert(sl == leaf); b
        case cb: BranchLike => findParent(cb, cb.hyperCube.indexOf(point))
      }

      val newParent = findParent(prev, qIdx)
      leafRemoved()
      leaf.parent = newParent
    }
  }

  /** A left tree node implementation provides more specialized child nodes
    * of type `LeftChild`. It furthermore defines a resolution method
    * `findImmediateLeaf` which is typically called after arriving here
    * from a `findP0` call.
    */
  protected sealed trait LeftBranch extends /* LeftBranchOption with */ BranchLike with LeftNonEmpty {
    /** For a `LeftBranch`, all its children are more specific
      * -- they are instances of `LeftChild` and thus support
      * order intervals.
      */
    protected def children: Array[S#Var[LeftChildOption]]

    final def prevOption: Option[Branch] = None

    final def child      (idx: Int)                    (implicit tx: S#Tx): LeftChildOption = children(idx)()
    final def updateChild(idx: Int, c: LeftChildOption)(implicit tx: S#Tx): Unit            = children(idx)() = c

    final def demoteLeaf(point: D#PointLike, leaf: LeafImpl)(implicit tx: S#Tx): Unit = {
      val qIdx  = hyperCube.indexOf(point)
      val ok    = child(qIdx) == leaf
      if (ok) {
        updateChild(qIdx, EmptyValue)
        leafRemoved()
        leaf.remove() // dispose()
      } else {
        if (!DeterministicSkipOctree.sanitizing)
          assert(assertion = false, s"Internal error - expected $leaf not found in $this")
      }
    }

    final def insert(point: D#PointLike, value: A)(implicit tx: S#Tx): LeafImpl = {
      val qIdx = hyperCube.indexOf(point)
      child(qIdx) match {
        case EmptyValue =>
          newLeaf(qIdx, /* point, */ value) // (this adds it to the children!)

        case old: LeftNonEmptyChild =>
          // define the greatest interesting square for the new node to insert
          // in this node at qidx:
          val qn2 = old.union(hyperCube.orthant(qIdx), point)
          // create the new node (this adds it to the children!)
          val n2 = newNode(qIdx, qn2)
          val oIdx = old.orthantIndexIn(qn2)
          n2.updateChild(oIdx, old)
          val lIdx = qn2.indexOf(point)
          assert(oIdx != lIdx)
          // This is a tricky bit! And a reason
          // why should eventually try to do without
          // parent pointers at all. Since `old`
          // may be a leaf whose parent points
          // to a higher level tree, we need to
          // check first if the parent is `this`,
          // and if so, adjust the parent to point
          // to the new intermediate node `ne`!
          if (old.parent == this) old.updateParentLeft(n2)
          (n2: LeftBranch).newLeaf(lIdx, value)   // cf. SI-8432
      }
    }

    /** Instantiates an appropriate
      * leaf whose parent is this node, and which should be
      * ordered according to its position in this node.
      *
      * @param   qIdx  the orthant index of the new leaf in this node
      * @param   value the value associated with the new leaf
      * @return  the new leaf which has already assigned this node as
      *          parent and is already stored in this node's children
      *          at index `qIdx`
      */
    private def newLeaf(qIdx: Int, value: A)(implicit tx: S#Tx): LeafImpl = {
      val leafID    = tx.newID()
      val parentRef = tx.newVar[BranchLike](leafID, this)
      val l         = new LeafImpl(leafID, value, parentRef)
      updateChild(qIdx, l)
      l
    }

    /*
     * Instantiates an appropriate
     * sub-node whose parent is this node, and which should be
     * ordered according to its position in this node.
     *
     * @param   qidx  the orthant index of the new node in this (parent) node
     * @param   iq    the hyper-cube of the new node
     * @return  the new node which has already assigned this node as
     *          parent and is already stored in this node's children
     *          at index `qIdx`
     */
    private def newNode(qIdx: Int, iq: D#HyperCube)(implicit tx: S#Tx): LeftChildBranch = {
      val sz  = children.length
      val ch  = tx.newVarArray[LeftChildOption](sz)
      val cid = tx.newID()
      var i = 0
      while (i < sz) {
        ch(i) = tx.newVar[LeftChildOption](cid, EmptyValue)(LeftChildOptionSerializer)
        i += 1
      }
      val parentRef   = tx.newVar[LeftBranch](cid, this)
      val rightRef    = tx.newVar[NextOption](cid, EmptyValue)(RightOptionReader)
      val n           = new LeftChildBranch(
        cid, parentRef, iq, children = ch, nextRef = rightRef
      )
      updateChild(qIdx, n)
      n
    }
  }

  protected sealed trait TopBranch extends BranchLike {
    final def hyperCube: D#HyperCube = octree.hyperCube
  }

  /*
   * Serialization-id: 2
   */
  private def readLeftTopBranch(in: DataInput, access: S#Acc, id: S#ID)(implicit tx: S#Tx): LeftTopBranch = {
    val sz  = numOrthants
    val ch  = tx.newVarArray[LeftChildOption](sz)
    var i = 0
    while (i < sz) {
      ch(i) = tx.readVar[LeftChildOption](id, in)(LeftChildOptionSerializer)
      i += 1
    }
    val nextRef = tx.readVar[NextOption](id, in)(RightOptionReader)
    new LeftTopBranch(id, children = ch, nextRef = nextRef)
  }

  protected final class LeftTopBranch(val id: S#ID,
                                      protected val children: Array[S#Var[LeftChildOption]],
                                      protected val nextRef: S#Var[NextOption])
    extends LeftBranch with TopBranch with Mutable[S#ID, S#Tx] {
    // that's alright, we don't need to do anything special here
    protected def leafRemoved()(implicit tx: S#Tx) = ()

    def dispose()(implicit tx: S#Tx): Unit = {
      id.dispose()
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).dispose()
        i += 1
      }
      nextRef.dispose()
    }

    def write(out: DataOutput): Unit = {
      out.writeByte(2)
      id.write(out)
      // no need to write the hyperCube?
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).write(out)
        i += 1
      }
      nextRef.write(out)
    }

    protected def nodeName = "LeftTop"
  }

  /*
   * Serialization-id: 3
   */
  private def readLeftChildBranch(in: DataInput, access: S#Acc, id: S#ID)(implicit tx: S#Tx): LeftChildBranch = {
    val parentRef   = tx.readVar[LeftBranch](id, in)
    val hyperCube   = space.hyperCubeSerializer.read(in)
    val sz          = numOrthants
    val ch          = tx.newVarArray[LeftChildOption](sz)
    var i = 0
    while (i < sz) {
      ch(i) = tx.readVar[LeftChildOption](id, in)(LeftChildOptionSerializer)
      i += 1
    }
    val nextRef = tx.readVar[NextOption](id, in)(RightOptionReader)
    new LeftChildBranch(id, parentRef, hyperCube, children = ch, nextRef = nextRef)
  }

  protected final class LeftChildBranch(val id: S#ID, parentRef: S#Var[LeftBranch], val hyperCube: D#HyperCube,
                                      protected val children: Array[S#Var[LeftChildOption]],
                                      protected val nextRef: S#Var[NextOption])
    extends LeftBranch with ChildBranch with LeftNonEmptyChild {
    protected def nodeName = "LeftInner"

    def updateParentLeft(p: LeftBranch)(implicit tx: S#Tx): Unit = parent = p

    def parent                    (implicit tx: S#Tx): LeftBranch = parentRef()
    def parent_=(node: LeftBranch)(implicit tx: S#Tx): Unit       = parentRef() = node

    def dispose()(implicit tx: S#Tx): Unit = {
      id        .dispose()
      parentRef .dispose()
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).dispose()
        i += 1
      }
      nextRef.dispose()
    }

    def write(out: DataOutput): Unit = {
      out.writeByte(3)
      id.write(out)
      parentRef.write(out)
      space.hyperCubeSerializer.write(hyperCube, out)
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).write(out)
        i += 1
      }
      nextRef.write(out)
    }

    private def remove()(implicit tx: S#Tx): Unit = dispose()

    // make sure the node is not becoming uninteresting, in which case
    // we need to merge upwards
    protected def leafRemoved()(implicit tx: S#Tx): Unit = {
      val sz = children.length
      @tailrec def removeIfLonely(i: Int): Unit =
        if (i < sz) child(i) match {
          case lonely: LeftNonEmptyChild =>
            @tailrec def isLonely(j: Int): Boolean = {
              j == sz || (child(j) match {
                case _: LeftNonEmptyChild => false
                case _ => isLonely(j + 1)
              })
            }
            if (isLonely(i + 1)) {
              val p     = parent
              val myIdx = p.hyperCube.indexOf(hyperCube)
              p.updateChild(myIdx, lonely)
              if (lonely.parent == this) lonely.updateParentLeft(p)
              remove() // dispose() // removeAndDispose()
            }

          case _ => removeIfLonely(i + 1)
        }

      removeIfLonely(0)
    }
  }

  /*
    * Serialization-id: 4
    */
  private def readRightTopBranch(in: DataInput, access: S#Acc, id: S#ID)(implicit tx: S#Tx): RightTopBranch = {
    val prev  = TopBranchSerializer.read(in, access)
    val sz    = numOrthants
    val ch    = tx.newVarArray[RightChildOption](sz)
    var i = 0
    while (i < sz) {
      ch(i) = tx.readVar[RightChildOption](id, in)
      i += 1
    }
    val nextRef = tx.readVar[NextOption](id, in)(RightOptionReader)
    new RightTopBranch(id, prev, ch, nextRef)
  }

  protected final class RightTopBranch(val id: S#ID, val prev: TopBranch,
                                       protected val children: Array[S#Var[RightChildOption]],
                                       protected val nextRef: S#Var[NextOption])
    extends RightBranch with TopBranch {

    protected def nodeName = "RightTop"

    //      def hyperCube = impl.hyperCube

    private def remove()(implicit tx: S#Tx): Unit = {
      // first unlink
      assert(lastTreeImpl == this)
      lastTreeImpl  = prev
      prev.next     = EmptyValue
      dispose()
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id.dispose()
      //         // first unlink
      //         assert( lastTreeImpl == this )
      //         lastTreeImpl= prev
      //         prev.next   = EmptyValue

      // then dispose refs
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).dispose()
        i += 1
      }
      nextRef.dispose()
    }

    def write(out: DataOutput): Unit = {
      out.writeByte(4)
      id.write(out)
      // no need to write the hypercube!
      prev.write(out)
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).write(out)
        i += 1
      }
      nextRef.write(out)
    }

    // remove this node if it empty now and right-node tree
    protected def leafRemoved()(implicit tx: S#Tx): Unit = {
      if (next != EmptyValue) return

      val sz = children.length
      var i = 0
      while (i < sz) {
        val c = child(i)
        if (c != EmptyValue) return // node not empty, abort the check
        i += 1
      }

      // ok, we are the right most tree and the node is empty...
      remove()
    }

    //      private def removeAndDispose()( implicit tx: S#Tx ) {
    //         assert( lastTreeImpl == this )
    //         lastTreeImpl= prev
    //         prev.next   = EmptyValue
    //         dispose()
    //      }
  }

  /*
    * Serialization-id: 5
    */
  private def readRightChildBranch(in: DataInput, access: S#Acc, id: S#ID)(implicit tx: S#Tx): RightChildBranch = {
    val parentRef = tx.readVar[RightBranch](id, in)
    val prev      = BranchSerializer.read(in, access)
    val hyperCube = space.hyperCubeSerializer.read(in)
    val sz        = numOrthants
    val ch        = tx.newVarArray[RightChildOption](sz)
    var i = 0
    while (i < sz) {
      ch(i) = tx.readVar[RightChildOption](id, in)
      i += 1
    }
    val nextRef = tx.readVar[NextOption](id, in)(RightOptionReader)
    new RightChildBranch(id, parentRef, prev, hyperCube, ch, nextRef)
  }

  private final class RightChildBranch(val id: S#ID, parentRef: S#Var[RightBranch],
                                       val prev: BranchLike, val hyperCube: D#HyperCube,
                                       protected val children: Array[S#Var[RightChildOption]],
                                       protected val nextRef: S#Var[NextOption])
    extends RightBranch with ChildBranch with RightNonEmptyChild {

    protected def nodeName = "RightInner"

    def updateParentRight(p: RightBranch)(implicit tx: S#Tx): Unit = parent = p

    private def remove()(implicit tx: S#Tx): Unit = {
      // first unlink
      prev.next = EmptyValue
      dispose()
    }

    def dispose()(implicit tx: S#Tx): Unit = {
      id.dispose()
      //         // first unlink
      //         prev.next = EmptyValue

      // then dispose refs
      parentRef.dispose()
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).dispose()
        i += 1
      }
      nextRef.dispose()
    }

    def write(out: DataOutput): Unit = {
      out.writeByte(5)
      id.write(out)
      parentRef.write(out)
      prev.write(out)
      space.hyperCubeSerializer.write(hyperCube, out)
      var i = 0
      val sz = children.length
      while (i < sz) {
        children(i).write(out)
        i += 1
      }
      nextRef.write(out)
    }

    //      private def removeAndDispose()( implicit tx: S#Tx ): Unit = {
    //         prev.next = EmptyValue
    //         dispose()
    //      }

    def parent                     (implicit tx: S#Tx): RightBranch = parentRef()
    def parent_=(node: RightBranch)(implicit tx: S#Tx): Unit        = parentRef() = node

    // make sure the node is not becoming uninteresting, in which case
    // we need to merge upwards
    protected def leafRemoved()(implicit tx: S#Tx): Unit = {
      val sz = children.length
      @tailrec def removeIfLonely(i: Int): Unit =
        if (i < sz) child(i) match {
          case lonely: RightNonEmptyChild =>
            @tailrec def isLonely(j: Int): Boolean = {
              j == sz || (child(j) match {
                case _: RightNonEmptyChild  => false
                case _                      => isLonely(j + 1)
              })
            }
            if (isLonely(i + 1)) {
              val p       = parent
              val myIdx   = p.hyperCube.indexOf(hyperCube)
              p.updateChild(myIdx, lonely)
              if (lonely.parent == this) lonely.updateParentRight(p)
              remove()
            }

          case _ => removeIfLonely(i + 1)
        }

      removeIfLonely(0)
    }
  }

  def debugPrint()(implicit tx: S#Tx): String = {
    val baos  = new ByteArrayOutputStream()
    val ps    = new PrintStream(baos)
    import ps._

    println(s"Debug print for $this")
    println("Skip list of leaves:")
    println(skipList.debugPrint())
    println("Octree structure:")

    def dumpTree(b: BranchLike, indent: Int): Unit = {
      val iStr = " " * indent
      b match {
        case lb: LeftBranch =>
          println(s"${iStr}LeftBranch${lb.id} with ${b.hyperCube}")
        case _ =>
          println(s"${iStr}RightBranch${b.id} with ${b.hyperCube}")
      }
      for(i <- 0 until numOrthants) {
        print(s"$iStr  Child #${i+1} = ")
        b.child(i) match {
          case cb: BranchLike =>
            println("Branch:")
            dumpTree(cb, indent + 4)
          case l: LeafImpl =>
            println(s"Leaf${l.id} ${l.value}")
          case empty => println(empty)
        }
      }
    }

    def dumpTrees(b: BranchLike, level: Int): Unit = {
      println(s"\n---level $level----")
      dumpTree(b, 0)
      b.nextOption.foreach(b => dumpTrees(b, level + 1))
    }

    dumpTrees(head, 0)
    ps.close()
    new String(baos.toByteArray, "UTF-8")
  }
}