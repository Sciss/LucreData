/*
 *  Ancestor.scala
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

import geom.{DistanceMeasure, IntSpace, IntDistanceMeasure3D, IntPoint3D, IntCube}
import stm.{Disposable, Sys}
import geom.IntSpace.ThreeDim
import serial.{DataInput, DataOutput, Serializer, Writable}

//import stm.{SpecGroup => ialized}

object Ancestor {
  private final val SER_VERSION = 65

  private[Ancestor] val cube = IntCube(0x40000000, 0x40000000, 0x40000000, 0x40000000)

  private type TreeOrder[S <: Sys[S]] = TotalOrder.Set.Entry[S]

  object Vertex {
    private[Ancestor] implicit def toPoint[S <: Sys[S], Version](v: Vertex[S, Version], tx: S#Tx): IntPoint3D =
      IntPoint3D(v.pre.tag(tx), v.post.tag(tx), v.versionInt)
  }

  sealed trait Vertex[S <: Sys[S], Version] extends Writable with Disposable[S#Tx] {

    // ---- abstract ----

    def version: Version

    private[Ancestor] def pre:  TreeOrder[S]
    private[Ancestor] def post: TreeOrder[S]
    private[Ancestor] def tree: Tree[S, Version]

    // ---- implementation ----

    final def isAncestorOf(that: Vertex[S, Version])(implicit tx: S#Tx): Boolean =
      versionInt <= that.versionInt &&
      pre .compare(that.pre ) <= 0 &&
      post.compare(that.post) >= 0

    final def versionInt: Int = tree.intView(version)

    final def write(out: DataOutput): Unit = {
      tree.versionSerializer.write(version, out)
      pre .write(out)
      post.write(out)
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      pre .dispose()
      post.dispose()
    }

    override def toString = s"Vertex($version)"
  }

  implicit def treeSerializer[S <: Sys[S], Version](
    implicit versionSerializer: Serializer[S#Tx, S#Acc, Version],
    intView: Version => Int): Serializer[S#Tx, S#Acc, Tree[S, Version]] = new TreeSer[S, Version]

  def newTree[S <: Sys[S], Version](rootVersion: Version)(
    implicit tx: S#Tx, versionSerializer: Serializer[S#Tx, S#Acc, Version],
    intView: Version => Int): Tree[S, Version] = {

    new TreeNew[S, Version](rootVersion, tx)
  }

  def readTree[S <: Sys[S], Version](in: DataInput, access: S#Acc)(
    implicit tx: S#Tx, versionSerializer: Serializer[S#Tx, S#Acc, Version],
    intView: Version => Int): Tree[S, Version] = {

    new TreeRead[S, Version](in, access, tx)
  }

  private final class TreeSer[S <: Sys[S], Version](implicit versionSerializer: Serializer[S#Tx, S#Acc, Version],
                                                    versionView: Version => Int)
    extends Serializer[S#Tx, S#Acc, Tree[S, Version]] {

    def write(t: Tree[S, Version], out: DataOutput): Unit = t.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Tree[S, Version] = 
      new TreeRead[S, Version](in, access, tx)

    override def toString = "Ancestor.treeSerializer"
  }

  private sealed trait TreeImpl[S <: Sys[S], Version] extends Tree[S, Version] {
    me =>

    // ---- abstract ----

    protected def order: TotalOrder.Set[S]

    // ---- implementation ----

    override def toString = "Ancestor.Tree(root=" + root + ")"

    implicit protected object VertexSerializer extends Serializer[S#Tx, S#Acc, K] {
      def write(v: K, out: DataOutput): Unit = v.write(out)

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): K = new K {
        def tree = me

        val version = versionSerializer.read(in, access)
        val pre     = order.readEntry(in, access)
        val post    = order.readEntry(in, access)
      }
    }

    final def write(out: DataOutput): Unit = {
      out.writeByte(SER_VERSION)
      order.write(out)
      root.write(out)
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      order.dispose()
      root.dispose()
    }

    final def vertexSerializer: Serializer[S#Tx, S#Acc, K] = VertexSerializer

    final def insertChild(parent: K, newChild: Version)(implicit tx: S#Tx): K = new K {
      def tree = me

      val version = newChild
      val pre     = parent.pre.append()
      val post    = pre.append()
    }

    final def insertRetroChild(parent: K, newChild: Version)(implicit tx: S#Tx): K = new K {
      def tree = me

      val version = newChild
      val pre     = parent.pre.append()
      val post    = parent.post.prepend()

      override def toString = super.toString + "@r-ch"
    }

    final def insertRetroParent(child: K, newParent: Version)(implicit tx: S#Tx): K = {
      require(child != root)
      new K {
        def tree = me

        val version = newParent
        val pre     = child.pre.prepend()
        val post    = child.post.append()

        override def toString = super.toString + "@r-par"
      }
    }
  }

  private final class TreeNew[S <: Sys[S], Version](rootVersion: Version, tx0: S#Tx)(
    implicit val versionSerializer: Serializer[S#Tx, S#Acc, Version], val intView: Version => Int)
    extends TreeImpl[S, Version] {
    me =>

    protected val order = TotalOrder.Set.empty[S](0)(tx0)
    val root: K = new K {
      def tree: Tree[S, Version] = me

      def version = rootVersion
      val pre     = order.root
      val post    = pre.appendMax()(tx0)
    }
  }

  private final class TreeRead[S <: Sys[S], Version](in: DataInput, access: S#Acc, tx0: S#Tx)(
    implicit val versionSerializer: Serializer[S#Tx, S#Acc, Version], val intView: Version => Int)
    extends TreeImpl[S, Version] {

    {
      val serVer = in.readByte()
      require(serVer == SER_VERSION, "Incompatible serialized version (found " + serVer +
        ", required " + SER_VERSION + ").")
    }

    protected val order = TotalOrder.Set.read[S](in, access)(tx0)
    val root = VertexSerializer.read(in, access)(tx0)
  }

  sealed trait Tree[S <: Sys[S], Version] extends Writable with Disposable[S#Tx] {
    protected type K = Vertex[S, Version]

    private[Ancestor] def versionSerializer: Serializer[S#Tx, S#Acc, Version]
    private[Ancestor] def intView: Version => Int

    def vertexSerializer: Serializer[S#Tx, S#Acc, K]

    def root: K

    def insertChild      (parent: K, newChild : Version)(implicit tx: S#Tx): K
    def insertRetroChild (parent: K, newChild : Version)(implicit tx: S#Tx): K
    def insertRetroParent(child : K, newParent: Version)(implicit tx: S#Tx): K
  }

  private type MarkOrder[S <: Sys[S], Version, A] = TotalOrder.Map.Entry[S, Mark[S, Version, A]]

  private final val chebyMetric = IntDistanceMeasure3D.chebyshevXY
  // left-bottom-front
  // = left in pre-order list, right in post-order list, smaller in version
  private final val metric = chebyMetric.orthant(2)

  private final class FilterMetric(pred: Int => Boolean) extends IntDistanceMeasure3D.LongImpl {
    import IntSpace.ThreeDim.{PointLike, HyperCube}

    override def toString = "Ancestor.FilterMetric@" + pred.hashCode.toHexString

    def distance(a: PointLike, b: PointLike): Long = {
      if (b.x <= a.x && b.y >= a.y && pred(b.z)) {
        chebyMetric.distance(a, b)
      } else maxValue
    }

    def minDistance(p: PointLike, q: HyperCube): Long = {
      val qe = q.extent
      val qem1 = qe - 1

      if (q.cx - qe <= p.x && q.cy + qem1 >= p.y && pred(q.cz - qe)) {
        chebyMetric.minDistance(p, q)
      } else maxValue
    }

    def maxDistance(p: PointLike, q: HyperCube): Long = {
      val qe = q.extent
      val qem1 = qe - 1

      if (q.cx + qem1 <= p.x && q.cy - qe >= p.y && pred(q.cz + qem1)) {
        chebyMetric.maxDistance(p, q)
      } else maxValue
    }
  }

  private sealed trait Mark[S <: Sys[S], Version, /* @spec(ValueSpec) */ A] extends Writable {

    // ---- abstract ----

    def fullVertex: Vertex[S, Version]

    def pre:  MarkOrder[S, Version, A]
    def post: MarkOrder[S, Version, A]

    def value: A

    def map: MapImpl[S, Version, A]

    // ---- implementation ----

    final def toPoint(implicit tx: S#Tx): IntPoint3D = IntPoint3D(pre.tag, post.tag, fullVertex.versionInt)

    final def write(out: DataOutput): Unit = {
      fullVertex.write(out)
      pre       .write(out)
      post      .write(out)
      map.valueSerializer.write(value, out)
    }

    final def removeAndDispose()(implicit tx: S#Tx): Unit = {
      map.skip.remove(this)
      pre .removeAndDispose()
      post.removeAndDispose()
    }

    override def toString = "Mark(" + fullVertex.version + " -> " + value + ")"
  }

  def newMap[S <: Sys[S], Version, /* @spec(ValueSpec) */ A](full: Tree[S, Version], rootVertex: Vertex[S, Version],
    rootValue: A)(implicit tx: S#Tx, valueSerializer: Serializer[S#Tx, S#Acc, A]): Map[S, Version, A] = {

    new MapNew[S, Version, A](full, rootVertex, rootValue, tx, valueSerializer)
  }

  def readMap[S <: Sys[S], Version, /* @spec(ValueSpec) */ A](in: DataInput, access: S#Acc, full: Tree[S, Version])(
    implicit tx: S#Tx, valueSerializer: Serializer[S#Tx, S#Acc, A]): Map[S, Version, A] = {

    new MapRead[S, Version, A](full, in, access, tx, valueSerializer)
  }

  /*
   * The result of isomorph search (mapping full tree vertex coordinates to marked tree coordinates).
   *
   * @param pre     the nearest mark in the pre-order traversal
   * @param preCmp  the relation between the query (full) vertex and the found mark vertex.
   *                `-1` indicates that the full vertex lies left of the found mark vertex in the pre-order list,
   *                `0` indicates that both refer to the same version, and `1` indicates that the full vertex lies
   *                right to the mark vertex in the pre-order list
   * @param post    the nearest mark in the post-order traversal
   * @param postCmp the relation between the query (full) vertex and the found mark vertex.
   *                `-1` indicates that the full vertex lies left of the found mark vertex in the post-order list,
   *                `0` indicates that both refer to the same version, and `1` indicates that the full vertex lies
   *                right to the mark vertex in the post-order list
   */
  private final class IsoResult[S <: Sys[S], Version, /* @spec(ValueSpec) */ A](val pre:  Mark[S, Version, A],
                                                                          val preCmp: Int,
                                                                          val post: Mark[S, Version, A],
                                                                          val postCmp: Int) {

    override def toString = "Iso(pre " + (if (preCmp < 0) "< " else if (preCmp > 0) "> " else "== ") + pre + "," +
      "post " + (if (postCmp < 0) "< " else if (postCmp > 0) "> " else "== ") + post + ")"
  }

  private sealed trait MapImpl[S <: Sys[S], Version, /* @spec(ValueSpec) */ A]
    extends Map[S, Version, A] with TotalOrder.Map.RelabelObserver[S#Tx, Mark[S, Version, A]] {
    me =>

    final type M = Mark[S, Version, A]

    // ---- abstract ----

    protected def preOrder:  TotalOrder.Map[S, M]
    protected def postOrder: TotalOrder.Map[S, M]
    protected def preList:   SkipList  .Set[S, M]
    protected def postList:  SkipList  .Set[S, M]

    private[Ancestor] def skip: SkipOctree[S, IntSpace.ThreeDim, M]

    // ---- implementation ----

    override def toString = "Ancestor.Map(tree=" + full + ")"

    final protected def preOrdering: Ordering[S#Tx, M] = new Ordering[S#Tx, M] {
      def compare(a: M, b: M)(implicit tx: S#Tx): Int = a.pre compare b.pre
    }

    final protected def postOrdering: Ordering[S#Tx, M] = new Ordering[S#Tx, M] {
      def compare(a: M, b: M)(implicit tx: S#Tx): Int = a.post compare b.post
    }

    protected implicit object markSerializer extends Serializer[S#Tx, S#Acc, M] {
      def write(v: M, out: DataOutput): Unit = v.write(out)

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): M = new M {
        def map         = me

        val fullVertex  = full.vertexSerializer.read(in, access)
        val pre         = preOrder        .readEntry(in, access)
        val post        = postOrder       .readEntry(in, access)
        val value       = valueSerializer      .read(in, access)
      }
    }

    final def write(out: DataOutput): Unit = {
      out.writeByte(SER_VERSION)
      // note: we ask for the full tree through the serializer constructor,
      // thus we omit writing it out ourselves
      preOrder .write(out)
      postOrder.write(out)
      preList  .write(out)
      postList .write(out)
      skip     .write(out)
      // root.write( out )
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      preOrder.dispose()
      postOrder.dispose()
      preList.dispose()
      postList.dispose()
      skip.dispose()
    }

    final def add(entry: (K, A))(implicit tx: S#Tx): Boolean = {
      val mv = wrap(entry)
      preList  += mv
      postList += mv
      skip.add(mv)
    }

    final def +=( entry: (K, A) )( implicit tx: S#Tx ) : this.type = {
      add(entry)
      this
    }

    private def query(vertex: K)(implicit tx: S#Tx): IsoResult[S, Version, A] = {
      val cfPre = vertex.pre
      val (cmPreN, cmPreCmp) = preList.isomorphicQuery(new Ordered[S#Tx, M] {
        def compare(that: M)(implicit tx: S#Tx): Int = {
          cfPre.compare(that.fullVertex.pre)
        }
      })
      if (cmPreCmp == 0) return new IsoResult(cmPreN, 0, cmPreN, 0)

      val cfPost = vertex.post
      val (cmPostN, cmPostCmp) = postList.isomorphicQuery(new Ordered[S#Tx, M] {
        def compare(that: M)(implicit tx: S#Tx): Int = {
          cfPost.compare(that.fullVertex.post)
        }
      })
      new IsoResult(cmPreN, cmPreCmp, cmPostN, cmPostCmp)
    }

    private def wrap(entry: (K, A))(implicit tx: S#Tx): M = {
      val vertex  = entry._1
      val iso     = query(vertex)
      new M {
        def map         = me
        val fullVertex  = vertex
        val value       = entry._2
        val pre         = preOrder.insert()
        val post        = postOrder.insert()
        if (iso.preCmp <= 0) {
          preOrder.placeBefore(iso.pre, this)
        } else {
          preOrder.placeAfter(iso.pre, this)
        }
        if (iso.postCmp <= 0) {
          postOrder.placeBefore(iso.post, this)
        } else {
          postOrder.placeAfter(iso.post, this)
        }
      }
    }

    final def remove(vertex: K)(implicit tx: S#Tx): Boolean = {
      val iso = query(vertex)
      iso.preCmp == 0 /* && (iso.postCmp == 0) */ && {
        // assert(iso.postCmp == 0)
        iso.pre.removeAndDispose() // iso.pre is a VM!
        true
      }
    }

    final def -=(vertex: K)(implicit tx: S#Tx): this.type = {
      remove(vertex)
      this
    }

    final def get(vertex: K)(implicit tx: S#Tx): Option[A] = {
      val iso = query(vertex)
      if (iso.preCmp == 0) {
        // assert(iso.postCmp == 0)
        Some(iso.pre.value)
      } else None
    }

    // XXX TODO: DRY
    final def nearest(vertex: K)(implicit tx: S#Tx): (K, A) = {
      val iso = query(vertex)
      if (iso.preCmp == 0) {
        // assert(iso.postCmp == 0)
        (vertex, iso.pre.value)
      } else {
        val preTag  = iso.pre.pre.tag
        val postTag = iso.post.post.tag
        val x       = if (iso.preCmp < 0) preTag - 1 else preTag
        val y       = if (iso.postCmp > 0) postTag + 1 else postTag
        val nn      = skip.nearestNeighbor(IntPoint3D(x, y, vertex.versionInt), metric)
        (nn.fullVertex, nn.value)
      }
    }

    final def nearestOption(vertex: K)(implicit tx: S#Tx): Option[(K, A)] = {
      val iso = query(vertex)
      if (iso.preCmp == 0) {
        // assert(iso.postCmp == 0)
        Some((vertex, iso.pre.value))
      } else {
        nearestWithMetric(vertex, iso, metric)
      }
    }

    final def nearestWithFilter(vertex: K)(p: Int => Boolean)(implicit tx: S#Tx): Option[(K, A)] = {
      val iso = query(vertex)
      nearestWithMetric(vertex, iso, new FilterMetric(p))
    }

    private def nearestWithMetric(vertex: K, iso: IsoResult[S, Version, A],
                                  metric: DistanceMeasure[Long, ThreeDim])
                                 (implicit tx: S#Tx): Option[(K, A)] = {
      val preTag  = iso.pre.pre.tag
      val postTag = iso.post.post.tag
      val x       = if (iso.preCmp  < 0) preTag  - 1 else preTag
      val y       = if (iso.postCmp > 0) postTag + 1 else postTag
      val nnOpt   = skip.nearestNeighborOption(IntPoint3D(x, y, vertex.versionInt), metric)
      nnOpt.map { nn => (nn.fullVertex, nn.value) }
    }

    // ---- RelabelObserver ----
    final def beforeRelabeling(iter: Iterator[S#Tx, M])(implicit tx: S#Tx): Unit =
      iter.foreach(skip -= _)

    final def afterRelabeling(iter: Iterator[S#Tx, M])(implicit tx: S#Tx): Unit =
      iter.foreach(skip += _)

    final def debugPrint(implicit tx: S#Tx): String = {
      val s = skip.toList.map { m =>
        val v = m.fullVertex
        s"{version = ${v.versionInt}, value = ${m.value}, pre = ${v.pre.tag}, post = ${v.post.tag}}"
      }
      s.mkString("[", ", ", "]")
    }
  }

  private final class MapNew[S <: Sys[S], Version, A](val full: Tree[S, Version],
                                                      rootVertex: Vertex[S, Version],
                                                      rootValue: A, tx0: S#Tx,
                                                      val valueSerializer: Serializer[S#Tx, S#Acc, A])
    extends MapImpl[S, Version, A] {
    me =>

    protected val preOrder: TotalOrder.Map[S, M] =
      TotalOrder.Map.empty[S, M](me, _.pre)(tx0, markSerializer)

    protected val postOrder: TotalOrder.Map[S, M] =
      TotalOrder.Map.empty[S, M](me, _.post, rootTag = Int.MaxValue)(tx0, markSerializer)

    private[Ancestor] val skip: SkipOctree[S, IntSpace.ThreeDim, M] = {
      val pointView = (p: M, tx: S#Tx) => p.toPoint(tx)
      SkipOctree.empty[S, IntSpace.ThreeDim, M](cube)(tx0, pointView, IntSpace.ThreeDim, markSerializer)
    }

    protected val root: M = {
      val res: M = new M {
        def map         = me
        val fullVertex  = rootVertex
        def pre         = preOrder.root
        def post        = postOrder.root
        val value       = rootValue

        override def toString = "Root(" + value + ")"
      }
      (skip += res)(tx0)
      res
    }

    protected val preList: SkipList.Set[S, M] = {
      implicit val ord = preOrdering
      implicit val tx = tx0
      val res = SkipList.Set.empty[S, M]
      res.add(root)
      res
    }

    protected val postList: SkipList.Set[S, M] = {
      implicit val ord = postOrdering
      implicit val tx = tx0
      val res = SkipList.Set.empty[S, M]
      res.add(root)
      res
    }
  }

  private final class MapRead[S <: Sys[S], Version, A](val full: Tree[S, Version], in: DataInput,
                                                       access: S#Acc, tx0: S#Tx,
                                                       val valueSerializer: Serializer[S#Tx, S#Acc, A])
    extends MapImpl[S, Version, A] {
    me =>

    {
      val serVer = in.readByte()
      require(serVer == SER_VERSION, "Incompatible serialized version (found " + serVer +
        ", required " + SER_VERSION + ").")
    }

    protected val preOrder: TotalOrder.Map[S, M] =
      TotalOrder.Map.read[S, M](in, access, me, _.pre)(tx0, markSerializer)

    protected val postOrder: TotalOrder.Map[S, M] =
      TotalOrder.Map.read[S, M](in, access, me, _.post)(tx0, markSerializer)

    protected val preList: SkipList.Set[S, M] = {
      implicit val ord = preOrdering
      implicit val tx = tx0
      SkipList.Set.read[S, M](in, access)
    }

    protected val postList: SkipList.Set[S, M] = {
      implicit val ord = postOrdering
      implicit val tx = tx0
      SkipList.Set.read[S, M](in, access)
    }

    private[Ancestor] val skip: SkipOctree[S, IntSpace.ThreeDim, M] = {
      val pointView = (p: M, tx: S#Tx) => p.toPoint(tx)
      SkipOctree.read[S, IntSpace.ThreeDim, M](in, access)(tx0, pointView, IntSpace.ThreeDim, markSerializer)
    }
  }

  sealed trait Map[S <: Sys[S], Version, /* @spec(ValueSpec) */ A] extends Writable with Disposable[S#Tx] {
    type K = Vertex[S, Version]

    def full: Tree[S, Version]

    def debugPrint(implicit tx: S#Tx): String

    /**
     * Marks a given key with a given value.
     *
     * @param   entry the key-value pair (where the key is a vertex in the full tree)
     * @return  `true` if the mark is new, `false` if there had been a mark for the given vertex.
     */
    def add(entry: (K, A))(implicit tx: S#Tx): Boolean

    def +=(entry: (K, A))(implicit tx: S#Tx): this.type

    def remove(vertex: K)(implicit tx: S#Tx): Boolean

    def -=(vertex: K)(implicit tx: S#Tx): this.type

    /**
     * Queries for a mark at a given version vertex. Unlike `nearest`, this does
     * not search in the map, but merely tests if the given vertex has been
     * marked or not.
     *
     * @param   vertex  the version vertex to look up
     * @return  the value associated with that vertex, or `None` if the vertex is unmarked.
     */
    def get(vertex: K)(implicit tx: S#Tx): Option[A]

    /**
     * Finds the nearest marked ancestor of a given version key.
     * Since the map is constructed with a defined root value, this method is
     * guaranteed to succeed&mdash;if there are no other marks in the map,
     * it will return the root value (unless the `version` argument is
     * illegal, i.e. has a version lower than the root vertex' version).
     *
     * @param   vertex  the key to look for. The algorithm searches for
     *                  the nearest ancestor in the marked map with a version less than or
     *                  equal to the given version
     * @return  a pair consisting of the tree vertex found and the value with which
     *          it has been marked. If the query `version` vertex was marked, it will be
     *          that vertex which is returned, and not an ancestor.
     */
    def nearest(vertex: K)(implicit tx: S#Tx): (K, A)

    def nearestOption(vertex: K)(implicit tx: S#Tx): Option[(K, A)]

    /**
     * Searches for the nearest marked ancestor, where version control is handed over
     * to a custom predicate function. I.e., while `nearestOption` will test for a
     * version that is less than or equal to the query version, the behaviour may be
     * customised here. The predicate function is called with the `versionInt` field
     * of the vertices, e.g. using the tree's `intView`.
     *
     * Only those vertices are considered for which the predicate is `true`.
     *
     * '''Note:''' This currently only works correctly if the predicate tests for
     * version anteriority!
     *
     * @param vertex  the query vertex
     * @param p       the predicate function for the integer view of the vertex versions
     */
    def nearestWithFilter(vertex: K)(p: Int => Boolean)(implicit tx: S#Tx): Option[(K, A)]

    def valueSerializer: Serializer[S#Tx, S#Acc, A]
  }
}