/*
 *  Ancestor.scala
 *  (LucreData)
 *
 *  Copyright (c) 2011-2012 Hanns Holger Rutz. All rights reserved.
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

package de.sciss.collection.txn

import de.sciss.collection.geom.{DistanceMeasure3D, Point3D, Cube, Space}
import de.sciss.lucre.{DataOutput, DataInput}
import de.sciss.lucre.stm.{Disposable, TxnSerializer, Writer, Sys}

object Ancestor {
   private val SER_VERSION = 0

   private[Ancestor] val cube = Cube( 0x40000000, 0x40000000, 0x40000000, 0x40000000 )

   private type TreePreOrder[  S <: Sys[ S ]] = TotalOrder.Set.Entry[ S ]
   private type TreePostOrder[ S <: Sys[ S ]] = TotalOrder.Set.Entry[ S ]

   object Vertex {
      private[Ancestor] implicit def toPoint[ S <: Sys[ S ], Version ]( v: Vertex[ S, Version ], tx: S#Tx ) : Point3D =
         new Point3D( v.preHead.tag( tx ), v.post.tag( tx ), v.versionInt )
   }
   sealed trait Vertex[ S <: Sys[ S ], Version ] extends Writer with Disposable[ S#Tx ] {

      // ---- abstract ----

      def version: Version
      private[Ancestor] def preHead: TreePreOrder[ S ]
      private[Ancestor] def preTail: TreePreOrder[ S ]
      private[Ancestor] def post:    TreePostOrder[ S ]

      private[Ancestor] def tree: Tree[ S, Version ]

      // ---- implementation ----

      final def versionInt: Int = tree.intView( version )

      final def write( out: DataOutput ) {
         tree.versionSerializer.write( version, out )
         preHead.write( out )
         preTail.write( out )
         post.write( out )
      }

      final def dispose()( implicit tx: S#Tx ) {
         preHead.dispose()
         preTail.dispose()
         post.dispose()
      }

      override def toString = "Vertex(" + version + ")"
   }

   implicit def treeSerializer[ S <: Sys[ S ], Version ](
      implicit versionSerializer: TxnSerializer[ S#Tx, S#Acc, Version ],
      intView: Version => Int) : TxnSerializer[ S#Tx, S#Acc, Tree[ S, Version ]] = {

      new TreeSer[ S, Version ]
   }

   def newTree[ S <: Sys[ S ], Version ]( rootVersion: Version )(
      implicit tx: S#Tx, versionSerializer: TxnSerializer[ S#Tx, S#Acc, Version ],
      intView: Version => Int ) : Tree[ S, Version ] = {

      new TreeNew[ S, Version ]( rootVersion, tx )
   }

   def readTree[ S <: Sys[ S ], Version ]( in: DataInput, access: S#Acc )(
      implicit tx: S#Tx, versionSerializer: TxnSerializer[ S#Tx, S#Acc, Version ],
      intView: Version => Int ) : Tree[ S, Version ] = {

      new TreeRead[ S, Version ]( in, access, tx )
   }

   private final class TreeSer[ S <: Sys[ S ], Version ]( implicit versionSerializer: TxnSerializer[ S#Tx, S#Acc, Version ],
                                                          versionView: Version => Int )
   extends TxnSerializer[ S#Tx, S#Acc, Tree[ S, Version ]] {
      def write( t: Tree[ S, Version ], out: DataOutput ) { t.write( out )}

      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Tree[ S, Version ] =
         new TreeRead[ S, Version ]( in, access, tx )
   }

   private sealed trait TreeImpl[ S <: Sys[ S ], Version ] extends Tree[ S, Version ] {
      me =>

      // ---- abstract ----

      protected def preOrder  : TotalOrder.Set[ S ]
      protected def postOrder : TotalOrder.Set[ S ]
      // def root : K

      // ---- implementation ----

      implicit protected object VertexSerializer extends TxnSerializer[ S#Tx, S#Acc, K ] {
         def write( v: K, out: DataOutput ) { v.write( out )}

         def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : K = new K {
            def tree    = me
            val version = versionSerializer.read( in, access )
            val preHead = preOrder.readEntry(   in, access )
            val preTail = preOrder.readEntry(   in, access )
            val post    = postOrder.readEntry(  in, access )
         }
      }

      final def write( out: DataOutput ) {
         out.writeUnsignedByte( SER_VERSION )
         preOrder.write( out )
         postOrder.write( out )
         root.write( out )
      }

      final def dispose()( implicit tx: S#Tx ) {
         preOrder.dispose()
         postOrder.dispose()
         root.dispose()
      }

      final def vertexSerializer : TxnSerializer[ S#Tx, S#Acc, K ] = VertexSerializer

      final def insertChild( parent: K, newChild: Version )( implicit tx: S#Tx ) : K = {
         val v = new K {
            def tree    = me
            val version = newChild
            val preHead = parent.preTail.prepend() // preOrder.insert()
            val preTail = preHead.append() // preOrder.insert()
            val post    = parent.post.prepend() // postOrder.insert()
         }
         v
      }

      final def insertRetroChild( parent: K, newChild: Version )( implicit tx: S#Tx ) : K = {
         val v = new K {
            def tree    = me
            val version = newChild
            val preHead = parent.preHead.append()  // preOrder.insert()
            val preTail = parent.preTail.prepend() // preOrder.insert()
            val post    = parent.post.prepend()    // postOrder.insert()
            override def toString = super.toString + "@r-ch"
         }
         v
      }

      final def insertRetroParent( child: K, newParent: Version )( implicit tx: S#Tx ) : K = {
         require( child != root )
         val v = new K {
            def tree    = me
            val version = newParent
            val preHead = child.preHead.prepend()  // preOrder.insert()
            val preTail = child.preTail.append()   // preOrder.insert()
            val post    = child.post.append()      // postOrder.insert()
            override def toString = super.toString + "@r-par"
         }
         v
      }
   }

   private final class TreeNew[ S <: Sys[ S ], Version ]( rootVersion: Version, tx0: S#Tx )(
      implicit val versionSerializer: TxnSerializer[ S#Tx, S#Acc, Version ], val intView: Version => Int )
   extends TreeImpl[ S, Version ] {
      me =>

      protected val preOrder      = TotalOrder.Set.empty[ S ]( 0 )( tx0 )
      protected val postOrder     = TotalOrder.Set.empty[ S ]( Int.MaxValue )( tx0 )
      val root: K = new K {
         def tree: Tree[ S, Version ] = me
         def version = rootVersion
         val preHead = preOrder.root
         val preTail = preHead.append()( tx0 ) // preOrder.insert()
         val post    = postOrder.root
      }
   }

   private final class TreeRead[ S <: Sys[ S ], Version ]( in: DataInput, access: S#Acc, tx0: S#Tx  )(
      implicit val versionSerializer: TxnSerializer[ S#Tx, S#Acc, Version ], val intView: Version => Int )
   extends TreeImpl[ S, Version ] {

      {
         val serVer = in.readUnsignedByte()
         require( serVer == SER_VERSION, "Incompatible serialized version (found " + serVer +
            ", required " + SER_VERSION + ")." )
      }

      protected val preOrder  = TotalOrder.Set.read[ S ]( in, access )( tx0 )
      protected val postOrder = TotalOrder.Set.read[ S ]( in, access )( tx0 )
      val root                = VertexSerializer.read( in, access )( tx0 )
   }

   sealed trait Tree[ S <:Sys[ S ], Version ] extends Writer with Disposable[ S#Tx ] {
      protected type K = Vertex[ S, Version ]

      private[Ancestor] def versionSerializer: TxnSerializer[ S#Tx, S#Acc, Version ]
      private[Ancestor] def intView: Version => Int

      def vertexSerializer : TxnSerializer[ S#Tx, S#Acc, K ]

      def root : K

      def insertChild( parent: K, newChild: Version )( implicit tx: S#Tx ) : K

      def insertRetroChild( parent: K, newChild: Version )( implicit tx: S#Tx ) : K

      def insertRetroParent( child: K, newParent: Version )( implicit tx: S#Tx ) : K
   }

   private type MarkOrder[ S <: Sys[ S ], Version, A ] = TotalOrder.Map.Entry[ S, Mark[ S, Version, A ]]

   private val metric = DistanceMeasure3D.chebyshevXY.orthant( 2 )

   private sealed trait Mark[ S <: Sys[ S ], Version, @specialized A ] extends Writer {

      // ---- abstract ----

      def fullVertex: Vertex[ S, Version ]
      def pre:  MarkOrder[ S, Version, A ]
      def post: MarkOrder[ S, Version, A ]
      def value: A

      def map: MapImpl[ S, Version, A ]

      // ---- implementation ----

      final def toPoint( implicit tx: S#Tx ): Point3D = new Point3D( pre.tag, post.tag, fullVertex.versionInt )

      final def write( out: DataOutput ) {
         fullVertex.write( out )
         pre.write( out )
         post.write( out )
         map.valueSerializer.write( value, out )
      }

      final def removeAndDispose()( implicit tx: S#Tx ) {
         map.skip.remove( this )
         pre.removeAndDispose()
         post.removeAndDispose()
      }

      override def toString = "Mark(" + fullVertex.version + " -> " + value + ")"
   }

   def newMap[ S <: Sys[ S ], Version, @specialized A ]( full: Tree[ S, Version ],
                                                         rootVertex: Vertex[ S, Version ], rootValue: A )(
      implicit tx: S#Tx, valueSerializer: TxnSerializer[ S#Tx, S#Acc, A ]) : Map[ S, Version, A ] = {

      new MapNew[ S, Version, A ]( full, rootVertex, rootValue, tx, valueSerializer )
   }

   def readMap[ S <: Sys[ S ], Version, @specialized A ]( in: DataInput, access: S#Acc, full: Tree[ S, Version ])(
      implicit tx: S#Tx, valueSerializer: TxnSerializer[ S#Tx, S#Acc, A ]) : Map[ S, Version, A ] = {

      new MapRead[ S, Version, A ]( full, in, access, tx, valueSerializer )
   }

//   private final class MapSer[ S <: Sys[ S ], Version, A ]( full: Tree[ S, Version ])(
//      implicit valueSerializer: TxnSerializer[ S#Tx, S#Acc, A ])
//   extends TxnSerializer[ S#Tx, S#Acc, Map[ S, Version, A ]] {
//      def write( m: Map[ S, Version, A ], out: DataOutput ) { m.write( out )}
//
//      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Map[ S, Version, A ] =
//         new MapRead[ S, Version, A ]( full, in, access, tx )
//   }

   private final class IsoResult[ S <: Sys[ S ], Version, @specialized A ](
      val pre: Mark[ S, Version, A ], val preCmp: Int, val post: Mark[ S, Version, A ], val postCmp: Int ) {

      override def toString = "Iso(pre "  + (if( preCmp  < 0 ) "< " else if( preCmp  > 0) "> " else "== ") +  pre  + "," +
                                  "post " + (if( postCmp < 0 ) "< " else if( postCmp > 0) "> " else "== ") +  post + ")"
   }

   private sealed trait MapImpl[ S <: Sys[ S ], Version, @specialized A ]
   extends Map[ S, Version, A ] with TotalOrder.Map.RelabelObserver[ S#Tx, Mark[ S, Version, A ]] {
      me =>

      final type M = Mark[ S, Version, A ]

      // ---- abstract ----

      protected def preOrder  : TotalOrder.Map[ S, M ]
      protected def postOrder : TotalOrder.Map[ S, M ]

      protected def preList  : SkipList[ S, M ]
      protected def postList : SkipList[ S, M ]

      private[Ancestor] def skip: SkipOctree[ S, Space.ThreeDim, M ]

      // ---- implementation ----

      final protected def preOrdering : Ordering[ S#Tx, M ] = new Ordering[ S#Tx, M ] {
         def compare( a: M, b: M )( implicit tx: S#Tx ) : Int = a.pre compare b.pre
      }

      final protected def postOrdering : Ordering[ S#Tx, M ] = new Ordering[ S#Tx, M ] {
         def compare( a: M, b: M )( implicit tx: S#Tx ) : Int = a.post compare b.post
      }

      protected implicit object markSerializer extends TxnSerializer[ S#Tx, S#Acc, M ] {
         def write( v: M, out: DataOutput ) { v.write( out )}

         def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : M = new M {
            def map        = me
            val fullVertex = full.vertexSerializer.read( in, access )
            val pre        = preOrder.readEntry( in, access )
            val post       = postOrder.readEntry( in, access )
            val value      = valueSerializer.read( in, access )
         }
      }

      final def write( out: DataOutput ) {
         out.writeUnsignedByte( SER_VERSION )
         // note: we ask for the full tree through the serializer constructor,
         // thus we omit writing it out ourselves
         preOrder.write( out )
         postOrder.write( out )
         preList.write( out )
         postList.write( out )
         skip.write( out )
//         root.write( out )
      }

      final def dispose()( implicit tx: S#Tx ) {
         preOrder.dispose()
         postOrder.dispose()
         preList.dispose()
         postList.dispose()
         skip.dispose()
      }

      final def add( entry: (K, A) )( implicit tx: S#Tx ) : Boolean = {
         val mv    = wrap( entry )
         preList  += mv
         postList += mv
         skip.add( mv )
      }

      final def +=( entry: (K, A) )( implicit tx: S#Tx ) : this.type = {
         add( entry )
         this
      }

      private def query( vertex: K )( implicit tx: S#Tx ) : IsoResult[ S, Version, A ] = {
         val cfPre = vertex.preHead
         val (cmPreN, cmPreCmp) = preList.isomorphicQuery( new Ordered[ S#Tx, M ] {
            def compare( that: M )( implicit tx: S#Tx ) : Int = {
               cfPre.compare( that.fullVertex.preHead )
            }
         })
         val cfPost = vertex.post
         val (cmPostN, cmPostCmp ) = postList.isomorphicQuery( new Ordered[ S#Tx, M ] {
            def compare( that: M )( implicit tx: S#Tx ) : Int = {
               cfPost.compare( that.fullVertex.post )
            }
         })
         new IsoResult[ S, Version, A ]( cmPreN, cmPreCmp, cmPostN, cmPostCmp )
      }

      private def wrap( entry: (K, A) )( implicit tx: S#Tx ) : M = {
         val vertex = entry._1
         val iso = query( vertex )
         new M {
            def map        = me
            val fullVertex = vertex
            val value      = entry._2
            val pre        = preOrder.insert()
            val post       = postOrder.insert()
            if( iso.preCmp <= 0 ) {
               preOrder.placeBefore( iso.pre, this )
            } else {
               preOrder.placeAfter( iso.pre, this )
            }
            if( iso.postCmp <= 0 ) {
               postOrder.placeBefore( iso.post, this )
            } else {
               postOrder.placeAfter( iso.post, this )
            }
         }
      }

      final def remove( vertex: K )( implicit tx: S#Tx ) : Boolean = {
         val iso = query( vertex )
         (iso.preCmp == 0) /* && (iso.postCmp == 0) */ && {
            assert( iso.postCmp == 0 )
            iso.pre.removeAndDispose() // iso.pre is a VM!
            true
         }
      }

      final def -=( vertex: K )( implicit tx: S#Tx ) : this.type = {
         remove( vertex )
         this
      }

      final def get( vertex: K )( implicit tx: S#Tx ) : Option[ A ] = {
         val iso = query( vertex )
         if( iso.preCmp == 0 ) {
            assert( iso.postCmp == 0 )
            Some( iso.pre.value )
         } else None
      }

      final def nearest( vertex: K )( implicit tx: S#Tx ) : (K, A) = {
         val iso = query( vertex )
         if( iso.preCmp == 0 ) {
            assert( iso.postCmp == 0 )
            (vertex, iso.pre.value)
         } else {
            val preTag  = iso.pre.pre.tag
            val postTag = iso.post.post.tag
            val x       = if( iso.preCmp  < 0 ) preTag  - 1 else preTag
            val y       = if( iso.postCmp > 0 ) postTag + 1 else postTag
            val nn      = skip.nearestNeighbor( Point3D( x, y, vertex.versionInt ), metric )
            (nn.fullVertex, nn.value)
         }
      }

      // ---- RelabelObserver ----
      final def beforeRelabeling( iter: Iterator[ S#Tx, M ])( implicit tx: S#Tx ) {
//println( "RELABEL - ::: BEGIN :::" )
         iter.foreach { mv =>
//println( "RELABEL - " + mv )
            skip -= mv
         }
//println( "RELABEL - ::: END :::" )
      }

      final def afterRelabeling( iter: Iterator[ S#Tx, M ])( implicit tx: S#Tx ) {
//println( "RELABEL + ::: BEGIN :::" )
         iter.foreach { mv =>
//println( "RELABEL + " + mv )
            skip += mv
         }
//println( "RELABEL + ::: END :::" )
      }
   }

   private final class MapNew[ S <: Sys[ S ], Version, @specialized A ]( val full: Tree[ S, Version ],
      rootVertex: Vertex[ S, Version ], rootValue: A, tx0: S#Tx, val valueSerializer: TxnSerializer[ S#Tx, S#Acc, A ])
   extends MapImpl[ S, Version, A ] {
      me =>

      protected val preOrder  : TotalOrder.Map[ S, M ] =
         TotalOrder.Map.empty[ S, M ]( me, _.pre )( tx0, markSerializer )

      protected val postOrder : TotalOrder.Map[ S, M ] =
         TotalOrder.Map.empty[ S, M ]( me, _.post, rootTag = Int.MaxValue )( tx0, markSerializer )

      private[Ancestor] val skip: SkipOctree[ S, Space.ThreeDim, M ] = {
         val pointView = (p: M, tx: S#Tx) => p.toPoint( tx )
         SkipOctree.empty[ S, Space.ThreeDim, M ]( cube )( tx0, pointView, Space.ThreeDim, markSerializer,
                                                            SpaceSerializers.CubeSerializer )
      }

      protected val root: M = {
         val res: M = new M {
            def map        = me
            val fullVertex = rootVertex // full.root
            def pre        = preOrder.root
            def post       = postOrder.root
            val value      = rootValue

            override def toString = "Root(" + value + ")"
         }
         skip.+=( res )( tx0 )
         res
      }

      protected val preList : SkipList[ S, M ] = {
         implicit val ord  = preOrdering
         implicit val tx   = tx0
         val res           = SkipList.empty[ S, M ]
         res.add( root )
         res
      }

      protected val postList : SkipList[ S, M ] = {
         implicit val ord  = postOrdering
         implicit val tx   = tx0
         val res           = SkipList.empty[ S, M ]
         res.add( root )
         res
      }
   }

   private final class MapRead[ S <: Sys[ S ], Version, @specialized A ]( val full: Tree[ S, Version ], in: DataInput,
      access: S#Acc, tx0: S#Tx, val valueSerializer: TxnSerializer[ S#Tx, S#Acc, A ])
   extends MapImpl[ S, Version, A ] {
      me =>

      {
         val serVer = in.readUnsignedByte()
         require( serVer == SER_VERSION, "Incompatible serialized version (found " + serVer +
            ", required " + SER_VERSION + ")." )
      }

      protected val preOrder  : TotalOrder.Map[ S, M ] =
         TotalOrder.Map.read[ S, M ]( in, access, me, _.pre  )( tx0, markSerializer )

      protected val postOrder : TotalOrder.Map[ S, M ] =
         TotalOrder.Map.read[ S, M ]( in, access, me, _.post )( tx0, markSerializer )

      protected val preList : SkipList[ S, M ] = {
         implicit val ord  = preOrdering
         implicit val tx   = tx0
         SkipList.read[ S, M ]( in, access )
      }

      protected val postList : SkipList[ S, M ] = {
         implicit val ord  = postOrdering
         implicit val tx   = tx0
         SkipList.read[ S, M ]( in, access )
      }

      private[Ancestor] val skip: SkipOctree[ S, Space.ThreeDim, M ] = {
         val pointView = (p: M, tx: S#Tx) => p.toPoint( tx )
         SkipOctree.read[ S, Space.ThreeDim, M ]( in, access )( tx0, pointView, Space.ThreeDim, markSerializer,
            SpaceSerializers.CubeSerializer )
      }
   }

   sealed trait Map[ S <: Sys[ S ], Version, @specialized A ] extends Writer with Disposable[ S#Tx ] {
      type K = Vertex[ S, Version ]

      def full: Tree[ S, Version ]

      /**
       * Marks a given key with a given value.
       *
       * @param   entry the key-value pair (where the key is a vertex in the full tree)
       * @return  `true` if the mark is new, `false` if there had been a mark for the given vertex.
       */
      def add( entry: (K, A) )( implicit tx: S#Tx ) : Boolean
      def +=(  entry: (K, A) )( implicit tx: S#Tx ) : this.type
      def remove( vertex: K )( implicit tx: S#Tx ) : Boolean
      def -=(     vertex: K )( implicit tx: S#Tx ) : this.type

      /**
       * Queries for a mark at a given version vertex. Unlike `nearest`, this does
       * not search in the map, but merely tests if the given vertex has been
       * marked or not.
       *
       * @param   vertex  the version vertex to look up
       * @return  the value associated with that vertex, or `None` if the vertex is unmarked.
       */
      def get( vertex: K )( implicit tx: S#Tx ) : Option[ A ]

      /**
       * Finds the nearest marked ancestor of a given version key.
       * Since the map is constructed with a defined root value, this method is
       * guaranteed to succeed&mdash;if there are no other marks in the map,
       * it will return the root value (unless the `version` argument is
       * illegal, i.e. has a version lower than the root vertex' version).
       *
       * @param   vertex  the key to look for. The algorithm searches for
       *          the nearest ancestor in the marked map with a version less than or
       *          equal to the given version
       * @return  a pair consisting of the tree vertex found and the value with which
       *          it has been marked. If the query `version` vertex was marked, it will be
       *          that vertex which is returned, and not an ancestor.
       */
      def nearest( vertex: K )( implicit tx: S#Tx ) : (K, A)

      def valueSerializer: TxnSerializer[ S#Tx, S#Acc, A ]
   }
}