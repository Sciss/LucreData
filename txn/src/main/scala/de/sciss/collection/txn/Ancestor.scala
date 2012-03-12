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
      private[Ancestor] implicit def toPoint[ S <: Sys[ S ], A ]( v: Vertex[ S, A ], tx: S#Tx ) : Point3D =
         new Point3D( v.preHead.tag( tx ), v.post.tag( tx ), v.version )
   }
   sealed trait Vertex[ S <: Sys[ S ], A ] extends Writer with Disposable[ S#Tx ] {
      def value: A
      final def version: Int = tree.versionView( value )

      private[Ancestor] def preHead: TreePreOrder[ S ]
      private[Ancestor] def preTail: TreePreOrder[ S ]
      private[Ancestor] def post:    TreePostOrder[ S ]

      private[Ancestor] def tree: Tree[ S, A ]

      final def write( out: DataOutput ) {
         tree.valueSerializer.write( value, out )
         preHead.write( out )
         preTail.write( out )
         post.write( out )
      }

      final def dispose()( implicit tx: S#Tx ) {
         preHead.dispose()
         preTail.dispose()
         post.dispose()
      }

      override def toString = "Vertex(" + value + ")"
   }

   implicit def treeSerializer[ S <: Sys[ S ], A ]( implicit valueSerializer: TxnSerializer[ S#Tx, S#Acc, A ],
                                                    versionView: A => Int) : TxnSerializer[ S#Tx, S#Acc, Tree[ S, A ]] =
      new TreeSer[ S, A ]

   def newTree[ S <: Sys[ S ], A ]( rootValue: A )( implicit tx: S#Tx, valueSerializer: TxnSerializer[ S#Tx, S#Acc, A ],
                                                    versionView: A => Int ) : Tree[ S, A ] =
      new TreeNew[ S, A ]( rootValue, tx )

   def readTree[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc )
                                   ( implicit tx: S#Tx, valueSerializer: TxnSerializer[ S#Tx, S#Acc, A ],
                                     versionView: A => Int ) : Tree[ S, A ] =
      new TreeRead[ S, A ]( in, access, tx )

   private final class TreeSer[ S <: Sys[ S ], A ]( implicit valueSerializer: TxnSerializer[ S#Tx, S#Acc, A ],
                                                    versionView: A => Int )
   extends TxnSerializer[ S#Tx, S#Acc, Tree[ S, A ]] {
      def write( t: Tree[ S, A ], out: DataOutput ) { t.write( out )}

      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Tree[ S, A ] =
         new TreeRead[ S, A ]( in, access, tx )
   }

   private sealed trait TreeImpl[ S <: Sys[ S ], A ] extends Tree[ S, A ] {
      me =>

      protected def preOrder  : TotalOrder.Set[ S ]
      protected def postOrder : TotalOrder.Set[ S ]

      implicit protected object VertexSerializer extends TxnSerializer[ S#Tx, S#Acc, V ] {
         def write( v: V, out: DataOutput ) { v.write( out )}

         def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : V = {
            new V {
               def tree    = me
               val value   = valueSerializer.read( in, access )
               val preHead = preOrder.readEntry(   in, access )
               val preTail = preOrder.readEntry(   in, access )
               val post    = postOrder.readEntry(  in, access )
            }
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

      final def vertexSerializer : TxnSerializer[ S#Tx, S#Acc, V ] = VertexSerializer

      final def insertChild( parent: V, newChild: A )( implicit tx: S#Tx ) : V = {
         val v = new V {
            def tree    = me
            val value   = newChild
            val preHead = parent.preTail.prepend() // preOrder.insert()
            val preTail = preHead.append() // preOrder.insert()
            val post    = parent.post.prepend() // postOrder.insert()
         }
         v
      }

      final def insertRetroChild( parent: V, newChild: A )( implicit tx: S#Tx ) : V = {
         val v = new V {
            def tree    = me
            val value   = newChild
            val preHead = parent.preHead.append()  // preOrder.insert()
            val preTail = parent.preTail.prepend() // preOrder.insert()
            val post    = parent.post.prepend()    // postOrder.insert()
            override def toString = super.toString + "@r-ch"
         }
         v
      }

      final def insertRetroParent( child: V, newParent: A )( implicit tx: S#Tx ) : V = {
         require( child != root )
         val v = new V {
            def tree    = me
            val value   = newParent
            val preHead = child.preHead.prepend()  // preOrder.insert()
            val preTail = child.preTail.append()   // preOrder.insert()
            val post    = child.post.append()      // postOrder.insert()
            override def toString = super.toString + "@r-par"
         }
         v
      }
   }

   private final class TreeNew[ S <: Sys[ S ], A ]( rootValue: A, tx0: S#Tx )(
      implicit val valueSerializer: TxnSerializer[ S#Tx, S#Acc, A ], val versionView: A => Int )
   extends TreeImpl[ S, A ] {
      me =>

      protected val preOrder      = TotalOrder.Set.empty[ S ]( 0 )( tx0 )
      protected val postOrder     = TotalOrder.Set.empty[ S ]( Int.MaxValue )( tx0 )
      val root = new V {
         def tree: Tree[ S, A ] = me
         def value   = rootValue
         val preHead = preOrder.root
         val preTail = preHead.append()( tx0 ) // preOrder.insert()
         val post    = postOrder.root
      }
   }

   private final class TreeRead[ S <: Sys[ S ], A ]( in: DataInput, access: S#Acc, tx0: S#Tx  )(
      implicit val valueSerializer: TxnSerializer[ S#Tx, S#Acc, A ], val versionView: A => Int )
   extends TreeImpl[ S, A ] {

      {
         val version = in.readUnsignedByte()
         require( version == SER_VERSION, "Incompatible serialized version (found " + version +
            ", required " + SER_VERSION + ")." )
      }

      protected val preOrder  = TotalOrder.Set.read[ S ]( in, access )( tx0 )
      protected val postOrder = TotalOrder.Set.read[ S ]( in, access )( tx0 )
      val root                = VertexSerializer.read( in, access )( tx0 )
   }

   sealed trait Tree[ S <:Sys[ S ], A ] extends Writer with Disposable[ S#Tx ] {
      protected type V = Vertex[ S, A ]

      private[Ancestor] def valueSerializer: TxnSerializer[ S#Tx, S#Acc, A ]
      private[Ancestor] def versionView: A => Int

      def vertexSerializer : TxnSerializer[ S#Tx, S#Acc, V ]

      def root : V

      def insertChild( parent: V, newChild: A )( implicit tx: S#Tx ) : V

      def insertRetroChild( parent: V, newChild: A )( implicit tx: S#Tx ) : V

      def insertRetroParent( child: V, newParent: A )( implicit tx: S#Tx ) : V

   //   def mark()( implicit tx: S#Tx ) : MarkTree[ S, A ]
   }

   private type MarkOrder[ S <: Sys[ S ], A, V ] = TotalOrder.Map.Entry[ S, Mark[ S, A, V ]]

   private val metric = DistanceMeasure3D.chebyshevXY.orthant( 2 )

   private sealed trait Mark[ S <: Sys[ S ], A, @specialized V ] extends Writer {
      def fullVertex: Vertex[ S, A ]
      final def toPoint( implicit tx: S#Tx ): Point3D = new Point3D( pre.tag, post.tag, fullVertex.version )
      def pre:  MarkOrder[ S, A, V ]
      def post: MarkOrder[ S, A, V ]
      def value: V

      def map: MapImpl[ S, A, V ] // MarkTree[ S, A, V ]

      def write( out: DataOutput ) {
         fullVertex.write( out )
         pre.write( out )
         post.write( out )
         map.valueSerializer.write( value, out )
      }

      def removeAndDispose()( implicit tx: S#Tx ) {
         map.skip.remove( this )
         pre.removeAndDispose()
         post.removeAndDispose()
      }

      override def toString = "Mark(" + fullVertex.value + " -> " + value + ")"
   }

   def newMap[ S <: Sys[ S ], A, @specialized V ]( full: Tree[ S, A ], rootValue: V )(
      implicit tx: S#Tx, valueSerializer: TxnSerializer[ S#Tx, S#Acc, V ]) : Map[ S, A, V ] = {

      new MapNew[ S, A, V ]( full, rootValue, tx )
   }

   def readMap[ S <: Sys[ S ], A, @specialized V ]( in: DataInput, access: S#Acc, full: Tree[ S, A ])(
      implicit tx: S#Tx, valueSerializer: TxnSerializer[ S#Tx, S#Acc, V ]) : Map[ S, A, V ] = {

      new MapRead[ S, A, V ]( full, in, access, tx )
   }

   private final class MapSer[ S <: Sys[ S ], A, V ]( full: Tree[ S, A ])(
      implicit valueSerializer: TxnSerializer[ S#Tx, S#Acc, V ])
   extends TxnSerializer[ S#Tx, S#Acc, Map[ S, A, V ]] {
      def write( m: Map[ S, A, V ], out: DataOutput ) { m.write( out )}

      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Map[ S, A, V ] = {

         new MapRead[ S, A, V ]( full, in, access, tx )
      }
   }

   private final class IsoResult[ S <: Sys[ S ], A, @specialized V ](
      val pre: Mark[ S, A, V ], val preCmp: Int, val post: Mark[ S, A, V ], val postCmp: Int ) {

      override def toString = "Iso(pre "  + (if( preCmp  < 0 ) "< " else if( preCmp  > 0) "> " else "== ") +  pre  + "," +
                                  "post " + (if( postCmp < 0 ) "< " else if( postCmp > 0) "> " else "== ") +  post + ")"
   }

   private sealed trait MapImpl[ S <: Sys[ S ], A, @specialized V ]
   extends Map[ S, A, V ] with TotalOrder.Map.RelabelObserver[ S#Tx, Mark[ S, A, V ]] {
      me =>

      final type MV = Mark[ S, A, V ]

      protected def preOrdering : Ordering[ S#Tx, MV ] = new Ordering[ S#Tx, MV ] {
         def compare( a: MV, b: MV )( implicit tx: S#Tx ) : Int = a.pre compare b.pre
      }

      protected def postOrdering : Ordering[ S#Tx, MV ] = new Ordering[ S#Tx, MV ] {
         def compare( a: MV, b: MV )( implicit tx: S#Tx ) : Int = a.post compare b.post
      }
      protected def full: Tree[ S, A ]
//      private[Ancestor] implicit def valueSerializer: TxnSerializer[ S#Tx, S#Acc, V ]

      protected def preOrder  : TotalOrder.Map[ S, MV ]
      protected def postOrder : TotalOrder.Map[ S, MV ]

      private[Ancestor] def skip: SkipOctree[ S, Space.ThreeDim, MV ]
//      protected def root: MV

      protected def preList : SkipList[ S, MV ]
      protected def postList : SkipList[ S, MV ]

      protected implicit object vertexSerializer extends TxnSerializer[ S#Tx, S#Acc, MV ] {
         def write( v: MV, out: DataOutput ) { v.write( out )}

         def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : MV = new MV {
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
         preList.write( out )
         postList.write( out )
         skip.write( out )
//         root.write( out )
      }

      final def dispose()( implicit tx: S#Tx ) {
         preList.dispose()
         postList.dispose()
         skip.dispose()
      }

      final def add( entry: (K, V) )( implicit tx: S#Tx ) : Boolean = {
         val mv    = wrap( entry )
         preList  += mv
         postList += mv
         skip.add( mv )
      }

      final def +=( entry: (K, V) )( implicit tx: S#Tx ) : this.type = {
         add( entry )
         this
      }

      private def query( version: K )( implicit tx: S#Tx ) : IsoResult[ S, A, V ] = {
         val cfPre = version.preHead
         val (cmPreN, cmPreCmp) = preList.isomorphicQuery( new Ordered[ S#Tx, MV ] {
            def compare( that: MV )( implicit tx: S#Tx ) : Int = {
               cfPre.compare( that.fullVertex.preHead )
            }
         })
         val cfPost = version.post
         val (cmPostN, cmPostCmp ) = postList.isomorphicQuery( new Ordered[ S#Tx, MV ] {
            def compare( that: MV )( implicit tx: S#Tx ) : Int = {
               cfPost.compare( that.fullVertex.post )
            }
         })
         new IsoResult[ S, A, V ]( cmPreN, cmPreCmp, cmPostN, cmPostCmp )
      }

      private def wrap( entry: (K, V) )( implicit tx: S#Tx ) : MV = {
         val version = entry._1
         val iso = query( version )
         new MV {
            def map        = me
            val fullVertex = version
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
            val value      = entry._2
         }
      }

      final def remove( version: K )( implicit tx: S#Tx ) : Boolean = {
         val iso = query( version )
         (iso.preCmp == 0) /* && (iso.postCmp == 0) */ && {
            assert( iso.postCmp == 0 )
            iso.pre.removeAndDispose() // iso.pre is a VM!
            true
         }
      }

      final def -=( version: K )( implicit tx: S#Tx ) : this.type = {
         remove( version )
         this
      }

      final def get( version: K )( implicit tx: S#Tx ) : Option[ V ] = {
         val iso = query( version )
         if( iso.preCmp == 0 ) {
            assert( iso.postCmp == 0 )
            Some( iso.pre.value )
         } else None
      }

      final def nearest( version: K )( implicit tx: S#Tx ) : (K, V) = {
         val iso = query( version )
         if( iso.preCmp == 0 ) {
            assert( iso.postCmp == 0 )
            (version, iso.pre.value)
         } else {
            val preTag  = iso.pre.pre.tag
            val postTag = iso.post.post.tag
            val x       = if( iso.preCmp  < 0 ) preTag  - 1 else preTag
            val y       = if( iso.postCmp > 0 ) postTag + 1 else postTag
            val nn      = skip.nearestNeighbor( Point3D( x, y, version.version ), metric )
            (nn.fullVertex, nn.value)
         }
      }

      // ---- RelabelObserver ----
      final def beforeRelabeling( iter: Iterator[ S#Tx, MV ])( implicit tx: S#Tx ) {
//println( "RELABEL - ::: BEGIN :::" )
         iter.foreach { mv =>
//println( "RELABEL - " + mv )
            skip -= mv
         }
//println( "RELABEL - ::: END :::" )
      }

      final def afterRelabeling( iter: Iterator[ S#Tx, MV ])( implicit tx: S#Tx ) {
//println( "RELABEL + ::: BEGIN :::" )
         iter.foreach { mv =>
//println( "RELABEL + " + mv )
            skip += mv
         }
//println( "RELABEL + ::: END :::" )
      }
   }

   private final class MapNew[ S <: Sys[ S ], A, @specialized V ]( protected val full: Tree[ S, A ], rootValue: V, tx0: S#Tx )(
      implicit val valueSerializer: TxnSerializer[ S#Tx, S#Acc, V ])
   extends MapImpl[ S, A, V ] {
      me =>

      protected val preOrder  : TotalOrder.Map[ S, MV ] =
         TotalOrder.Map.empty[ S, MV ]( me, _.pre )( tx0, vertexSerializer )

      protected val postOrder : TotalOrder.Map[ S, MV ] =
         TotalOrder.Map.empty[ S, MV ]( me, _.post, rootTag = Int.MaxValue )( tx0, vertexSerializer )

      private[Ancestor] val skip: SkipOctree[ S, Space.ThreeDim, MV ] = {
         val pointView = (p: MV, tx: S#Tx) => p.toPoint( tx )
         SkipOctree.empty[ S, Space.ThreeDim, MV ]( cube )( tx0, pointView, Space.ThreeDim, vertexSerializer,
                                                            SpaceSerializers.CubeSerializer )
      }

      protected val root: MV = {
         val res = new MV {
            def map        = me
            def fullVertex = full.root
            def pre        = preOrder.root
            def post       = postOrder.root
            def value      = rootValue

            override def toString = "Root(" + value + ")"
         }
         skip.+=( res )( tx0 )
         res
      }

      protected val preList : SkipList[ S, MV ] = {
         implicit val ord  = preOrdering
         implicit val tx   = tx0
         val res           = SkipList.empty[ S, MV ]
         res.add( root )
         res
      }

      protected val postList : SkipList[ S, MV ] = {
         implicit val ord  = postOrdering
         implicit val tx   = tx0
         val res           = SkipList.empty[ S, MV ]
         res.add( root )
         res
      }
   }

   private final class MapRead[ S <: Sys[ S ], A, @specialized V ]( protected val full: Tree[ S, A ], in: DataInput,
                                                                    access: S#Acc, tx0: S#Tx )(
      implicit val valueSerializer: TxnSerializer[ S#Tx, S#Acc, V ])
   extends MapImpl[ S, A, V ] {
      me =>

      {
         val version = in.readUnsignedByte()
         require( version == SER_VERSION, "Incompatible serialized version (found " + version +
            ", required " + SER_VERSION + ")." )
      }

      protected val preOrder  : TotalOrder.Map[ S, MV ] =
         TotalOrder.Map.read[ S, MV ]( in, access, me, _.pre  )( tx0, vertexSerializer )

      protected val postOrder : TotalOrder.Map[ S, MV ] =
         TotalOrder.Map.read[ S, MV ]( in, access, me, _.post )( tx0, vertexSerializer )

      private[Ancestor] val skip: SkipOctree[ S, Space.ThreeDim, MV ] = {
         val pointView = (p: MV, tx: S#Tx) => p.toPoint( tx )
         SkipOctree.read[ S, Space.ThreeDim, MV ]( in, access )( tx0, pointView, Space.ThreeDim, vertexSerializer,
            SpaceSerializers.CubeSerializer )
      }

      protected val preList : SkipList[ S, MV ] = {
         implicit val ord  = preOrdering
         implicit val tx   = tx0
         SkipList.read[ S, MV ]( in, access )
      }

      protected val postList : SkipList[ S, MV ] = {
         implicit val ord  = postOrdering
         implicit val tx   = tx0
         SkipList.read[ S, MV ]( in, access )
      }
   }

   sealed trait Map[ S <:Sys[ S ], A, @specialized V ] extends Writer with Disposable[ S#Tx ] {
      type K = Vertex[ S, A ]

      /**
       * Marks a given key with a given value.
       *
       * @param   entry the key-value pair (where the key is a vertex in the full tree)
       * @return  `true` if the mark is new, `false` if there had been a mark for the given vertex.
       */
      def add( entry: (K, V) )( implicit tx: S#Tx ) : Boolean
      def +=(  entry: (K, V) )( implicit tx: S#Tx ) : this.type
      def remove( version: K )( implicit tx: S#Tx ) : Boolean
      def -=(     version: K )( implicit tx: S#Tx ) : this.type

      /**
       * Queries for a mark at a given version vertex. Unlike `nearest`, this does
       * not search in the map, but merely tests if the given vertex has been
       * marked or not.
       *
       * @param   version  the version vertex to look up
       * @return  the value associated with that vertex, or `None` if the vertex is unmarked.
       */
      def get( version: K )( implicit tx: S#Tx ) : Option[ V ]

      /**
       * Finds the nearest marked ancestor of a given version key.
       * Since the map is constructed with a defined root value, this method is
       * guaranteed to succeed&mdash;if there are no other marks in the map,
       * it will return the root value (unless the `version` argument is
       * illegal, i.e. has a version lower than the root vertex' version).
       *
       * @param   version  the key to look for. The algorithm searches for
       *          the nearest ancestor in the marked map with a version less than or
       *          equal to the given version
       * @return  a pair consisting of the tree vertex found and the value with which
       *          it has been marked. If the query `version` vertex was marked, it will be
       *          that vertex which is returned, and not an ancestor.
       */
      def nearest( version: K )( implicit tx: S#Tx ) : (K, V)

      def valueSerializer: TxnSerializer[ S#Tx, S#Acc, V ]
   }
}