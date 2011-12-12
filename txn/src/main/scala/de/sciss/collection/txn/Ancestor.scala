/*
 *  Ancestor.scala
 *  (LucreData)
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

package de.sciss.collection.txn

import de.sciss.collection.geom.{DistanceMeasure3D, Point3D, Cube, Space}
import de.sciss.lucrestm.{Writer, Serializer, DataOutput, DataInput, Sys}

object Ancestor {
   private[Ancestor] val cube = Cube( 0x40000000, 0x40000000, 0x40000000, 0x40000000 )

//   private object PreKey {
//      implicit def reader[ S <: Sys[ S ], A ]( implicit vertexReader: Reader[ Vertex[ S, A ]]) : Reader[ PreKey[ S, A ]] =
//         new ReaderImpl[ S, A ]( vertexReader )
//
//      private final class ReaderImpl[ S <: Sys[ S ], A ]( vertexReader: Reader[ Vertex[ S, A ]])
//      extends Reader[ PreKey[ S, A ]] {
//         def read( in: DataInput ) : PreKey[ S, A ] = {
//            val id = in.readUnsignedByte()
//            val v  = vertexReader.read( in )
//            if( id == 0 ) v.preHeadKey else v.preTailKey
//         }
//      }
//   }

//   private type PreOrder[  S <: Sys[ S ], A ] = TotalOrder.Map.Entry[ S, PreKey[ S, A ]]
//   private type PostOrder[ S <: Sys[ S ], A ] = TotalOrder.Map.Entry[ S, Vertex[ S, A ]]
   private type TreePreOrder[  S <: Sys[ S ]] = TotalOrder.Set.Entry[ S ]
   private type TreePostOrder[ S <: Sys[ S ]] = TotalOrder.Set.Entry[ S ]

   //   private sealed trait PreKey[ S <: Sys[ S ], A ] extends VertexProxy[ S, A ] {
   //      def order: PreOrder[ S ]
   //      def id: Int
   //
   //      final def write( out: DataOutput ) {
   //         out.writeUnsignedByte( id )
   //         source.write( out )
   //      }
   //
   //      override def equals( that: Any ) : Boolean = {
   //         (that.isInstanceOf[ PreKey[ _, _ ]] && {
   //            val thatPre = that.asInstanceOf[ PreKey[ _, _ ]]
   //            (id == thatPre.id) && (source == thatPre.source)
   //         })
   //      }
   //   }
   //
   //   private final class PreHeadKey[ S <: Sys[ S ], A ]( val source: Vertex[ S, A ])
   //   extends PreKey[ S, A ] {
   //      def order   = source.preHead
   //      def id      = 0
   //
   //      override def toString = source.toString + "<pre>"
   //      def debugString( implicit tx: S#Tx ) = toString + "@" + source.preHead.tag
   //   }
   //
   //   private final class PreTailKey[ S <: Sys[ S ], A ]( val source: Vertex[ S, A ])
   //   extends PreKey[ S, A ] {
   //      def order   = source.preTail
   //      def id      = 1
   //
   //      override def toString = source.toString + "<pre-tail>"
   //      def debugString( implicit tx: S#Tx ) = toString + "@" + source.preTail.tag
   //   }
   //
   //   sealed trait VertexProxy[ S <: Sys[ S ], A ] extends Writer {
   //      private[FullTree] def source: Vertex[ S, A ]
   //   }

   object Vertex {
      private[Ancestor] implicit def toPoint[ S <: Sys[ S ], A ]( v: Vertex[ S, A ], tx: S#Tx ) : Point3D =
         new Point3D( v.preHead.tag( tx ), v.post.tag( tx ), v.version )

//      private[FullTree] implicit def vertexSerializer[ S <: Sys[ S ], A ](
//         implicit valueReader: Reader[ A ], versionView: A => Int ) : Serializer[ Vertex[ S, A ]] =
//            new SerializerImpl[ S, A ]( valueReader, versionView )
   }
   sealed trait Vertex[ S <: Sys[ S ], A ] /* extends VertexProxy[ S, A ] */ {
      def value: A
      final def version: Int = tree.versionView( value )

//      private[confluent] final def preCompare(  that: Vertex[ S, A ]) : Int = preHead.compare( that.preHead )
//      private[confluent] final def postCompare( that: Vertex[ S, A ]) : Int = post.compare(    that.post )

//      private[FullTree] final def source: Vertex[ S, A ] = this

//      private[FullTree] final val preHeadKey  = new PreHeadKey( this )
//      private[FullTree] final val preTailKey  = new PreTailKey( this )
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

      override def toString = "Vertex(" + value + ")"
   }

   def newTree[ S <: Sys[ S ], A ]( rootValue: A )( implicit tx: S#Tx, valueSerializer: Serializer[ A ],
                                                    versionView: A => Int, versionManifest: Manifest[ A ]) : Tree[ S, A ] =
      new TreeNew[ S, A ]( rootValue )

   private final class TreeNew[ S <: Sys[ S ], A ]( rootValue: A )(
      implicit tx: S#Tx, val valueSerializer: Serializer[ A ], val versionView: A => Int,
      val versionManifest: Manifest[ A ])
   extends Tree[ S, A ] /* with TotalOrder.Map.RelabelObserver[ S#Tx, VertexProxy[ S, A ]] */ {
      me =>

      private implicit object VertexSerializer extends Serializer[ V ] {
         def write( v: V, out: DataOutput ) { v.write( out )}

         def read( in: DataInput ) : V = {
            new V {
               def tree    = me
               val value   = valueSerializer.read( in )
               val preHead = preOrder.readEntry( in )
               val preTail = preOrder.readEntry( in )
               val post    = postOrder.readEntry( in )
            }
         }
      }

      def vertexSerializer : Serializer[ V ] = VertexSerializer

      val skip : SkipOctree[ S, Space.ThreeDim, V ] = {
         import SpaceSerializers.CubeSerializer
//         implicit val pv      = SkipOctree.nonTxnPointView[ Space.ThreeDim, V ]
         implicit val system  = tx.system
         implicit val smf     = Sys.manifest[ S ]
         SkipOctree.empty[ S, Space.ThreeDim, V ]( cube )
      }
//      val preOrder      = TotalOrder.Map.empty[ S, PreKey[ S, A ]]( me, _.order, 0 )
//      val postOrder     = TotalOrder.Map.empty[ S, V ](             me, _.post,  Int.MaxValue )
      val preOrder      = TotalOrder.Set.empty[ S ]( 0 )
      val postOrder     = TotalOrder.Set.empty[ S ]( Int.MaxValue )
      val root = new V {
         def tree: Tree[ S, A ] = me
         def value   = rootValue
         val preHead = preOrder.root
         val preTail = preHead.append() // preOrder.insert()
         val post    = postOrder.root
//         preOrder.placeAfter( preHeadKey, preTailKey )   // preTailKey must come last
      }
      skip += root

      def insertChild( parent: V, newChild: A )( implicit tx: S#Tx ) : V = {
         val v = new V {
            def tree    = me
            val value   = newChild
            val preHead = parent.preTail.prepend() // preOrder.insert()
            val preTail = preHead.append() // preOrder.insert()
            val post    = parent.post.prepend() // postOrder.insert()
//            preOrder.placeBefore( parent.preTailKey, preHeadKey )
//            postOrder.placeBefore( parent, this )
//            preOrder.placeAfter( preHeadKey, preTailKey )   // preTailKey must come last!
         }
         skip += v
         v
      }

      def insertRetroChild( parent: V, newChild: A )( implicit tx: S#Tx ) : V = {
         val v = new V {
            def tree    = me
            val value   = newChild
            val preHead = parent.preHead.append()  // preOrder.insert()
            val preTail = parent.preTail.prepend() // preOrder.insert()
            val post    = parent.post.prepend()    // postOrder.insert()
//            preOrder.placeAfter( parent.preHeadKey, preHeadKey )
//            postOrder.placeBefore( parent, this )
//            preOrder.placeBefore( parent.preTailKey, preTailKey ) // preTailKey must come last
            override def toString = super.toString + "@r-ch"
         }
         skip += v
         v
      }

      def insertRetroParent( child: V, newParent: A )( implicit tx: S#Tx ) : V = {
         require( child != root )
         val v = new V {
            def tree    = me
            val value   = newParent
            val preHead = child.preHead.prepend()  // preOrder.insert()
            val preTail = child.preTail.append()   // preOrder.insert()
            val post    = child.post.append()      // postOrder.insert()
//            preOrder.placeBefore( child.preHeadKey, preHeadKey )
//            postOrder.placeAfter( child, this )
//            preOrder.placeAfter( child.preTailKey, preTailKey )   // preTailKey must come last
            override def toString = super.toString + "@r-par"
         }
         skip += v
         v
      }

//      // ---- RelabelObserver ----
//
//      def beforeRelabeling( iter: Iterator[ S#Tx, VertexProxy[ S, A ]])( implicit tx: S#Tx ) {
//         // the nasty thing is, in the pre-order list the items appear twice
//         // due to pre versus preTail. thus the items might get removed twice
//         // here, too, and we cannot assert that t.remove( v ) == true
//         iter.foreach( skip -= _.source )
//      }
//
//      def afterRelabeling( iter: Iterator[ S#Tx, VertexProxy[ S, A ]])( implicit tx: S#Tx ) {
//         iter.foreach( skip += _.source )
//      }
   }

   sealed trait Tree[ S <:Sys[ S ], A ] {
      protected type V = Vertex[ S, A ]

      private[Ancestor] def valueSerializer: Serializer[ A ]
      private[Ancestor] def versionView: A => Int

      def vertexSerializer : Serializer[ V ]
      implicit def versionManifest : Manifest[ A ]

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

      def map: MapNew[ S, A, V ] // MarkTree[ S, A, V ]

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
      implicit tx: S#Tx, valueSerializer: Serializer[ V ], vmf: Manifest[ V ]) : Map[ S, A, V ] = {

      new MapNew[ S, A, V ]( full, rootValue )
   }

   private final class IsoResult[ S <: Sys[ S ], A, @specialized V ](
      val pre: Mark[ S, A, V ], val preCmp: Int, val post: Mark[ S, A, V ], val postCmp: Int ) {

      override def toString = "Iso(pre "  + (if( preCmp  < 0 ) "< " else if( preCmp  > 0) "> " else "== ") +  pre  + "," +
                                  "post " + (if( postCmp < 0 ) "< " else if( postCmp > 0) "> " else "== ") +  post + ")"
   }

   private final class MapNew[ S <: Sys[ S ], A, @specialized V ]( full: Tree[ S, A ], rootValue: V )(
      implicit tx: S#Tx, val valueSerializer: Serializer[ V ], vmf: Manifest[ V ])
   extends Map[ S, A, V ] with TotalOrder.Map.RelabelObserver[ S#Tx, Mark[ S, A, V ]] {
      me =>

      import full.versionManifest

      type MV = Mark[ S, A, V ]

      private implicit val vertexSerializer : Serializer[ MV ] = new Serializer[ MV ] {
         def write( v: MV, out: DataOutput ) { v.write( out )}

         def read( in: DataInput ) : MV = new MV {
            def map        = me
            val fullVertex = full.vertexSerializer.read( in )
            val pre        = preOrder.readEntry( in )
            val post       = postOrder.readEntry( in )
            val value      = valueSerializer.read( in )
         }
      }

      private val preOrder  : TotalOrder.Map[ S, MV ] = TotalOrder.Map.empty[ S, MV ]( me, _.pre )
      private val postOrder : TotalOrder.Map[ S, MV ] = TotalOrder.Map.empty[ S, MV ]( me, _.post, rootTag = Int.MaxValue )

      val skip: SkipOctree[ S, Space.ThreeDim, MV ] = {
         implicit val pointView = (p: MV, tx: S#Tx) => p.toPoint( tx )
         import SpaceSerializers.CubeSerializer
         implicit val system              = tx.system
         implicit val smf: Manifest[ S ]  = Sys.manifest[ S ]
         SkipOctree.empty[ S, Space.ThreeDim, MV ]( cube )
      }

      val root: MV = {
         val res = new MV {
            def map        = me
            def fullVertex = full.root
            def pre        = preOrder.root
            def post       = postOrder.root
            def value      = rootValue

            override def toString = "Root(" + value + ")"
         }
         skip += res
         res
      }

      val preList : SkipList[ S, MV ] = {
         implicit val ord = new Ordering[ S#Tx, MV ] {
            def compare( a: MV, b: MV )( implicit tx: S#Tx ) : Int = a.pre compare b.pre
         }
         implicit val system              = tx.system
         implicit val smf: Manifest[ S ]  = Sys.manifest[ S ]
         val res = SkipList.empty[ S, MV ]
         res.add( root )
         res
      }

      val postList : SkipList[ S, MV ] = {
         implicit val ord = new Ordering[ S#Tx, MV ] {
            def compare( a: MV, b: MV )( implicit tx: S#Tx ) : Int = a.post compare b.post
         }
         implicit val system = tx.system
         implicit val smf: Manifest[ S ] = Sys.manifest[ S ]
         val res = SkipList.empty[ S, MV ]
         res.add( root )
         res
      }

      def add( entry: (K, V) )( implicit tx: S#Tx ) : Boolean = {
         val mv    = wrap( entry )
         preList  += mv
         postList += mv
         skip.add( mv )
      }

      def +=( entry: (K, V) )( implicit tx: S#Tx ) : this.type = {
         add( entry )
         this
      }

      private def query( version: K ) : IsoResult[ S, A, V ] = {
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

      private def wrap( entry: (K, V) ) : MV = {
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

      def remove( version: K )( implicit tx: S#Tx ) : Boolean = {
         val iso = query( version )
         (iso.preCmp == 0) /* && (iso.postCmp == 0) */ && {
            assert( iso.postCmp == 0 )
            iso.pre.removeAndDispose() // iso.pre is a VM!
            true
         }
      }

      def -=( version: K )( implicit tx: S#Tx ) : this.type = {
         remove( version )
         this
      }

      def get( version: K )( implicit tx: S#Tx ) : Option[ V ] = {
         val iso = query( version )
         if( iso.preCmp == 0 ) {
            assert( iso.postCmp == 0 )
            Some( iso.pre.value )
         } else None
      }

      def nearest( version: K )( implicit tx: S#Tx ) : (K, V) = {
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
      def beforeRelabeling( iter: Iterator[ S#Tx, MV ])( implicit tx: S#Tx ) {
         iter.foreach( skip -= _ )
      }

      def afterRelabeling( iter: Iterator[ S#Tx, MV ])( implicit tx: S#Tx ) {
         iter.foreach( skip += _ )
      }
   }

   sealed trait Map[ S <:Sys[ S ], A, @specialized V ] {
      type K = Vertex[ S, A ]

      /**
       * Marks a given key with a given value.
       *
       * @param   entry the key-value pair (where the key is a vertex in the full tree)
       * @return  `true` if the mark is new, `false` if there had been a mark for the given vertex.
       */
      def add( entry: (K, V) )( implicit tx: S#Tx ) : Boolean
      def +=( entry: (K, V) )( implicit tx: S#Tx ) : this.type
      def remove( version: K )( implicit tx: S#Tx ) : Boolean
      def -=( version: K )( implicit tx: S#Tx ) : this.type

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

      def valueSerializer: Serializer[ V ]
   }
}