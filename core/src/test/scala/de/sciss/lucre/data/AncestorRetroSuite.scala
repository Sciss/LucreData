package de.sciss
package lucre
package data

import org.scalatest.{GivenWhenThen, FeatureSpec}
import geom.{IntSpace, IntPoint3D, IntDistanceMeasure3D, IntCube}
import concurrent.stm.Ref
import java.io.File
import stm.store.BerkeleyDB
import stm.{InMemory, Durable, Cursor, Sys}
import annotation.tailrec
import collection.immutable.{Vector => Vec}
import serial.{Reader, Writable, DataInput, DataOutput, Serializer}

/**
* To run this test copy + paste the following into sbt:
* {{
* test-only de.sciss.lucre.data.AncestorRetroSuite
* }}
*/
class AncestorRetroSuite extends FeatureSpec with GivenWhenThen {
  val PARENT_LOOKUP             = true
  val MARKED_ANCESTOR           = true
  val NUM1                      = 10000   // 4407 // 4407   // 10000 // 283 // 10000  // tree size in PARENT_LOOKUP
  val NUM2                      = 11000   // tree size in MARKED_ANCESTOR // 100000    // 150000
  val MARKER_PERCENTAGE         = 0.3     // 0.3       // 0.5 // percentage of elements marked (0 to 1)
  val RETRO_CHILD_PERCENTAGE    = 0.1     // from those elements marked, amount which are inserted as retro-children (0 to 1)
  val RETRO_PARENT_PERCENTAGE   = 0.1     // from those elements marked, amount which are inserted as retro-parents (0 to 1)

  val INMEMORY                  = true
  val DATABASE                  = true

  val VERIFY_MARKTREE_CONTENTS  = false   // be careful to not enable this with large TREE_SIZE (> some 1000)
  val PRINT_DOT                 = false
  val PRINT_ORDERS              = false   // to print out the pre- and post-order lists (enable for debugging only)
  val HUNT_DOWN_ORDER_BUGS      = false

  def seed: Long                = 0L

  var verbose                   = false
  val DEBUG_LAST                = false   // if enabled, switches to verbosity for the last element in the sequence

  if (INMEMORY) {
    withSys[InMemory]("Mem", () => InMemory(): InMemory /* please IDEA */ , (_, _) => ())
  }
  if (DATABASE) {
    withSys[Durable]("BDB", () => {
      val dir = File.createTempFile("ancestor", "_database")
      dir.delete()
      dir.mkdir()
      println(dir.getAbsolutePath)
      Durable(BerkeleyDB.open(dir)): Durable /* please IDEA */
    }, {
      case (bdb, success) =>
        if (success) {
          //            val sz = bdb.step( bdb.numUserRecords( _ ))
          //            if( sz != 0 ) bdb.step( implicit tx => bdb.debugListUserRecords() ).foreach( println )
          //            assert( sz == 0, "Final DB user size should be 0, but is " + sz )
        }
        bdb.close()
    })
  }

  object FullVertexPre {
    implicit def serializer[S <: Sys[S]](implicit vertexReader: Reader[S#Tx, S#Acc, FullVertex[S]]): Serializer[S#Tx, S#Acc, FullVertexPre[S]] =
      new SerImpl[S](vertexReader)

    private final class SerImpl[S <: Sys[S]](vertexReader: Reader[S#Tx, S#Acc, FullVertex[S]])
      extends Serializer[S#Tx, S#Acc, FullVertexPre[S]] {
      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): FullVertexPre[S] = {
        val id  = in.readByte()
        val v   = vertexReader.read(in, access)
        if (id == 0) v.preHeadKey else v.preTailKey
      }

      def write(v: FullVertexPre[S], out: DataOutput) {
        v.write(out)
      }
    }
  }

  sealed trait FullVertexPre[S <: Sys[S]] extends Writable with VertexSource[S, FullVertex[S]] {
    def order: FullPreOrder[S]

    def id: Int

    final def write(out: DataOutput) {
      out.writeByte(id)
      source.write(out)
    }

    override def equals(that: Any): Boolean =
      (that.isInstanceOf[FullVertexPre[_]] && {
        val thatPre = that.asInstanceOf[FullVertexPre[_]]
        (id == thatPre.id) && (source == thatPre.source)
      })
  }

  final class FullVertexPreHead[S <: Sys[S]](val source: FullVertex[S])
    extends FullVertexPre[S] {

    def order = source.pre

    def id = 0

    override def toString = source.toString + "<pre>"

    def debugString(implicit tx: S#Tx) = toString + "@" + source.pre.tag
  }

  final class FullVertexPreTail[S <: Sys[S]](val source: FullVertex[S])
    extends FullVertexPre[S] {

    def order = source.preTail

    def id = 1

    override def toString = source.toString + "<pre-tail>"

    def debugString(implicit tx: S#Tx) = toString + "@" + source.preTail.tag
  }

  object FullTree {
    def apply[S <: Sys[S]]()(implicit tx: S#Tx): FullTree[S] = {
      implicit val pointView = (p: FullVertex[S], tx: S#Tx) => p.toPoint(tx)
      new FullTree[S] {
        val system = tx.system
        val cube = IntCube(0x40000000, 0x40000000, 0x40000000, 0x40000000)
        val t = {
          SkipOctree.empty[S, IntSpace.ThreeDim, FullVertex[S]](cube)
        }
        val orderObserver = new RelabelObserver[ S, FullVertex[ S ]]( "full", t )
        val preOrder      = TotalOrder.Map.empty[S, FullVertexPre[S]](orderObserver, _.order, 0)
        val postOrder     = TotalOrder.Map.empty[S, FullVertex[S]](orderObserver, _.post, Int.MaxValue /* - 1 */)
        implicit lazy val vertexSer: Serializer[S#Tx, S#Acc, FullVertex[S]] = new Serializer[S#Tx, S#Acc, FullVertex[S]] {
          def write(v: FullVertex[S], out: DataOutput) {
            v.write(out)
          }

          def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): FullVertex[S] = {
            new FullVertex[S] {
              val version = in.readInt()
              val pre = preOrder.readEntry(in, access)
              val post = postOrder.readEntry(in, access)
              val preTail = preOrder.readEntry(in, access)
            }
          }
        }
        val root: FullVertex[ S ] = new FullVertex[ S ] {
//               val pre: FullPreOrder[ S ]       = preOrder.root
//               val post: FullPostOrder[ S ]     = postOrder.root // insertAfter( this, this )
//               val preTail: FullPreOrder[ S ]   = preOrder.insertAfter( preHeadKey, preTailKey )
               val pre: FullPreOrder[ S ]       = preOrder.root
               val post: FullPostOrder[ S ]     = postOrder.root // insertAfter( this, this )
               val preTail: FullPreOrder[ S ]   = preOrder.insert()
               val version = 0
               preOrder.placeAfter( preHeadKey, preTailKey )   // preTailKey must come last
//               override def toString = "full-root"
            }
            t += root
         }
      }
   }
//   final class FullTree[ S <: Sys[ S ]] private (
//      val t: SkipOctree[ S, Space.IntThreeDim, FullVertex[ S ]],
//      val root: FullRootVertex[ S ],
//      val preOrder: TotalOrder.Map[ S, FullVertexPre[ S ]],
//      val postOrder: TotalOrder.Map[ S, FullVertex[ S ]],
//      val vertexSer: Serializer[ FullVertex[ S ]]
//   ) {

   sealed trait FullTree[ S <: Sys[ S ]] {
      def system: S
      def t: SkipOctree[ S, IntSpace.ThreeDim, FullVertex[ S ]]
      def root: FullVertex[ S ]
      def preOrder: TotalOrder.Map[ S, FullVertexPre[ S ]]
      def postOrder: TotalOrder.Map[ S, FullVertex[ S ]]
      def vertexSer: Serializer[ S#Tx, S#Acc, FullVertex[ S ]]

      private type V = FullVertex[ S ]

      private val versionCnt = Ref( 1 )
      def nextVersion()( implicit tx: S#Tx ) : Int = {
         val res = versionCnt.get( tx.peer )
         versionCnt.set( res + 1 )( tx.peer )
         res
      }

      def insertChild( parent: V )( implicit tx: S#Tx ) : V = {
         val v = new FullVertex[ S ] {
//            val pre     = preOrder.insertBefore( parent.preTailKey, preHeadKey )
//            val preTail = preOrder.insertAfter( preHeadKey, preTailKey )
//            val post    = postOrder.insertBefore( parent, this )
            val pre     = preOrder.insert()
            val preTail = preOrder.insert()
            val post    = postOrder.insert()
            val version = nextVersion()
//if( version == 411 ) {
//   println( "AHAAAA" )
//}
            preOrder.placeBefore( parent.preTailKey, preHeadKey )
            postOrder.placeBefore( parent, this )
            preOrder.placeAfter( preHeadKey, preTailKey )   // preTailKey must come last!
         }

if( verbose ) {
   val (chStr, pStr) = v.toPoint -> parent.toPoint
   println( "Full ins. child " + chStr + " with parent " + pStr )
}
         t.add( v )
         v
      }

      def insertRetroChild( parent: V )( implicit tx: S#Tx ) : V = {
         val v = new FullVertex[ S ] {
//            val pre     = preOrder.insertAfter( parent.preHeadKey, preHeadKey )
//            val preTail = preOrder.insertBefore( parent.preTailKey, preTailKey )
//            val post    = postOrder.insertBefore( parent, this )
            val pre     = preOrder.insert()
            val preTail = preOrder.insert()
            val post    = postOrder.insert()
            val version = nextVersion()
            preOrder.placeAfter( parent.preHeadKey, preHeadKey )
            postOrder.placeBefore( parent, this )
            preOrder.placeBefore( parent.preTailKey, preTailKey ) // preTailKey must come last
            override def toString = super.toString + "@r-ch"
         }

         t.add( v )
         v
      }

      def insertRetroParent( child: V )( implicit tx: S#Tx ) : V = {
         require( child != root )
         val v = new FullVertex[ S ] {
//            val pre     = preOrder.insertBefore( child.preHeadKey, preHeadKey )
//            val preTail = preOrder.insertAfter( child.preTailKey, preTailKey )
//            val post    = postOrder.insertAfter( child, this )
            val pre     = preOrder.insert()
            val preTail = preOrder.insert()
            val post    = postOrder.insert()
            val version = nextVersion()
            preOrder.placeBefore( child.preHeadKey, preHeadKey )
            postOrder.placeAfter( child, this )
            preOrder.placeAfter( child.preTailKey, preTailKey )   // preTailKey must come last
            override def toString = super.toString + "@r-par"
         }

         t.add( v )
         v
      }

//      def validate() {
////         When( "the size of the vertices is queried from the quadtree" )
////         Then( "it should be equal to the number of observed labelings and relabelings" )
////         val qsz = t.system.step { implicit tx => t.size }
////         assert( qsz == preObserver.map.size,
////            "pre-observer size (" + preObserver.map.size + ") is different from quad size (" + qsz + ")" )
////         assert( qsz == postObserver.map.size,
////            "post-observer size (" + postObserver.map.size + ") is different from quad size (" + qsz + ")" )
//      }
   }

   type FullPreOrder[ S <: Sys[ S ]]  = TotalOrder.Map.Entry[ S, FullVertexPre[ S ]]
   type FullPostOrder[ S <: Sys[ S ]] = TotalOrder.Map.Entry[ S, FullVertex[ S ]]

   sealed trait VertexSource[ S <: Sys[ S ], V ] {
      def source: V
      def debugString( implicit tx: S#Tx ) : String
   }

   sealed trait VertexLike[ S <: Sys[ S ], Repr ] extends Writable with VertexSource[ S, Repr ] {
      def version : Int
      def toPoint( implicit tx: S#Tx ) : IntPoint3D
   }

   sealed trait FullVertex[ S <: Sys[ S ]] extends VertexLike[ S, FullVertex[ S ]] {
      def source = this

      final val preHeadKey = new FullVertexPreHead[ S ]( this )
      final val preTailKey = new FullVertexPreTail[ S ]( this )

      def pre: FullPreOrder[ S ]
      def post: FullPostOrder[ S ]
      def preTail: FullPreOrder[ S ]

      final def write( out: DataOutput ) {
         out.writeInt( version )
         pre.write( out )
         post.write( out )
         preTail.write( out )
      }

      override def equals( that: Any ) : Boolean = {
         (that.isInstanceOf[ FullVertex[ _ ]] && (that.asInstanceOf[ FullVertex[ _ ]].version == version))
      }

//      final def x : Int = system.step { implicit tx => pre.tag }
//      final def y : Int = system.step { implicit tx => post.tag }
//      final def z : Int = version

      final def toPoint( implicit tx: S#Tx ) = IntPoint3D( pre.tag, post.tag, version )

      final def debugString( implicit tx: S#Tx ) = toString + "<post>@" + post.tag

      override def toString = "Full(" + version + ")"
   }

//   sealed trait FullRootVertex[ S <: Sys[ S ]] extends FullVertex[ S ] {
////      implicit def vertexSer : Serializer[ FullVertex[ S ]]
////      def preOrder  : TotalOrder.Map[ S, FullVertexPre[ S ]]
////      def postOrder : TotalOrder.Map[ S, FullVertex[ S ]]
//   }

   final class RelabelObserver[ S <: Sys[ S ], V <: VertexLike[ S, V ]]( name: String,
                                                                      t: SkipOctree[ S, IntSpace.ThreeDim, V ])
   extends TotalOrder.Map.RelabelObserver[ S#Tx, VertexSource[ S, V ]] {
      def beforeRelabeling( /* v0s: VertexSource[ V ], */ iter: Iterator[ S#Tx, VertexSource[ S, V ]])( implicit tx: S#Tx ) {
         if( verbose ) println( "RELABEL " + name + " - begin" )
//         val v0 = v0s.source
         iter.foreach { vs =>
            val v = vs.source
//            if( v ne v0 ) {
               if( verbose ) {
                  val str = try { vs.debugString } catch { case np: NullPointerException =>
                  "<null>"
                  }
                  println( "RELABEL " + name + " - " + str )
               }
//               assert( t.remove( v ), "When inserting " + v0 + ", the vertex " + v + " seems to have not been in the " + name + " tree" )

               // the nasty thing is, in the pre-order list the items appear twice
               // due to pre versus preTail. thus the items might get removed twice
               // here, too, and we cannot assert that t.remove( v ) == true
               t -= v
//            }
         }
         if( verbose ) println( "RELABEL " + name + " - end" )
      }
      def afterRelabeling( /* v0s: VertexSource[ V ], */ iter: Iterator[ S#Tx, VertexSource[ S, V ]])( implicit tx: S#Tx ) {
         if( verbose ) println( "RELABEL " + name + " + begin" )
//         val v0 = v0s.source
         iter.foreach { vs =>
            val v = vs.source
//            if( v ne v0 ) {
               if( verbose ) {
                  val str = try { vs.debugString } catch { case np: NullPointerException =>
                  "<null>"
                  }
                  println( "RELABEL " + name + " + " + str )
               }
//               assert( t.add( v ))

               // the nasty thing is, in the pre-order list the items appear twice
               // due to pre versus preTail. thus the items might get added twice
               // here, too, and we cannot assert that t.add( v ) == true
               t += v
//            }
         }
         if( verbose ) println( "RELABEL " + name + " + end" )
      }
   }

   type MarkOrder[ S <: Sys[ S ]] = TotalOrder.Map.Entry[ S, MarkVertex[ S ]]

//   object MarkVertex {
//      implicit def reader[ S <: Sys[ S ]] : Reader[ MarkVertex[ S ]] = new ReaderImpl[ S ]
//
//      private final class ReaderImpl[ S <: Sys[ S ]] extends Reader[ MarkVertex[ S ]] {
//         def read( in: DataInput ) : MarkVertex[ S ] = {
//
//         }
//      }
//   }
   sealed trait MarkVertex[ S <: Sys[ S ]] extends VertexLike[ S, MarkVertex[ S ]] {
      final def source = this

      def full: FullVertex[ S ]
      def pre: MarkOrder[ S ]
      def post: MarkOrder[ S ]

//      final def x : Int = system.step { implicit tx => pre.tag }
//      final def y : Int = system.step { implicit tx => post.tag }
      final def version : Int = full.version
//      final def z : Int = full.version

      final def write( out: DataOutput ) {
         full.write( out )
         pre.write( out )
         post.write( out )
      }

      final def toPoint( implicit tx: S#Tx ) = IntPoint3D( pre.tag, post.tag, version )

      override def toString = "Mark(" + version + ")"

      def debugString( implicit tx: S#Tx ) = toString

      override def equals( that: Any ) : Boolean = {
         (that.isInstanceOf[ MarkVertex[ _ ]] && (that.asInstanceOf[ MarkVertex[ _ ]].version == version))
      }
   }

   sealed trait MarkRootVertex[ S <: Sys[ S ]] extends MarkVertex[ S ] {
      implicit def vertexSer : Serializer[ S#Tx, S#Acc, MarkVertex[ S ]]
      def preOrder: TotalOrder.Map[ S, MarkVertex[ S ]]
      def postOrder: TotalOrder.Map[ S, MarkVertex[ S ]]
   }

   object MarkTree {
      def apply[ S <: Sys[ S ]]( ft: FullTree[ S ])( implicit tx: S#Tx, system: S ) : MarkTree[ S ] = {
         implicit val pointView = (p: MarkVertex[ S ], tx: S#Tx) => p.toPoint( tx )
         lazy val orderObserver = new RelabelObserver[ S, MarkVertex[ S ]]( "mark", t )
         lazy val _vertexSer: Serializer[ S#Tx, S#Acc, MarkVertex[ S ]] = new Serializer[ S#Tx, S#Acc, MarkVertex[ S ]] {
            def write( v: MarkVertex[ S ], out: DataOutput ) { v.write( out )}

            def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : MarkVertex[ S ] = {
               new MarkVertex[ S ] {
                  val full    = ft.vertexSer.read( in, access )
                  val pre     = root.preOrder.readEntry( in, access )
                  val post    = root.postOrder.readEntry( in, access )
               }
            }
         }
         lazy val root: MarkRootVertex[ S ]  = new MarkRootVertex[ S ] {
            implicit val vertexSer = _vertexSer
            def full       = ft.root
            lazy val preOrder   = TotalOrder.Map.empty[ S, MarkVertex[ S ]]( orderObserver, _.pre )
            lazy val postOrder  = TotalOrder.Map.empty[ S, MarkVertex[ S ]]( orderObserver, _.post, rootTag = Int.MaxValue )
//            lazy val pre: MarkOrder[ S ]  = preOrder.root
//            lazy val post: MarkOrder[ S ] = postOrder.root.append( this )
            lazy val pre: MarkOrder[ S ]  = preOrder.root
            lazy val post: MarkOrder[ S ] = postOrder.root // postOrder.insertAfter( root, this )
         }
         lazy val t = {
            implicit val keySer = _vertexSer
            SkipOctree.empty[ S, IntSpace.ThreeDim, MarkVertex[ S ]]( ft.t.hyperCube )
         }
         t += root
         implicit val orderSer: Serializer[ S#Tx, S#Acc, TotalOrder.Map.Entry[ S, MarkVertex[ S ]]] =
            root.preOrder.EntrySerializer
//            Serializer.fromMutableReader( root.preOrder.EntrySerializer, system )

         lazy val preList   = {
            implicit val ord = new Ordering[ S#Tx, MarkVertex[ S ]] {
               def compare( a: MarkVertex[ S ], b: MarkVertex[ S ])( implicit tx: S#Tx ) : Int = a.pre.compare( b.pre )
            }
            implicit val keySer = _vertexSer
            val res = SkipList.Set.empty[ S, MarkVertex[ S ]]
            res.add( root )
            res
         }

         lazy val postList   = {
            implicit val ord = new Ordering[ S#Tx, MarkVertex[ S ]] {
               def compare( a: MarkVertex[ S ], b: MarkVertex[ S ])( implicit tx: S#Tx ) : Int = a.post.compare( b.post )
            }
            implicit val keySer = _vertexSer
            val res = SkipList.Set.empty[ S, MarkVertex[ S ]]
            res.add( root )
            res
         }
         val mt = new MarkTree( ft, t, root, preList, postList )
//         if( verbose ) mt.printInsertion( root )
         mt
      }
   }
   final class MarkTree[ S <: Sys[ S ]] private( val ft: FullTree[ S ],
                                                 val t: SkipOctree[ S, IntSpace.ThreeDim, MarkVertex[ S ]],
                                 val root: MarkRootVertex[ S ], val preList: SkipList.Set[ S, MarkVertex[ S ]],
                                 val postList: SkipList.Set[ S, MarkVertex[ S ]]) {
      type V = MarkVertex[ S ]

      def system = ft.system

      def printInsertion( vm: V )( implicit cursor: Cursor[ S ]) {
         val (mStr, fStr) = cursor.step { implicit tx => vm.toPoint -> vm.full.toPoint }
         println( "Mark ins. node " + mStr + " with full " + fStr )
      }
   }

   def withSys[ S <: Sys[ S ] with Cursor[ S ]]( sysName: String, sysCreator: () => S, sysCleanUp: (S, Boolean) => Unit ) {
      def randomlyFilledTree( n: Int )( implicit system: S ) = new {
         Given( "a randomly filled tree, corresponding node orders and their quadtree" )
         val (t, treeSeq, parents) = system.step { implicit tx =>
            val tr         = FullTree[ S ]()
            val rnd        = new util.Random( seed )
            var treeSeq    = Vec[ FullVertex[ S ]]( tr.root )
            var parents    = Map.empty[ FullVertex[ S ], FullVertex[ S ]]
            var children   = Map.empty[ FullVertex[ S ], Set[ FullVertex[ S ]]]

            for( i <- 1 to n ) {
   if( DEBUG_LAST && i == n ) {
      verbose = true
   }
   //            try {
                  val refIdx  = rnd.nextInt( i )
                  val ref     = treeSeq( refIdx )
                  val retro   = if( refIdx > 0 ) rnd.nextDouble() else 1.1 // no retro stuff with root!
                  if( retro <= RETRO_CHILD_PERCENTAGE ) {
   if( verbose ) println( "v" + i + " is retro child to " + refIdx )
                     val child = tr.insertRetroChild( ref /*, i */)
                     treeSeq :+= child
                     parents += child -> ref
                     val oldChildren = children.getOrElse( ref, Set.empty )
                     children += ref -> Set( child )  // only child (overwrite previous entries for parent)
                     oldChildren.foreach { c2 => parents += c2 -> child }  // update parent for old children
                     children += child -> oldChildren
                  } else if( retro <= (RETRO_CHILD_PERCENTAGE + RETRO_PARENT_PERCENTAGE) ) {
   if( verbose ) println( "v" + i + " is retro parent to " + refIdx )
                     val parent = tr.insertRetroParent( ref /*, i */)
                     treeSeq :+= parent
                     val oldParentO = parents.get( ref )
                     parents += ref -> parent   // overwrites previous entry
                     oldParentO.foreach( op => parents += parent -> op )
                     children += parent -> Set( ref )
                     oldParentO.foreach { oldParent =>
                        children += oldParent -> (children.getOrElse( oldParent, Set.empty) - ref + parent) // replace child
                     }
                  } else { // regular child
   if( verbose ) println( "v" + i + " is child to " + refIdx )
                     val child = tr.insertChild( ref /*, i */)
                     treeSeq :+= child
                     parents += child -> ref
                     children += ref -> (children.getOrElse( ref, Set.empty) + child)
                  }
   //if( verbose ) printPrePost( t, treeSeq )

   //            } catch {
   //               case e =>
   //                  println( "(for i = " + i + ")" )
   //                  throw e
   //            }

               if( HUNT_DOWN_ORDER_BUGS ) {
                  treeSeq.foreach { v =>
                     v.pre.validate( "pre " + i )
                     v.post.validate( "post " + i )
                     v.preTail.validate( "preTail " + i )
                  }
               }

            }
            (tr, treeSeq, parents)
         }
      }

   //   private def printPrePost( t: FullTree, treeSeq: IndexedSeq[ FullTree#Vertex ]) {
   //      println( " PRE ORDER: " + t.r.head.tagList.map( pre => treeSeq.find( _.pre.tag == pre )).collect({ case Some( v ) => v.version }).mkString( ", " ))
   //      println( "POST ORDER: " + t.postOrder.head.tagList.map( post => treeSeq.find( _.post.tag == post ).get.version ).mkString( ", " ))
   //   }

      def scenarioWithTime( name: String, descr: String )( body: => Unit ) {
         scenario( descr ) {
            val t1 = System.currentTimeMillis()
            body
            val t2 = System.currentTimeMillis()
            println( "For " + name + "(" + sysName + ") the tests took " + TestUtil.formatSeconds( (t2 - t1) * 0.001 ))
         }
      }

      if( PARENT_LOOKUP ) {
         feature( "Tree parent node lookup should be possible in a " + sysName + " octree representing pre-order, post-order and version" ) {
            info( "The vertices of a tree are represented by their positions" )
            info( "in the tree's pre- and post-order traversals (as total orders), plus an incremental version." )
            info( "NN search is possible with these orders representing" )
            info( "the x, y and z coordinates of an octree." )

            scenarioWithTime( "Parent-Lookup", "Verifying parent node lookup" ) {
               implicit val system  = sysCreator()
               var success = false
               try {
                  val gagaism = randomlyFilledTree( NUM1 )
                  import gagaism._
         //         val (t, treeSeq, parents) = randomlyFilledTree()
//                  t.validate()

                  // If a valid ancestor is defined by being left of the query in
                  // the pre-order, and right of the query in the post-order,
                  // and by having a version smaller than or equal to query version,
                  // Then, Given that the pre-order is horizontally stored,
                  // and the post-order is vertically stored, and the version is stored in the z-axis,
                  // we can express this by constraining the search to the orthant
                  // index binary 010 = 2. From the candidates we need
                  // to find the one that is closest in the pre- or post-order. This
                  // is expressed by a XY chebychev distance measure.
                  When( "each vertex is asked for its parent node through NN search in the quadtree" )
                  Then( "the results should be identical to an independently maintained map" )
                  val metric = IntDistanceMeasure3D.chebyshevXY.orthant( 2 )
                  // val metric = IntDistanceMeasure3D.vehsybehcXY.orthant( 2 )

         //         if( verbose ) printPrePost( t, treeSeq )

                  @tailrec def testChild( version: Int, child: FullVertex[ S ]) {
                     parents.get( child ) match {
                        case None =>

                        case Some( parent ) if( parent.version <= version ) =>
                           val found: Option[ FullVertex[ S ]] = system.step { implicit tx =>
                              val p0 = child.toPoint
         //                     val point = IntPoint3D( child.x - 1, child.y + 1, child.version ) // make sure we skip the child itself
                              val point = p0.copy( x = p0.x - 1, y = p0.y + 1 )
                              val f = t.t.nearestNeighborOption( point, metric )
                              f
                           }
                           assert( found == Some( parent ), "For child " + child + ", found " + found + " instead of " + parent )

                        case Some( parent ) =>
                           testChild( version, parent )   // skip too new retro versions
                     }
                  }

                  treeSeq.foreach { child => testChild( child.version, child )}
                  success = true

               } finally {
                  sysCleanUp( system, success )
               }
            }
         }
      }

      if( MARKED_ANCESTOR ) {
         feature( "Marked ancestor lookup should be possible through isomorphic mapping between two " + sysName + " octrees" ) {
            info( "Two trees are now maintained (as quadtrees with pre/post order coordinates)." )
            info( "One tree represents the full version tree, the other a subtree representing markers." )
            info( "Marked ancestor lookup is performed by translating a coordinate from the" )
            info( "full tree into the marker tree, followed by NN search." )

            scenarioWithTime( "Marked-Ancestors", "Verifying marked ancestor lookup" ) {
               implicit val system  = sysCreator()
               var success = false
               try {
                  Given( "a randomly filled tree, corresponding node orders and their octree" )
                  Given( "a random marking of a subset of the vertices" )

                  val gagaism = randomlyFilledTree( NUM2 )
                  import gagaism._
         if( DEBUG_LAST ) verbose = false
                  type V      = FullVertex[ S ]
                  val tm      = system.step { implicit tx => MarkTree( t )}
                  val rnd     = new util.Random( seed )

                  implicit val ord: Ordering[ S#Tx, MarkOrder[ S ]] = new Ordering[ S#Tx, MarkOrder[ S ]] {
                     def compare( a: MarkOrder[ S ], b: MarkOrder[ S ])( implicit tx: S#Tx ) : Int = {
                        val ta = a.tag
                        val tb = b.tag
                        if( ta < tb ) -1 else if( ta > tb ) 1 else 0
                     }
                  }

         //         val mPreList   = {
         //            import tm.orderSer
         //            implicit def s = tm.t.system
         //            tm.t.system.step { implicit tx =>
         //               val res = SkipList.empty[ S, tm.Order ]
         //               res.add( tm.preOrder.root )
         //               res
         //            }
         //         }
         //         val mPostList = {
         //            import tm.orderSer
         //            implicit def s = tm.t.system
         //            tm.t.system.step { implicit tx =>
         //               val res = SkipList.empty[ S, tm.Order ]
         //               res.add( tm.postOrder.root )
         //               res
         //            }
         //         }

         //         var preTagIsoMap     = Map( tm.preOrder.root -> t.root.pre )
         //         var postTagIsoMap    = Map( tm.postOrder.root -> t.root.post )
         //         var preTagValueMap   = Map( tm.preOrder.root -> 0 )
         //         var postTagValueMap  = Map( tm.postOrder.root -> 0 )
                  var markSet          = Set( 0 )

                  treeSeq.zipWithIndex.drop(1).foreach { case (child, i) =>
         if( DEBUG_LAST && i == NUM2 - 1 ) verbose = true
                     if( rnd.nextDouble() < MARKER_PERCENTAGE ) {
                        system.step { implicit tx =>
         if( verbose ) println( ":: mark insert for full " + child.toPoint )
                           val cfPre = child.pre
                           val (cmPreN, cmPreCmp) = tm.preList.isomorphicQuery( new Ordered[ S#Tx, MarkVertex[ S ]] {
                              def compare( that: MarkVertex[ S ])( implicit tx: S#Tx ) : Int = {
                                 val res = cfPre.compare( that.full.pre )
         if( verbose ) println( ":: mark insert pre :: compare to m=" + that.toPoint + ", f=" + that.full.toPoint + " -> " + res )
                                 res
                              }
                           })
                           val cfPost = child.post
                           val (cmPostN, cmPostCmp ) = tm.postList.isomorphicQuery(  new Ordered[ S#Tx, MarkVertex[ S ]] {
                              def compare( that: MarkVertex[ S ])( implicit tx: S#Tx ) : Int = {
                                 val res = cfPost.compare( that.full.post )
         if( verbose ) println( ":: mark insert post :: compare to m=" + that.toPoint + ", f=" + that.full.toPoint + " -> " + res )
                                 res
                              }
                           })
         if( verbose ) println( ":: mark insert pre " + (if( cmPreCmp <= 0 ) "before" else "after") + " " + cmPreN.toPoint )
         if( verbose ) println( ":: mark insert post " + (if( cmPostCmp <= 0 ) "before" else "after") + " " + cmPostN.toPoint )
                           val vm = new MarkVertex[ S ] {
                              val pre  = tm.root.preOrder.insert()
                              val post = tm.root.postOrder.insert()
                              val full = child
                              if( cmPreCmp  <= 0 ) {
                                 tm.root.preOrder.placeBefore( cmPreN, this ) // cmPreN.prepend(  this )
                              } else {
                                 tm.root.preOrder.placeAfter( cmPreN, this ) // cmPreN.append(  this )
                              }
                              if( cmPostCmp <= 0 ) {
                                 tm.root.postOrder.placeBefore( cmPostN, this ) // cmPostN.prepend( this )
                              } else {
                                 tm.root.postOrder.placeAfter( cmPostN, this ) // cmPostN.append( this )
                              }
                           }

         if( verbose ) tm.printInsertion( vm )

                           tm.t.add( vm )
                           tm.preList.add( vm )
                           tm.postList.add( vm )
                           markSet += i
                        }
                     }
                  }

                  When( "full and marked tree are decomposed into pre and post order traversals" )

                  val preVals    = system.step { implicit tx => treeSeq.sortBy( _.pre.tag ).map( _.version )}
                  val postVals   = system.step { implicit tx => treeSeq.sortBy( _.post.tag ).map( _.version )}
                  val mPreSeq    = system.step { implicit tx => tm.preList.toIndexedSeq }
                  val mPreVals   = mPreSeq.map( _.version ) // ( t => t.value.version preTagValueMap( t ))
                  val mPostSeq   = system.step { implicit tx => tm.postList.toIndexedSeq }
                  val mPostVals  = mPostSeq.map( _.version ) // ( t => postTagValueMap( t ))

                  if( PRINT_ORDERS ) {
                     println( preVals.mkString( " pre full: ", ", ", "" ))
                     println( postVals.mkString( "post full: ", ", ", "" ))
                     println( mPreVals.mkString( " pre mark: ", ", ", "" ))
                     println( mPostVals.mkString( "post mark: ", ", ", "" ))
                  }

                  Then( "the order of the marked vertices is isomorphic to their counterparts in the full lists" )
                  assert( preVals.intersect( mPreVals ) == mPreVals, preVals.take( 20 ).toString + " versus " + mPreVals.take( 20 ))
                  assert( postVals.intersect( mPostVals ) == mPostVals, postVals.take( 20 ).toString + " versus " + mPreVals.take( 20 ))

                  if( PRINT_DOT ) {
                     val sb = new StringBuilder()
                     sb.append( "digraph Tree {\n" )
                     treeSeq.foreach { v =>
                        val id = v.version
                        sb.append( "  " + id.toString )
                        sb.append( "\n" )
                     }
                     parents.foreach { case (child, parent) =>
                        sb.append( "  " + parent.version.toString + " -> " + child.version.toString + "\n" )
                     }
                     sb.append( "}\n" )
                     println( sb.toString() )
                  }

                  When( "each vertex is asked for its nearest marked ancestor through mapping to the marked quadtree and NN search" )
                  Then( "the results should be identical to those obtained from independent brute force" )

         //println( "\n-----TREE-----" ); treeSeq.foreach( println )
         //println( "\n-----MARK-----" ); markSet.foreach( println )
         //println( "\n-----PARE-----" ); parents.foreach( println )
         //println()
         //verbose = false
                  if( VERIFY_MARKTREE_CONTENTS ) {
                     val markPt  = system.step { implicit tx => tm.t.toIndexedSeq.map( _.toPoint )}
                     val obsPre  = markPt.sortBy( _.x ).map( _.z )
                     val obsPost = markPt.sortBy( _.y ).map( _.z )
         //            println( "managed mark-pre:" )
         //            println( mPreVals.mkString( ", " ))
         //            println( "octree mark-pre:" )
         //            println( obsPre.mkString( ", " ))
                     assert( obsPre  == mPreVals )
                     assert( obsPost == mPostVals )
                  }

                  val metric = IntDistanceMeasure3D.chebyshevXY.orthant( 2 )
                  // val metric = IntDistanceMeasure3D.vehsybehcXY.orthant( 2 )

         if( DEBUG_LAST ) verbose = false
                  treeSeq.zipWithIndex.foreach { case (child, i) =>
         if( DEBUG_LAST && i == NUM2 - 1 ) verbose = true
                     val (found, parent, point) = system.step { implicit tx =>
                        val cfPre = child.pre
                        val (preIso, preIsoCmp) = tm.preList.isomorphicQuery( new Ordered[ S#Tx, MarkVertex[ S ]] {
                           def compare( that: MarkVertex[ S ])( implicit tx: S#Tx ) : Int = {
                              val res = cfPre.compare( that.full.pre )
         if( verbose ) println( ":: mark find pre :: compare to m=" + that.toPoint + ", f=" + that.full.toPoint + " -> " + res )
                              res
         //                  preTagIsoMap.get( that ).map( _.compare( child.pre )).getOrElse( 1 )
                           }
                        })
                        val cfPost = child.post
                        val (postIso, postIsoCmp) = tm.postList.isomorphicQuery( new Ordered[ S#Tx, MarkVertex[ S ]] {
                           def compare( that: MarkVertex[ S ])( implicit tx: S#Tx ) : Int = {
                              val res = cfPost.compare( that.full.post )
         if( verbose ) println( ":: mark find post :: compare to m=" + that.toPoint + ", f=" + that.full.toPoint + " -> " + res )
                              res
         //                  postTagIsoMap.get( that ).map( _.compare( child.post )).getOrElse( 1 )
                           }
                        })

                        // condition for ancestor candidates: <= iso-pre, >= iso-post, <= version
                        // thus: we need to check the comparison result of the iso-search
                        // - if the pre-comp is -1, we need to make the iso-mapped x (pre) one smaller
                        // - if the post-comp is 1, we need to make the iso-mapped y (post) one larger
                        // - if the version-comp is -1, we need to make iso-mapped z (version) one smaller
                        // (although, we can skip the last two steps, as the false positive is already ruled out by step 1)
                        // (there is no iso-mapping for the version)
                        //
                        // We can also shortcut. pre-comp == 0 implies post-comp == 0, since no two
                        // vertices can have the same positions in the orders.
                        // Thus, When pre-comp == 0 is detected, we already found our ancestor!

         if( verbose ) println( ":: mark find pre " + (if( preIsoCmp <= 0 ) "before" else "after") + " " + preIso.toPoint )
         if( verbose ) println( ":: mark find post " + (if( postIsoCmp <= 0 ) "before" else "after") + " " + postIso.toPoint )

                        val par = {
                           var p = child; while( p.version > child.version || !markSet.contains( p.version )) { p = parents( p )}
                           p.version
                        }
                        if( preIsoCmp == 0 ) {
                           assert( postIsoCmp == 0 )
                           val _pnt = IntPoint3D( preIso.pre.tag, postIso.post.tag, child.version )
                           val _f   = Some( preIso.version )
                           (_f, par, _pnt)
                        } else {
                           val x       = if( preIsoCmp < 0 )  preIso.pre.tag - 1 else preIso.pre.tag
                           val y       = if( postIsoCmp > 0 ) postIso.post.tag + 1 else postIso.post.tag
                           val _pnt    = IntPoint3D( x, y, child.version )
                           val _f      = tm.t.nearestNeighborOption( _pnt, metric ).map( _.version )
                           (_f, par, _pnt)
                        }
                     }
                     assert( found == Some( parent ), "For child " + child + "(iso " + point + "), found " + found.orNull + " instead of " + parent )
                  }
                  success = true

               } finally {
                  sysCleanUp( system, success )
               }
            }
         }
      }
   }
}