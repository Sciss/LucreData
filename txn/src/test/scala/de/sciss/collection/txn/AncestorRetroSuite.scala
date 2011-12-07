package de.sciss.collection
package txn

import org.scalatest.{GivenWhenThen, FeatureSpec}
import annotation.tailrec
import geom.{Point3D, DistanceMeasure3D, Cube, Space}
import concurrent.stm.Ref
import java.io.File
import de.sciss.lucrestm.{MutableReader, Mutable, Writer, BerkeleyDB, Sys, DataInput, DataOutput, Serializer, InMemory}

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.txn.AncestorRetroSuite
 * }}
 */
class AncestorRetroSuite extends FeatureSpec with GivenWhenThen {
   val PARENT_LOOKUP          = true
   val MARKED_ANCESTOR        = false // true
   val NUM1                   = 4 // 10000  // tree size in PARENT_LOOKUP
   val NUM2                   = 11000  // tree size in MARKED_ANCESTOR // 100000    // 150000
   val MARKER_PERCENTAGE      = 0.3 // 0.3       // 0.5 // percentage of elements marked (0 to 1)
   val RETRO_CHILD_PERCENTAGE = 0.1       // from those elements marked, amount which are inserted as retro-children (0 to 1)
   val RETRO_PARENT_PERCENTAGE= 0.1       // from those elements marked, amount which are inserted as retro-parents (0 to 1)

   val INMEMORY               = false // true
   val DATABASE               = true // false     // serializers not yet working

   val VERIFY_MARKTREE_CONTENTS = false   // be careful to not enable this with large TREE_SIZE (> some 1000)
   val PRINT_DOT              = false
   val PRINT_ORDERS           = false

   def seed : Long            = 0L

   var verbose                = false
   val DEBUG_LAST             = false  // if enabled, switches to verbosity for the last element in the sequence

   if( INMEMORY ) {
      withSys[ InMemory ]( "Mem", () => new InMemory, (_, _) => () )
   }
   if( DATABASE ) {
      withSys[ BerkeleyDB ]( "BDB", () => {
         val dir     = File.createTempFile( "ancestor", "_database" )
         dir.delete()
         dir.mkdir()
         val f       = new File( dir, "data" )
         println( f.getAbsolutePath )
         BerkeleyDB.open( f )
      }, { case (bdb, success) =>
         if( success ) {
            val sz = bdb.numUserRecords
            if( sz != 0 ) bdb.atomic( implicit tx => bdb.debugListUserRecords() ).foreach( println )
//            assert( sz == 0, "Final DB user size should be 0, but is " + sz )
         }
         bdb.close()
      })
   }

   object FullTree {
      def apply[ S <: Sys[ S ]]()( implicit tx: S#Tx, system: S, smf: Manifest[ S ]) : FullTree[ S ] = {
         import SpaceSerializers.CubeSerializer
         implicit val pointView = (p: FullVertex[ S ], tx: S#Tx) => p.toPoint( tx )
         val cube    = Cube( 0x40000000, 0x40000000, 0x40000000, 0x40000000 )
         lazy val orderObserver = new RelabelObserver[ S, FullVertex[ S ]]( "full", t )
         lazy val root: FullRootVertex[ S ] = new FullRootVertex[ S ] {
            me =>

            implicit val vertexSer : Serializer[ FullVertex[ S ]] = new Serializer[ FullVertex[ S ]] {
               def write( v: FullVertex[ S ], out: DataOutput ) { v.write( out )}

               def read( in: DataInput ) : FullVertex[ S ] = {
                  new FullVertex[ S ] {
                     val version = in.readInt()
//                     val pre     = preOrder.readEntry( in )
//                     val post    = postOrder.readEntry( in )
//                     val preTail = preOrder.readEntry( in )
                     // system.readMut[ FullOrders[ S ]]( in )
                     val orderRef = system.readRef[ FullOrders[ S ]]( in )
                  }
               }
            }

            lazy val preOrder            = TotalOrder.Map.empty[ S, FullVertex[ S ]]( this, orderObserver )
            lazy val postOrder           = TotalOrder.Map.empty[ S, FullVertex[ S ]]( this, orderObserver )
            lazy val orderRef = {
               implicit val orderSer = XXX
               system.newRef[ FullOrders[ S ]] { new FullOrders[ S ] {
                  val id = system.newID
                  val pre: FullOrder[ S ] = preOrder.root
                  //         def post: FullOrder      = postO.root
                  val post: FullOrder[ S ] = postOrder.root.append( me )  // XXX
                  val preTail: FullOrder[ S ] = pre.append( me )
               }}
            }
            val version = 0
            override def toString = "full-root"
         }
         lazy val t = {
            import root.vertexSer
            SkipOctree.empty[ S, Space.ThreeDim, FullVertex[ S ]]( cube )
         }
         t += root
         if( verbose ) println( "Full ins. root " + root.toPoint )
         new FullTree( t, root )
      }
   }
   final class FullTree[ S <: Sys[ S ]] private (
      val t: SkipOctree[ S, Space.ThreeDim, FullVertex[ S ]],
      val root: FullRootVertex[ S ]
   ) {
      private type V = FullVertex[ S ]

      private val versionCnt = Ref( 1 )
      def nextVersion()( implicit tx: S#Tx ) : Int = {
         val res = versionCnt.get
         versionCnt.set( res + 1 )
         res
      }

      def insertChild( parent: V )( implicit tx: S#Tx ) : V = {
         val v = new FullVertex[ S ] {
            me =>

            val orderRef = t.system.newRef[ FullOrders[ S ]] { new FullOrders[ S ] {
               val id      = t.system.newID()
               val pre     = parent.preTail.prepend( me ) // insertBefore( () )
               val post    = parent.post.prepend( me ) // insertBefore( () )
               val preTail = pre.append( me )
            }}
            val version = nextVersion()
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
            me =>

            val orderRef = t.system.newRef[ FullOrders[ S ]] { new FullOrders[ S ] {
               val id      = t.system.newID()
               val pre     = parent.pre.append( me )
               val post    = parent.post.prepend( me )
               val preTail = parent.preTail.prepend( me )
            }}
            val version = nextVersion()
            override def toString = super.toString + "@r-ch"
         }

         t.add( v )
         v
      }

      def insertRetroParent( child: V )( implicit tx: S#Tx ) : V = {
         require( child ne root )
         val v = new FullVertex[ S ] {
            me =>

            val orderRef = t.system.newRef[ FullOrders[ S ]] { new FullOrders[ S ] {
               val id      = t.system.newID()
               val pre     = child.pre.prepend( me )
               val post    = child.post.append( me )
               val preTail = child.preTail.append( me )
            }}
            val version = nextVersion()
            override def toString = super.toString + "@r-par"
         }

         t.add( v )
         v
      }

//      def validate() {
////         when( "the size of the vertices is queried from the quadtree" )
////         then( "it should be equal to the number of observed labelings and relabelings" )
////         val qsz = t.system.atomic { implicit tx => t.size }
////         assert( qsz == preObserver.map.size,
////            "pre-observer size (" + preObserver.map.size + ") is different from quad size (" + qsz + ")" )
////         assert( qsz == postObserver.map.size,
////            "post-observer size (" + postObserver.map.size + ") is different from quad size (" + qsz + ")" )
//      }
   }

   type FullOrder[ S <: Sys[ S ]] = TotalOrder.Map.Entry[ S, FullVertex[ S ]]

   sealed trait VertexLike[ S <: Sys[ S ]] extends Writer {
      def version : Int
      def toPoint( implicit tx: S#Tx ) : Point3D
   }

   object FullOrders {
      implicit def reader[ S <: Sys[ S ]]( implicit orderSer: Serializer[ FullOrder[ S ]]) : MutableReader[ S, FullOrders[ S ]] = new Reader[ S ]

      private final class Reader[ S <: Sys[ S ]]( implicit orderSer: Serializer[ FullOrder[ S ]])
      extends MutableReader[ S, FullOrders[ S ]] {
         def readData( in: DataInput, _id: S#ID ) : FullOrders[ S ] = new FullOrders[ S ] {
            val id      = _id
            val pre     = orderSer.read( in )
            val post    = orderSer.read( in )
            val preTail = orderSer.read( in )
         }
      }
   }
   sealed trait FullOrders[ S <: Sys[ S ]] extends Mutable[ S ] {
      def pre: FullOrder[ S ]
      def post: FullOrder[ S ]
      def preTail: FullOrder[ S ]

      final def writeData( out: DataOutput ) {
         pre.write( out )
         post.write( out )
         preTail.write( out )
      }

      final def disposeData()( implicit tx: S#Tx ) {
         pre.dispose()
         post.dispose()
         preTail.dispose()
      }
   }

   sealed trait FullVertex[ S <: Sys[ S ]] extends VertexLike[ S ] {
      def orderRef: S#Ref[ FullOrders[ S ]]

      final def pre( implicit tx: S#Tx ): FullOrder[ S ] = orderRef.get.pre
      final def post( implicit tx: S#Tx ): FullOrder[ S ] = orderRef.get.post
      final def preTail( implicit tx: S#Tx ): FullOrder[ S ] = orderRef.get.preTail

      final def write( out: DataOutput ) {
         out.writeInt( version )
         orderRef.write( out )
//         pre.write( out )
//         post.write( out )
//         preTail.write( out )
      }

//      final def x : Int = system.atomic { implicit tx => pre.tag }
//      final def y : Int = system.atomic { implicit tx => post.tag }
//      final def z : Int = version

      final def toPoint( implicit tx: S#Tx ) = Point3D( pre.tag, post.tag, version )

      override def toString = "FullVertex(" + version + ")"
   }

   sealed trait FullRootVertex[ S <: Sys[ S ]] extends FullVertex[ S ] {
      implicit def vertexSer : Serializer[ FullVertex[ S ]]
      def preOrder  : TotalOrder.Map[ S, FullVertex[ S ]]
      def postOrder : TotalOrder.Map[ S, FullVertex[ S ]]
   }

   final class RelabelObserver[ S <: Sys[ S ], V <: VertexLike[ S ]]( name: String,
                                                                      t: SkipOctree[ S, Space.ThreeDim, V ])
   extends TotalOrder.Map.RelabelObserver[ S#Tx, V ] {
      def beforeRelabeling( v0: V, iter: Iterator[ S#Tx, V ])( implicit tx: S#Tx ) {
         if( verbose ) println( "RELABEL " + name + " - begin" )
         iter.foreach { v =>
            if( v ne v0 ) {
               if( verbose ) {
                  val str = try { v.toPoint } catch { case np: NullPointerException =>
                  "<null>"
                  }
                  println( "RELABEL " + name + " - " + str )
               }
//               assert( t.remove( v ), "When inserting " + v0 + ", the vertex " + v + " seems to have not been in the " + name + " tree" )

               // the nasty thing is, in the pre-order list the items appear twice
               // due to pre versus preTail. thus the items might get removed twice
               // here, too, and we cannot assert that t.remove( v ) == true
               t -= v
            }
         }
         if( verbose ) println( "RELABEL " + name + " - end" )
      }
      def afterRelabeling( v0: V, iter: Iterator[ S#Tx, V ])( implicit tx: S#Tx ) {
         if( verbose ) println( "RELABEL " + name + " + begin" )
         iter.foreach { v =>
            if( v ne v0 ) {
               if( verbose ) {
                  val str = try { v.toPoint } catch { case np: NullPointerException =>
                  "<null>"
                  }
                  println( "RELABEL " + name + " + " + str )
               }
//               assert( t.add( v ))

               // the nasty thing is, in the pre-order list the items appear twice
               // due to pre versus preTail. thus the items might get added twice
               // here, too, and we cannot assert that t.add( v ) == true
               t += v
            }
         }
         if( verbose ) println( "RELABEL " + name + " + end" )
      }
   }

   type MarkOrder[ S <: Sys[ S ]] = TotalOrder.Map.Entry[ S, MarkVertex[ S ]]

   object MarkOrders {
      implicit def reader[ S <: Sys[ S ]]( implicit orderSer: Serializer[ MarkOrder[ S ]]) : MutableReader[ S, MarkOrders[ S ]] = new Reader[ S ]

      private final class Reader[ S <: Sys[ S ]]( implicit orderSer: Serializer[ MarkOrder[ S ]])
      extends MutableReader[ S, MarkOrders[ S ]] {
         def readData( in: DataInput, _id: S#ID ) : MarkOrders[ S ] = new MarkOrders[ S ] {
            val id      = _id
            val pre     = orderSer.read( in )
            val post    = orderSer.read( in )
         }
      }
   }
   sealed trait MarkOrders[ S <: Sys[ S ]] extends Mutable[ S ] {
      def pre: MarkOrder[ S ]
      def post: MarkOrder[ S ]

      final def writeData( out: DataOutput ) {
         pre.write( out )
         post.write( out )
      }

      final def disposeData()( implicit tx: S#Tx ) {
         pre.dispose()
         post.dispose()
      }
   }

   sealed trait MarkVertex[ S <: Sys[ S ]] extends VertexLike[ S ] {
      def full: FullVertex[ S ]

      def orderRef : S#Ref[ MarkOrders[ S ]]

      final def pre( implicit tx: S#Tx ) : MarkOrder[ S ] = orderRef.get.pre
      final def post( implicit tx: S#Tx ) : MarkOrder[ S ] = orderRef.get.post

//      final def x : Int = system.atomic { implicit tx => pre.tag }
//      final def y : Int = system.atomic { implicit tx => post.tag }
      final def version : Int = full.version
//      final def z : Int = full.version

      final def write( out: DataOutput ) {
         full.write( out )
//         pre.write( out )
//         post.write( out )
         orderRef.write( out )
      }

      final def toPoint( implicit tx: S#Tx ) = Point3D( pre.tag, post.tag, version )

      override def toString = "MarkVertex(" + version + ")"
   }

   sealed trait MarkRootVertex[ S <: Sys[ S ]] extends MarkVertex[ S ] {
      implicit def vertexSer : Serializer[ MarkVertex[ S ]]
      def preOrder: TotalOrder.Map[ S, MarkVertex[ S ]]
      def postOrder: TotalOrder.Map[ S, MarkVertex[ S ]]
   }

   object MarkTree {
      def apply[ S <: Sys[ S ]]( ft: FullTree[ S ])( implicit tx: S#Tx, system: S, smf: Manifest[ S ]) : MarkTree[ S ] = {
         import SpaceSerializers.CubeSerializer
         implicit val pointView = (p: MarkVertex[ S ], tx: S#Tx) => p.toPoint( tx )
         lazy val orderObserver = new RelabelObserver[ S, MarkVertex[ S ]]( "mark", t )
         lazy val root: MarkRootVertex[ S ]  = new MarkRootVertex[ S ] {
            me =>

            implicit val vertexSer: Serializer[ MarkVertex[ S ]] = new Serializer[ MarkVertex[ S ]] {
               def write( v: MarkVertex[ S ], out: DataOutput ) { v.write( out )}

               def read( in: DataInput ) : MarkVertex[ S ] = {
                  new MarkVertex[ S ] {
                     val full    = ft.root.vertexSer.read( in )
                     val orderRef = system.newRef[ MarkOrders[ S ]] { new MarkOrders[ S ] {
                        val id      = system.newID()
                        val pre     = preOrder.readEntry( in )
                        val post    = postOrder.readEntry( in )
                     }}
                  }
               }
            }

            def full       = ft.root
            lazy val preOrder   = TotalOrder.Map.empty[ S, MarkVertex[ S ]]( this, orderObserver )
            lazy val postOrder  = TotalOrder.Map.empty[ S, MarkVertex[ S ]]( this, orderObserver )
            lazy val orderRef = system.newRef[ MarkOrders[ S ]] { new MarkOrders[ S ] {
               val id                     = system.newID()
               val pre: MarkOrder[ S ]    = preOrder.root
               val post: MarkOrder[ S ]   = postOrder.root.append( me )
            }}
         }
         lazy val t = {
            import root.vertexSer
            SkipOctree.empty[ S, Space.ThreeDim, MarkVertex[ S ]]( ft.t.hyperCube )
         }
         t += root
         implicit val orderSer: Serializer[ TotalOrder.Map.Entry[ S, MarkVertex[ S ]]] =
            Serializer.fromMutableReader( root.preOrder.EntryReader, system )

         val preList   = {
            implicit val ord = Ordering.fromOrdered[ S#Tx, MarkOrder[ S ]]
            val res = SkipList.empty[ S, MarkOrder[ S ]]
            res.add( root.pre )
            res
         }

         val postList   = {
            implicit val ord = Ordering.fromOrdered[ S#Tx, MarkOrder[ S ]]
            val res = SkipList.empty[ S, MarkOrder[ S ]]
            res.add( root.post )
            res
         }
         val mt = new MarkTree( ft, t, root, preList, postList )
         if( verbose ) mt.printInsertion( root )
         mt
      }
   }
   final class MarkTree[ S <: Sys[ S ]] private( val ft: FullTree[ S ],
                                                 val t: SkipOctree[ S, Space.ThreeDim, MarkVertex[ S ]],
                                 val root: MarkRootVertex[ S ], val preList: SkipList[ S, MarkOrder[ S ]],
                                 val postList: SkipList[ S, MarkOrder[ S ]]) {
      type V = MarkVertex[ S ]

      def printInsertion( vm: V ) {
         val (mStr, fStr) = t.system.atomic { implicit tx => vm.toPoint -> vm.full.toPoint }
         println( "Mark ins. node " + mStr + " with full " + fStr )
      }
   }

   def withSys[ S <: Sys[ S ]]( sysName: String, sysCreator: () => S, sysCleanUp: (S, Boolean) => Unit )
                              ( implicit smf: Manifest[ S ]) {

      def randomlyFilledTree( n: Int )( implicit system: S ) = new {
         given( "a randomly filled tree, corresponding node orders and their quadtree" )
         val (t, treeSeq, parents) = system.atomic { implicit tx =>
            val tr         = FullTree[ S ]()
            val rnd        = new util.Random( seed )
            var treeSeq    = IndexedSeq[ FullVertex[ S ]]( tr.root )
            var parents    = Map.empty[ FullVertex[ S ], FullVertex[ S ]]
            var children   = Map.empty[ FullVertex[ S ], Set[ FullVertex[ S ]]]

            for( i <- 1 to n ) {
   if( DEBUG_LAST && i == n ) verbose = true
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
         feature( "Tree parent node lookup should be possible in a octree representing pre-order, post-order and version" ) {
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
                  // then, given that the pre-order is horizontally stored,
                  // and the post-order is vertically stored, and the version is stored in the z-axis,
                  // we can express this by constraining the search to the orthant
                  // index binary 010 = 2. From the candidates we need
                  // to find the one that is closest in the pre- or post-order. This
                  // is expressed by a XY chebychev distance measure.
                  when( "each vertex is asked for its parent node through NN search in the quadtree" )
                  then( "the results should be identical to an independently maintained map" )
                  val metric = DistanceMeasure3D.chebyshevXY.orthant( 2 )

         //         if( verbose ) printPrePost( t, treeSeq )

                  @tailrec def testChild( version: Int, child: FullVertex[ S ]) {
                     parents.get( child ) match {
                        case None =>

                        case Some( parent ) if( parent.version <= version ) =>
                           val found: Option[ FullVertex[ S ]] = t.t.system.atomic { implicit tx =>
                              val p0 = child.toPoint
         //                     val point = Point3D( child.x - 1, child.y + 1, child.version ) // make sure we skip the child itself
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
         feature( "Marked ancestor lookup should be possible through isomorphic mapping between two quadtrees" ) {
            info( "Two trees are now maintained (as quadtrees with pre/post order coordinates)." )
            info( "One tree represents the full version tree, the other a subtree representing markers." )
            info( "Marked ancestor lookup is performed by translating a coordinate from the" )
            info( "full tree into the marker tree, followed by NN search." )

            scenarioWithTime( "Marked-Ancestors", "Verifying marked ancestor lookup" ) {
               implicit val system  = sysCreator()
               var success = false
               try {
                  given( "a randomly filled tree, corresponding node orders and their quadtree" )
                  given( "a random marking of a subset of the vertices" )

                  val gagaism = randomlyFilledTree( NUM2 )
                  import gagaism._
         if( DEBUG_LAST ) verbose = false
                  type V      = FullVertex[ S ]
                  val tm      = system.atomic { implicit tx => MarkTree( t )}
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
         //            tm.t.system.atomic { implicit tx =>
         //               val res = SkipList.empty[ S, tm.Order ]
         //               res.add( tm.preOrder.root )
         //               res
         //            }
         //         }
         //         val mPostList = {
         //            import tm.orderSer
         //            implicit def s = tm.t.system
         //            tm.t.system.atomic { implicit tx =>
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
                        tm.t.system.atomic { implicit tx =>
         if( verbose ) println( ":: mark insert for full " + child.toPoint )
                           val cfPre = child.pre
                           val (cmPreN, cmPreCmp) = tm.preList.isomorphicQuery( new Ordered[ S#Tx, MarkOrder[ S ]] {
                              def compare( that: MarkOrder[ S ])( implicit tx: S#Tx ) : Int = {
                                 val res = cfPre.compare( that.value.full.pre )
         if( verbose ) println( ":: mark insert pre :: compare to m=" + that.value.toPoint + ", f=" + that.value.full.toPoint + " -> " + res )
                                 res
                              }
                           })
                           val cfPost = child.post
                           val (cmPostN, cmPostCmp ) = tm.postList.isomorphicQuery(  new Ordered[ S#Tx, MarkOrder[ S ]] {
                              def compare( that: MarkOrder[ S ])( implicit tx: S#Tx ) : Int = {
                                 val res = cfPost.compare( that.value.full.post )
         if( verbose ) println( ":: mark insert post :: compare to m=" + that.value.toPoint + ", f=" + that.value.full.toPoint + " -> " + res )
                                 res
                              }
                           })
         if( verbose ) println( ":: mark insert pre " + (if( cmPreCmp <= 0 ) "before" else "after") + " " + cmPreN.value.toPoint )
         if( verbose ) println( ":: mark insert post " + (if( cmPostCmp <= 0 ) "before" else "after") + " " + cmPostN.value.toPoint )
                           val vm = new MarkVertex[ S ] {
                              me =>

                              val full    = child
                              val orderRef = system.newRef[ MarkOrders[ S ]] { new MarkOrders[ S ] {
                                 val id      = system.newID()
                                 val pre     = if( cmPreCmp  <= 0 ) cmPreN.prepend(  me ) else cmPreN.append(  me )
                                 val post    = if( cmPostCmp <= 0 ) cmPostN.prepend( me ) else cmPostN.append( me )
                              }}
                           }

         if( verbose ) tm.printInsertion( vm )

                           tm.t.add( vm )
                           tm.preList.add( vm.pre )
                           tm.postList.add( vm.post )
                           markSet += i
                        }
                     }
                  }

                  when( "full and marked tree are decomposed into pre and post order traversals" )

                  val preVals    = system.atomic { implicit tx => treeSeq.sortBy( _.pre.tag ).map( _.version )}
                  val postVals   = system.atomic { implicit tx => treeSeq.sortBy( _.post.tag ).map( _.version )}
                  val mPreSeq    = system.atomic { implicit tx => tm.preList.toIndexedSeq }
                  val mPreVals   = mPreSeq.map( _.value.version ) // ( t => t.value.version preTagValueMap( t ))
                  val mPostSeq   = system.atomic { implicit tx => tm.postList.toIndexedSeq }
                  val mPostVals  = mPostSeq.map( _.value.version ) // ( t => postTagValueMap( t ))

                  if( PRINT_ORDERS ) {
                     println( preVals.mkString( " pre full: ", ", ", "" ))
                     println( postVals.mkString( "post full: ", ", ", "" ))
                     println( mPreVals.mkString( " pre mark: ", ", ", "" ))
                     println( mPostVals.mkString( "post mark: ", ", ", "" ))
                  }

                  then( "the order of the marked vertices is isomorphic to their counterparts in the full lists" )
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

                  when( "each vertex is asked for its nearest marked ancestor through mapping to the marked quadtree and NN search" )
                  then( "the results should be identical to those obtained from independent brute force" )

         //println( "\n-----TREE-----" ); treeSeq.foreach( println )
         //println( "\n-----MARK-----" ); markSet.foreach( println )
         //println( "\n-----PARE-----" ); parents.foreach( println )
         //println()
         //verbose = false
                  if( VERIFY_MARKTREE_CONTENTS ) {
                     val markPt  = system.atomic { implicit tx => tm.t.toIndexedSeq.map( _.toPoint )}
                     val obsPre  = markPt.sortBy( _.x ).map( _.z )
                     val obsPost = markPt.sortBy( _.y ).map( _.z )
         //            println( "managed mark-pre:" )
         //            println( mPreVals.mkString( ", " ))
         //            println( "octree mark-pre:" )
         //            println( obsPre.mkString( ", " ))
                     assert( obsPre  == mPreVals )
                     assert( obsPost == mPostVals )
                  }

                  val metric = DistanceMeasure3D.chebyshevXY.orthant( 2 )
                  treeSeq.zipWithIndex.foreach { case (child, i) =>
         if( DEBUG_LAST ) verbose = child.version == 91
                     val (found, parent, point) = system.atomic { implicit tx =>
                        val cfPre = child.pre
                        val (preIso, preIsoCmp) = tm.preList.isomorphicQuery( new Ordered[ S#Tx, MarkOrder[ S ]] {
                           def compare( that: MarkOrder[ S ])( implicit tx: S#Tx ) : Int = {
                              val res = cfPre.compare( that.value.full.pre )
         if( verbose ) println( ":: mark find pre :: compare to m=" + that.value.toPoint + ", f=" + that.value.full.toPoint + " -> " + res )
                              res
         //                  preTagIsoMap.get( that ).map( _.compare( child.pre )).getOrElse( 1 )
                           }
                        })
                        val cfPost = child.post
                        val (postIso, postIsoCmp) = tm.postList.isomorphicQuery( new Ordered[ S#Tx, MarkOrder[ S ]] {
                           def compare( that: MarkOrder[ S ])( implicit tx: S#Tx ) : Int = {
                              val res = cfPost.compare( that.value.full.post )
         if( verbose ) println( ":: mark find post :: compare to m=" + that.value.toPoint + ", f=" + that.value.full.toPoint + " -> " + res )
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
                        // Thus, when pre-comp == 0 is detected, we already found our ancestor!

         if( verbose ) println( ":: mark find pre " + (if( preIsoCmp <= 0 ) "before" else "after") + " " + preIso.value.toPoint )
         if( verbose ) println( ":: mark find post " + (if( postIsoCmp <= 0 ) "before" else "after") + " " + postIso.value.toPoint )

                        val par = {
                           var p = child; while( p.version > child.version || !markSet.contains( p.version )) { p = parents( p )}
                           p.version
                        }
                        if( preIsoCmp == 0 ) {
                           assert( postIsoCmp == 0 )
                           val _pnt = Point3D( preIso.tag, postIso.tag, child.version )
                           val _f   = Some( preIso.value.version )
                           (_f, par, _pnt)
                        } else {
                           val x       = if( preIsoCmp < 0 )  preIso.tag - 1 else preIso.tag
                           val y       = if( postIsoCmp > 0 ) postIso.tag + 1 else postIso.tag
                           val _pnt    = Point3D( x, y, child.version )
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