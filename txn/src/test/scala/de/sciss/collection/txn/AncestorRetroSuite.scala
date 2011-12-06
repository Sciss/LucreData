package de.sciss.collection
package txn

import org.scalatest.{GivenWhenThen, FeatureSpec}
import annotation.tailrec
import geom.{Point3D, DistanceMeasure3D, Cube, Space}
import concurrent.stm.Ref
import de.sciss.lucrestm.{DataInput, DataOutput, Serializer, InMemory}

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.txn.AncestorRetroSuite
 * }}
 */
class AncestorRetroSuite extends FeatureSpec with GivenWhenThen {
   def seed : Long            = 12345L
   val TREE_SIZE              = 10 // 100000    // 150000
   val MARKER_PERCENTAGE      = 0.2       // 0.5
   val RETRO_CHILD_PERCENTAGE = 0.1
   val RETRO_PARENT_PERCENTAGE= 0.1
   val PRINT_DOT              = false     // true
   val PRINT_ORDERS           = false

   var verbose = false

   type S = InMemory

   implicit val system = new InMemory

   type FullOrder = TotalOrder.Map.Entry[ S, FullVertex ]

   sealed trait FullVertex {
      def pre: FullOrder
      def post: FullOrder
      def preTail: FullOrder
      def version: Int

//      final def x : Int = system.atomic { implicit tx => pre.tag }
//      final def y : Int = system.atomic { implicit tx => post.tag }
//      final def z : Int = version

      final def toPoint( implicit tx: S#Tx ) = Point3D( pre.tag, post.tag, version )

      override def toString = "FullVertex(" + version + ")"
   }

   final class FullTree /* extends AbstractTree[ FullTree ] */ {
      private type V = FullVertex

      val cube       = Cube( 0x40000000, 0x40000000, 0x40000000, 0x40000000 )

      private val versionCnt = Ref( 0 )
      def nextVersion()( implicit tx: S#Tx ) : Int = {
         val res = versionCnt.get
         versionCnt.set( res + 1 )
         res
      }

//      final class Vertex( val pre: Order, val preTail: Order, val post: Order, val version: Int )
//      extends VertexLike {
////         val preTail = pre.append() // insertAfter( () )
//      }

//      lazy val root = newVertex( _init, preOrder.root, preOrder.root.append(), postOrder.root, nextVersion() )
      def root : V = Root

      implicit val vertexSer = new Serializer[ V ] {
         def write( v: FullVertex, out: DataOutput ) {
            sys.error( "TODO" )
         }

         def read( in: DataInput ) : FullVertex = {
            sys.error( "TODO" )
         }
      }

      val t: SkipOctree[ S, Space.ThreeDim, V ] = system.atomic { implicit tx =>
         import SpaceSerializers.CubeSerializer
         implicit val pointView = (p: V, tx: S#Tx) => p.toPoint( tx )
         SkipOctree.empty[ S, Space.ThreeDim, V ]( cube )
      }

      t.system.atomic { implicit tx => t += root }

      implicit val orderSer: Serializer[ TotalOrder.Map.Entry[ S, V ]] =
         Serializer.fromMutableReader( Root.preO.EntryReader, system )

      private object Root extends FullVertex {
         val preO     = t.system.atomic { implicit tx => TotalOrder.Map.empty[ S, V ]( this, OrderObserver )}
         val postO    = t.system.atomic { implicit tx => TotalOrder.Map.empty[ S, V ]( this, OrderObserver )}
         def pre: FullOrder       = preO.root
//         def post: FullOrder      = postO.root
def post: FullOrder      = t.system.atomic { implicit tx => postO.root.append( this )}  // XXX
         def preTail: FullOrder   = t.system.atomic { implicit tx => pre.append( this )}

         val version = 0
      }

      def preOrder: TotalOrder.Map[ S, V ]   = Root.preO
      def postOrder: TotalOrder.Map[ S, V ]  = Root.postO

//      def newVertex( value: A, pre: Order, preTail: Order, post: Order, version: Int ) : V =
//         new Vertex( value, pre, preTail, post, version )

      def insertChild( parent: V )( implicit tx: S#Tx ) : V = new FullVertex {
         val pre     = parent.preTail.prepend( this ) // insertBefore( () )
         val preTail = pre.append( this )
         val post    = parent.post.prepend( this ) // insertBefore( () )
         val version = nextVersion()

//         if( verbose ) println( "insertChild( parent = " + parent.value + ", child = " + value + " ; pre compare = " +
//            parent.pre.compare( pre ) + "; post compare = " + parent.post.compare( post ))

println( "INSERT " + system.atomic( toPoint( _ )))
         t.add( this )
      }

      def insertRetroChild( parent: V )( implicit tx: S#Tx ) : V = new FullVertex {
         val pre     = parent.pre.append( this )
         val preTail = parent.preTail.prepend( this )
         val post    = parent.post.prepend( this )
         val version = nextVersion()

         t.add( this )
      }

      def insertRetroParent( child: V )( implicit tx: S#Tx ) : V = new FullVertex {
         val pre     = child.pre.prepend( this )
         val preTail = child.preTail.append( this )
         val post    = child.post.append( this )
         val version = nextVersion()

         t.add( this )
      }

//      private def addNewVertex( value: A, pre: Order, preTail: Order, post: Order )
//                              ( implicit tx: S#Tx ) : Vertex = {
//         add( newVertex( value, pre, preTail, post, nextVersion() ))
//      }

      def validate() {
//         when( "the size of the vertices is queried from the quadtree" )
//         then( "it should be equal to the number of observed labelings and relabelings" )
//         val qsz = t.system.atomic { implicit tx => t.size }
//         assert( qsz == preObserver.map.size,
//            "pre-observer size (" + preObserver.map.size + ") is different from quad size (" + qsz + ")" )
//         assert( qsz == postObserver.map.size,
//            "post-observer size (" + postObserver.map.size + ") is different from quad size (" + qsz + ")" )
      }

      private object OrderObserver extends TotalOrder.Map.RelabelObserver[ S#Tx, V ] {
         type E = TotalOrder.Map.Entry[ S, V ]
//         // XXX could eventually add again elem to total order
//         // (would be Option[ V ] for pre order and V for post order)
//         var map = Map.empty[ TotalOrder.EntryLike, V ]
         def beforeRelabeling( iter: Iterator[ S#Tx, V ])( implicit tx: S#Tx ) {
            iter.foreach( t -= _ )
//            var e = first
//            var i = 0; while( i < num ) {
//               t.remove( e.value )
//               e = e.next.orNull
//               i += 1
//            }
         }
         def afterRelabeling( iter: Iterator[ S#Tx, V ])( implicit tx: S#Tx ) {
            iter.foreach( t += _ )
//            var e = first
//            var i = 0; while( i < num ) {
//               t.add( e.value )
//               e = e.next.orNull
//               i += 1
//            }
         }
      }
   }

   type MarkOrder = TotalOrder.Map.Entry[ S, MarkVertex ]

   sealed trait MarkVertex {
      def full: FullVertex
      def pre: MarkOrder
      def post: MarkOrder

//      final def x : Int = system.atomic { implicit tx => pre.tag }
//      final def y : Int = system.atomic { implicit tx => post.tag }
      final def version : Int = full.version
//      final def z : Int = full.version

      final def toPoint( implicit tx: S#Tx ) = Point3D( pre.tag, post.tag, version )

      override def toString = "MarkVertex(" + version + ")"
   }

   final class MarkTree( val ft: FullTree ) /* extends AbstractTree[ MarkTree ] */ {
      type V = MarkVertex

//      lazy val root = newVertex( _init, preOrder.root, postOrder.root, nextVersion() )

      implicit val vertexSer = new Serializer[ V ] {
         def write( v: MarkVertex, out: DataOutput ) {
            sys.error( "TODO" )
         }

         def read( in: DataInput ) : MarkVertex = {
            sys.error( "TODO" )
         }
      }

      implicit lazy val orderSer: Serializer[ TotalOrder.Map.Entry[ S, V ]] =
         Serializer.fromMutableReader( Root.preO.EntryReader, system )

      val t: SkipOctree[ S, Space.ThreeDim, V ] = system.atomic { implicit tx =>
         import SpaceSerializers.CubeSerializer
         implicit val pointView = (p: V, tx: S#Tx) => p.toPoint( tx )
         SkipOctree.empty[ S, Space.ThreeDim, V ]( ft.cube )
      }

      object Root extends MarkVertex {
         def full       = ft.root
         val preO       = t.system.atomic { implicit tx => TotalOrder.Map.empty[ S, V ]( this, OrderObserver )}
         val postO      = t.system.atomic { implicit tx => TotalOrder.Map.empty[ S, V ]( this, OrderObserver )}
//         def pre: MarkOrder       = preO.root
def pre: MarkOrder      = t.system.atomic { implicit tx => preO.root.append( this )}  // XXX
//         def post: MarkOrder      = postO.root
def post: MarkOrder      = t.system.atomic { implicit tx => postO.root.append( this )}  // XXX
      }

      def root = Root

      def preOrder: TotalOrder.Map[ S, V ]   = Root.preO
      def postOrder: TotalOrder.Map[ S, V ]  = Root.postO

      val preList   = {
         system.atomic { implicit tx =>
            implicit val ord = Ordering.fromOrdered[ S#Tx, MarkOrder ]
            val res = SkipList.empty[ S, MarkOrder ]
            res.add( Root.pre )
            res
         }
      }

      val postList   = {
         system.atomic { implicit tx =>
            implicit val ord = Ordering.fromOrdered[ S#Tx, MarkOrder ]
            val res = SkipList.empty[ S, MarkOrder ]
            res.add( Root.post )
            res
         }
      }

//      def newVertex( pre: Order, post: Order, version: Int ) : V = new Vertex( pre, post, version )

//      override def add( v: V )( implicit tx: S#Tx ) : V = super.add( v )

      private object OrderObserver extends TotalOrder.Map.RelabelObserver[ S#Tx, V ] {
         type E = TotalOrder.Map.Entry[ S, V ]
//         // XXX could eventually add again elem to total order
//         // (would be Option[ V ] for pre order and V for post order)
//         var map = Map.empty[ TotalOrder.EntryLike, V ]
         def beforeRelabeling( iter: Iterator[ S#Tx, V ])( implicit tx: S#Tx ) {
            iter.foreach( t -= _ )
//            var e = first
//            var i = 0; while( i < num ) {
//               t.remove( e.value )
//               e = e.next.orNull
//               i += 1
//            }
         }
         def afterRelabeling( iter: Iterator[ S#Tx, V ])( implicit tx: S#Tx ) {
            iter.foreach( t += _ )
//            var e = first
//            var i = 0; while( i < num ) {
//               t.add( e.value )
//               e = e.next.orNull
//               i += 1
//            }
         }
      }
   }

//   type V = Tree[ Unit ]#Vertex // [ Unit ]

   private def randomlyFilledTree( n: Int = TREE_SIZE ) = new {
      given( "a randomly filled tree, corresponding node orders and their quadtree" )
      val t       = new FullTree
      val (treeSeq, parents) = t.t.system.atomic { implicit tx =>
         val rnd        = new util.Random( seed )
         var treeSeq    = IndexedSeq( t.root )
         var parents    = Map.empty[ FullVertex, FullVertex ]
         var children   = Map.empty[ FullVertex, Set[ FullVertex ]]

         for( i <- 1 to n ) {
//            try {
               val refIdx  = rnd.nextInt( i )
               val ref     = treeSeq( refIdx )
               val retro   = rnd.nextDouble()
               if( retro <= RETRO_CHILD_PERCENTAGE ) {
if( verbose ) println( "v" + i + " is retro child to " + refIdx )
                  val child = t.insertRetroChild( ref /*, i */)
                  treeSeq :+= child
                  parents += child -> ref
                  val oldChildren = children.getOrElse( ref, Set.empty )
                  children += ref -> Set( child )  // only child (overwrite previous entries for parent)
                  oldChildren.foreach { c2 => parents += c2 -> child }  // update parent for old children
                  children += child -> oldChildren
               } else if( retro <= (RETRO_CHILD_PERCENTAGE + RETRO_PARENT_PERCENTAGE) ) {
if( verbose ) println( "v" + i + " is retro parent to " + refIdx )
                  val parent = t.insertRetroParent( ref /*, i */)
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
                  val child = t.insertChild( ref /*, i */)
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
         (treeSeq, parents)
      }
   }

//   private def printPrePost( t: FullTree, treeSeq: IndexedSeq[ FullTree#Vertex ]) {
//      println( " PRE ORDER: " + t.r.head.tagList.map( pre => treeSeq.find( _.pre.tag == pre )).collect({ case Some( v ) => v.version }).mkString( ", " ))
//      println( "POST ORDER: " + t.postOrder.head.tagList.map( post => treeSeq.find( _.post.tag == post ).get.version ).mkString( ", " ))
//   }

   feature( "Tree parent node lookup should be possible in a octree representing pre-order, post-order and version" ) {
      info( "The vertices of a tree are represented by their positions" )
      info( "in the tree's pre- and post-order traversals (as total orders), plus an incremental version." )
      info( "NN search is possible with these orders representing" )
      info( "the x, y and z coordinates of an octree." )

      scenario( "Verifying parent node lookup" ) {
         val gagaism = randomlyFilledTree()
         import gagaism._
//         val (t, treeSeq, parents) = randomlyFilledTree()
         t.validate()

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

         @tailrec def testChild( version: Int, child: FullVertex ) {
            parents.get( child ) match {
               case None =>

               case Some( parent ) if( parent.version <= version ) =>
                  val found: Option[ FullVertex ] = t.t.system.atomic { implicit tx =>
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
      }
   }

   feature( "Marked ancestor lookup should be possible through isomorphic mapping between two quadtrees" ) {
      info( "Two trees are now maintained (as quadtrees with pre/post order coordinates)." )
      info( "One tree represents the full version tree, the other a subtree representing markers." )
      info( "Marked ancestor lookup is performed by translating a coordinate from the" )
      info( "full tree into the marker tree, followed by NN search." )

      scenario( "Verifying marked ancestor lookup" ) {
         given( "a randomly filled tree, corresponding node orders and their quadtree" )
         given( "a random marking of a subset of the vertices" )

         val gagaism = randomlyFilledTree()
         import gagaism._

         type V      = FullVertex
         val tm      = new MarkTree( t )
         val rnd     = new util.Random( seed )

         implicit val ord: Ordering[ S#Tx, MarkOrder ] = new Ordering[ S#Tx, MarkOrder ] {
            def compare( a: MarkOrder, b: MarkOrder )( implicit tx: S#Tx ) : Int = {
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
            if( rnd.nextDouble() < MARKER_PERCENTAGE ) {
               tm.t.system.atomic { implicit tx =>
                  val (cmPreN, cmPreCmp) = tm.preList.isomorphicQuery( new Ordered[ S#Tx, MarkOrder ] {
                     def compare( that: MarkOrder )( implicit tx: S#Tx ) : Int = {
                        that.value.full.pre.compare( child.pre )
//                        preTagIsoMap.get( that ).map( _.compare( child.pre )).getOrElse( 1 )
                     }
                  })
                  val (cmPostN, cmPostCmp ) = tm.postList.isomorphicQuery(  new Ordered[ S#Tx, MarkOrder ] {
                     def compare( that: MarkOrder )( implicit tx: S#Tx ) : Int = {
                        that.value.full.post.compare( child.post )
//                        postTagIsoMap.get( that ).map( _.compare( child.post )).getOrElse( 1 )
                     }
                  })
//                  preTagIsoMap += cmPre -> child.pre
//                  postTagIsoMap += cmPost -> child.post
//                  preTagValueMap += cmPre -> i
//                  postTagValueMap += cmPost -> i
                  val vm = new MarkVertex {
                     val pre     = if( cmPreCmp  <= 0 ) cmPreN.prepend(  this ) else cmPreN.append(  this )
                     val post    = if( cmPostCmp <= 0 ) cmPostN.prepend( this ) else cmPostN.append( this )
                     val full    = child
//                     val version = child.version
                  }
                  tm.t.add( vm )
                  tm.preList.add( vm.pre )
                  tm.postList.add( vm.post )
                  markSet += i
               }
            }
         }

         when( "full and marked tree are decomposed into pre and post order traversals" )

         val preVals    = t.t.system.atomic { implicit tx => treeSeq.sortBy( _.pre.tag ).map( _.version )}
         val postVals   = t.t.system.atomic { implicit tx => treeSeq.sortBy( _.post.tag ).map( _.version )}
         val mPreSeq    = t.t.system.atomic { implicit tx => tm.preList.toIndexedSeq }
         val mPreVals   = mPreSeq.map( _.value.version ) // ( t => t.value.version preTagValueMap( t ))
         val mPostSeq   = t.t.system.atomic { implicit tx => tm.postList.toIndexedSeq }
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

         val metric = DistanceMeasure3D.chebyshevXY.orthant( 2 )
         treeSeq.foreach { child =>
//println( " -- testing " + child )
            val (found, parent, point) = system.atomic { implicit tx =>
               val (preIso, preIsoCmp) = tm.preList.isomorphicQuery( new Ordered[ S#Tx, MarkOrder ] {
                  def compare( that: MarkOrder )( implicit tx: S#Tx ) : Int = {
                     that.value.full.pre.compare( child.pre )
//                  preTagIsoMap.get( that ).map( _.compare( child.pre )).getOrElse( 1 )
                  }
               })
               val (postIso, postIsoCmp) = tm.postList.isomorphicQuery( new Ordered[ S#Tx, MarkOrder ] {
                  def compare( that: MarkOrder )( implicit tx: S#Tx ) : Int = {
                     that.value.full.post.compare( child.post )
//                  postTagIsoMap.get( that ).map( _.compare( child.post )).getOrElse( 1 )
                  }
               })
//               val atPreIso= preIso.value.full.pre //  preTagIsoMap.get( preIso )
//               if( atPreIso == Some( child.pre )) preIso.tag else preIso.tag - 1

               val x       = preIso.tag // + preIsoCmp
               val y       = postIso.tag
               val pnt     = Point3D( x, y, child.version )

               val f = tm.t.nearestNeighborOption( pnt, metric ).map( _.version )
               val par = {
                  var p = child; while( p.version > child.version || !markSet.contains( p.version )) { p = parents( p )}
                  p.version
               }
               (f, par, pnt)
            }
            assert( found == Some( parent ), "For child " + child + "(iso " + point + "), found " + found.orNull + " instead of " + parent )
         }
      }
   }
}