package de.sciss.collection
package txn

import org.scalatest.{GivenWhenThen, FeatureSpec}
import annotation.tailrec
import geom.{Point3D, DistanceMeasure3D, Cube, Point3DLike, Space}
import de.sciss.lucrestm.{Serializer, InMemory}
import concurrent.stm.Ref

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.mutable.AncestorRetroSuite
 * }}
 */
class AncestorRetroSuite extends FeatureSpec with GivenWhenThen {
   def seed : Long            = 12345L
   val TREE_SIZE              = 100000    // 150000
   val MARKER_PERCENTAGE      = 0.2       // 0.5
   val RETRO_CHILD_PERCENTAGE = 0.1
   val RETRO_PARENT_PERCENTAGE= 0.1
   val PRINT_DOT              = false     // true
   val PRINT_ORDERS           = false

   var verbose = false

   type S = InMemory

   trait VertexLike extends Point3DLike /* with Writer */ {
      def version: Int
      def pre: TotalOrder.Map.Entry[ S, VertexLike ] // preOrder.Entry
      def post: TotalOrder.Map.Entry[ S, VertexLike ] // postOrder.Entry
      def x( implicit tx: S#Tx ): Int = pre.tag
      def y( implicit tx: S#Tx ): Int = post.tag
      def z: Int = version
      override def toString = "Vertex(" + version + ")"
   }

   abstract class AbstractTree[ Self <: AbstractTree[ Self ]] {
      type V <: VertexLike

      private val versionCnt = Ref( 0 )
      final def nextVersion()( implicit tx: S#Tx ) : Int = {
         val res = versionCnt.get
         versionCnt.set( res + 1 )
         res
      }

      def preOrder:  TotalOrder.Map[ S, Self#V ]
      def postOrder: TotalOrder.Map[ S, Self#V ]

      implicit val vertexSerializer : Serializer[ VertexLike ]
//      implicit val vertexManifest : Manifest[ V ]
//      implicit val valueManifest: Manifest[ A ]

      def root: Self#V

//      final val root       = newVertex( _init, preOrder.root, postOrder.root, nextVersion() )
      final val cube       = Cube( 0x40000000, 0x40000000, 0x40000000, 0x40000000 )

      implicit val inMem   = new InMemory()

//      final val preOrder   = t.system.atomic { implicit tx => TotalOrder.Map.empty[ S, V ]( preObserver )}
//      final val postOrder  = t.system.atomic { implicit tx => TotalOrder.Map.empty[ S, V ]( postObserver )}

      implicit def vertexSer: Serializer[ Self#V ]
      implicit def vertexMF: Manifest[ Self#V ]

      def t: SkipOctree[ S, Space.ThreeDim, Self#V ]

//      final val t: SkipOctree[ S, Space.ThreeDim, Self#V ] = inMem.atomic { implicit tx =>
//         import SpaceSerializers.CubeSerializer
//         SkipOctree.empty[ S, Space.ThreeDim, Self#V ]( cube )
//      }
//
//      t.system.atomic { implicit tx => t += root }

//      def newVertex( value: A, pre: preOrder.Entry, post: postOrder.Entry, version: Int ) : V

//      protected def add( v: V )( implicit tx: S#Tx ) : V = {
//         preObserver.map  += v.pre -> v
//         postObserver.map += v.post -> v
//         t += v
//         v
//      }
   }

   final class FullTree extends AbstractTree[ FullTree ] {
      type V = Vertex

      type Order = TotalOrder.Map.Entry[ S, V ]

      sealed trait Vertex extends VertexLike {
         def preTail: Order
      }
//      final class Vertex( val pre: Order, val preTail: Order, val post: Order, val version: Int )
//      extends VertexLike {
////         val preTail = pre.append() // insertAfter( () )
//      }

//      lazy val root = newVertex( _init, preOrder.root, preOrder.root.append(), postOrder.root, nextVersion() )
      def root : V = Root

      implicit val vertexSer = new Serializer[ V ] {

      }

      val t: SkipOctree[ S, Space.ThreeDim, V ] = inMem.atomic { implicit tx =>
         import SpaceSerializers.CubeSerializer
         SkipOctree.empty[ S, Space.ThreeDim, V ]( cube )
      }

      t.system.atomic { implicit tx => t += root }

      private object Root extends V {
         val preO     = t.system.atomic { implicit tx => TotalOrder.Map.empty[ S, V ]( this, OrderObserver )}
         val postO    = t.system.atomic { implicit tx => TotalOrder.Map.empty[ S, V ]( this, OrderObserver )}
         def pre: Order       = preO.root
         def post: Order      = postO.root
         def preTail: Order   = t.system.atomic { implicit tx => pre.append( this )}
      }

      def preOrder: TotalOrder.Map[ S, V ]   = Root.preO
      def postOrder: TotalOrder.Map[ S, V ]  = Root.postO

//      def newVertex( value: A, pre: Order, preTail: Order, post: Order, version: Int ) : V =
//         new Vertex( value, pre, preTail, post, version )

      def insertChild( parent: V )( implicit tx: S#Tx ) : V = new Vertex {
         val pre     = parent.preTail.prepend( this ) // insertBefore( () )
         val preTail = pre.append( this )
         val post    = parent.post.prepend( this ) // insertBefore( () )
         val version = nextVersion()

//         if( verbose ) println( "insertChild( parent = " + parent.value + ", child = " + value + " ; pre compare = " +
//            parent.pre.compare( pre ) + "; post compare = " + parent.post.compare( post ))

         t.add( this )
      }

      def insertRetroChild( parent: V )( implicit tx: S#Tx ) : V = new Vertex {
         val pre     = parent.pre.append( this )
         val preTail = parent.preTail.prepend( this )
         val post    = parent.post.prepend( this )
         val version = nextVersion()

         t.add( this )
      }

      def insertRetroParent( child: V )( implicit tx: S#Tx ) : V = new Vertex {
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
         def beforeRelabeling( first: E, num: Int )( implicit tx: S#Tx ) {
            var e = first
            var i = 0; while( i < num ) {
               t.remove( e.value )
               e = e.next.orNull
               i += 1
            }
         }
         def afterRelabeling( first: E, num: Int )( implicit tx: S#Tx ) {
            var e = first
            var i = 0; while( i < num ) {
               t.add( e.value )
               e = e.next.orNull
               i += 1
            }
         }
      }
   }

   final class MarkTree extends AbstractTree[ MarkTree ] {
      type V = Vertex

      type Order = TotalOrder.Map.Entry[ S, V ]

      sealed trait Vertex extends VertexLike

//      lazy val root = newVertex( _init, preOrder.root, postOrder.root, nextVersion() )

      implicit val vertexSer = new Serializer[ V ] {

      }

      lazy val root : V = new Vertex {
         private val preO     = t.system.atomic { implicit tx => TotalOrder.Map.empty[ S, V ]( this, OrderObserver )}
         private val postO    = t.system.atomic { implicit tx => TotalOrder.Map.empty[ S, V ]( this, OrderObserver )}
         def pre: Order       = preO.root
         def post: Order      = postO.root
      }

//      def newVertex( pre: Order, post: Order, version: Int ) : V = new Vertex( pre, post, version )

//      override def add( v: V )( implicit tx: S#Tx ) : V = super.add( v )

      private object OrderObserver extends TotalOrder.Map.RelabelObserver[ S#Tx, V ] {
         type E = TotalOrder.Map.Entry[ S, V ]
//         // XXX could eventually add again elem to total order
//         // (would be Option[ V ] for pre order and V for post order)
//         var map = Map.empty[ TotalOrder.EntryLike, V ]
         def beforeRelabeling( first: E, num: Int )( implicit tx: S#Tx ) {
            var e = first
            var i = 0; while( i < num ) {
               t.remove( e.value )
               e = e.next.orNull
               i += 1
            }
         }
         def afterRelabeling( first: E, num: Int )( implicit tx: S#Tx ) {
            var e = first
            var i = 0; while( i < num ) {
               t.add( e.value )
               e = e.next.orNull
               i += 1
            }
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
         var parents    = Map.empty[ t.V, t.V ]
         var children   = Map.empty[ t.V, Set[ t.V ]]

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

         @tailrec def testChild( version: Int, child: t.V ) {
            parents.get( child ) match {
               case None =>

               case Some( parent ) if( parent.version <= version ) =>
                  val found: Option[t.V] = t.t.system.atomic { implicit tx =>
                     val point = Point3D( child.x - 1, child.y + 1, child.version ) // make sure we skip the child itself
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

         type V      = t.Vertex
         val tm      = new MarkTree
         val rnd     = new util.Random( seed )

         implicit val ord: Ordering[ S#Tx, tm.Order ] = new Ordering[ S#Tx, tm.Order ] {
            def compare( a: tm.Order, b: tm.Order )( implicit tx: S#Tx ) : Int = {
               val ta = a.tag
               val tb = b.tag
               if( ta < tb ) -1 else if( ta > tb ) 1 else 0
            }
         }

         val mPreList   = {
            val res = tm.t.system.atomic { implicit tx => SkipList.empty[ S, tm.Order ]}
            res.add( tm.preOrder.root )
            res
         }
         val mPostList = {
            val res = tm.t.system.atomic { implicit tx => SkipList.empty[ S, tm.Order ]}
            res.add( tm.postOrder.root )
            res
         }

         var preTagIsoMap     = Map( tm.preOrder.root -> t.root.pre )
         var postTagIsoMap    = Map( tm.postOrder.root -> t.root.post )
         var preTagValueMap   = Map( tm.preOrder.root -> 0 )
         var postTagValueMap  = Map( tm.postOrder.root -> 0 )
         var markSet          = Set( 0 )

         treeSeq.zipWithIndex.drop(1).foreach { case (child, i) =>
            if( rnd.nextDouble() < MARKER_PERCENTAGE ) {
               tm.t.system.atomic { implicit tx =>
                  val cmPreSucc = mPreList.isomorphicQuery( preTagIsoMap.get( _ ).map( _.compare( child.pre )).getOrElse( 1 ))
                  val cmPre = cmPreSucc.prepend()
                  mPreList.add( cmPre )
                  val cmPostSucc = mPostList.isomorphicQuery( postTagIsoMap.get( _ ).map( _.compare( child.post )).getOrElse( 1 ))
                  val cmPost = cmPostSucc.prepend()
                  mPostList.add( cmPost )
                  preTagIsoMap += cmPre -> child.pre
                  postTagIsoMap += cmPost -> child.post
                  preTagValueMap += cmPre -> i
                  postTagValueMap += cmPost -> i
//                  tm.add( new tm.Vertex( i, cmPre, cmPost, child.version ))
//                  tm.t.add( new tm.Vertex( /* i, */ cmPre, cmPost, child.version ) {} )
                  tm.t.add( new tm.Vertex {
                     val pre = cmPre
                     val post = cmPost
                     val version = child.version
                  })
                  markSet += i
               }
            }
         }

         when( "full and marked tree are decomposed into pre and post order traversals" )

         val preVals    = t.t.system.atomic { implicit tx => treeSeq.sortBy( _.pre.tag ).map( _.version )}
         val postVals   = t.t.system.atomic { implicit tx => treeSeq.sortBy( _.post.tag ).map( _.version )}
         val mPreSeq    = mPreList.toIndexedSeq
         val mPreVals   = mPreSeq.map( t => preTagValueMap( t ))
         val mPostSeq   = mPostList.toIndexedSeq
         val mPostVals  = mPostSeq.map( t => postTagValueMap( t ))

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

         val metric = DistanceMeasure3D.chebyshevXY.orthant( 2 ) // XXX THIS IS WRONG ???
         treeSeq.foreach { child =>
//println( " -- testing " + child )
            val preIso  = mPreList.isomorphicQuery  { e => preTagIsoMap.get(  e ).map( _.compare( child.pre  )).getOrElse( 1 )}
            val postIso = mPostList.isomorphicQuery { e => postTagIsoMap.get( e ).map( _.compare( child.post )).getOrElse( 1 )}
            val atPreIso= preTagIsoMap.get( preIso )
            val x       = if( atPreIso == Some( child.pre )) preIso.tag else preIso.tag - 1
            val y       = postIso.tag
            val point   = Point3D( x, y, child.version )

            val found = tm.t.system.atomic { implicit tx => tm.t.nearestNeighborOption( point, metric ).map( _.version )}
            val parent = {
               var p = child; while( p.version > child.version || !markSet.contains( p.version )) { p = parents( p )}
               p.version
            }
            assert( found == Some( parent ), "For child " + child + "(iso " + point + "), found " + found.orNull + " instead of " + parent )
         }
      }
   }
}