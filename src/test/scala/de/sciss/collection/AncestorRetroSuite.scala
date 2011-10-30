package de.sciss.collection

import geom.{Cube, Point3DLike, Space, DistanceMeasure2D, Point2D, Square, Point2DLike}
import mutable.{RandomizedSkipOctree, DeterministicSkipOctree, DeterministicSkipQuadtree, LLSkipList, RandomizedSkipQuadtree, TotalOrder}
import org.scalatest.{GivenWhenThen, FeatureSpec}

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.AncestorRetroSuite
 * }}
 */
class AncestorRetroSuite extends FeatureSpec with GivenWhenThen {
   def seed : Long         = 12345L
   val TREE_SIZE           = 100000    // 150000
   val MARKER_PERCENTAGE   = 0.2       // 0.5
   val PRINT_DOT           = false     // true
   val PRINT_ORDERS        = false
   val USE_DET             = true      // `true` to use deterministic octree, `false` to use randomized tree

   abstract class AbstractTree[ A ]( _init: A ) {
      type V <: VertexLike

      var verbose = false

      private var versionCnt = 0
      final def nextVersion() : Int = {
         val res = versionCnt
         versionCnt += 1
         res
      }

      final protected val preObserver    = new OrderObserver
      final protected val postObserver   = new OrderObserver
      final val preOrder   = TotalOrder( preObserver )
      final val postOrder  = TotalOrder( postObserver )
      final val root       = newVertex( _init, preOrder.root, postOrder.root, nextVersion() )
      final val cube       = Cube( 0x40000000, 0x40000000, 0x40000000, 0x40000000 )
      final val t          = if( USE_DET ) {
         DeterministicSkipOctree.empty[ Space.ThreeDim, V ]( Space.ThreeDim, cube )
      } else {
         RandomizedSkipOctree.empty[ Space.ThreeDim, V ]( Space.ThreeDim, cube, coin = RandomizedSkipOctree.Coin( 67890L ))
      }

      add( root )

      def newVertex( value: A, pre: preOrder.Entry, post: postOrder.Entry, version: Int ) : V

      trait VertexLike extends Point3DLike {
         def value: A
         def version: Int
         def pre: preOrder.Entry
         def post: postOrder.Entry
         def x: Int = pre.tag
         def y: Int = post.tag
         def z: Int = version
         override def toString = "Vertex(" + value + ", " + x + ", " + y + ", " + z + ")"
      }

      final protected class OrderObserver extends TotalOrder.RelabelObserver {
         // XXX could eventually add again elem to total order
         // (would be Option[ V ] for pre order and V for post order)
         var map = Map.empty[ TotalOrder.EntryLike, V ]
         def beforeRelabeling( first: TotalOrder.EntryLike, num: Int ) {
            var e = first
            var i = 0; while( i < num ) {
               map.get( e ).foreach( t.remove( _ ))
               e = e.next
               i += 1
            }
         }
         def afterRelabeling( first: TotalOrder.EntryLike, num: Int ) {
            var e = first
            var i = 0; while( i < num ) {
               map.get( e ).foreach( t.add( _ ))
               e = e.next
               i += 1
            }
         }
      }

      final protected def add( v: V ) : V = {
         preObserver.map  += v.pre -> v
         postObserver.map += v.post -> v
         t += v
         v
      }
   }

   final class FullTree[ A ]( _init: A ) extends AbstractTree( _init ) {
      type V = Vertex

      final class Vertex( val value: A, val pre: preOrder.Entry, val post: postOrder.Entry, val version: Int )
      extends VertexLike {
         val preTail = pre.append() // insertAfter( () )
      }

      def newVertex( value: A, pre: preOrder.Entry, post: postOrder.Entry, version: Int ) : V =
         new Vertex( value, pre, post, version )

      def insertChild( parent: Vertex, value : A ) : Vertex = {
         val cPre    = parent.preTail.prepend() // insertBefore( () )
         val cPost   = parent.post.prepend() // insertBefore( () )

if( verbose ) println( "insertChild( parent = " + parent.value + ", child = " + value + " ; pre compare = " +
   parent.pre.compare( cPre ) + "; post compare = " + parent.post.compare( cPost ))

         add( newVertex( value, cPre, cPost, nextVersion() ))
      }

      def validate() {
         when( "the size of the vertices is queried from the quadtree" )
         then( "it should be equal to the number of observed labelings and relabelings" )
         val qsz = t.size
         assert( qsz == preObserver.map.size,
            "pre-observer size (" + preObserver.map.size + ") is different from quad size (" + qsz + ")" )
         assert( qsz == postObserver.map.size,
            "post-observer size (" + postObserver.map.size + ") is different from quad size (" + qsz + ")" )
      }
   }

   final class MarkTree[ A ]( _init: A ) extends AbstractTree( _init ) {
      type V = Vertex

      final class Vertex( val value: A, val pre: preOrder.Entry, val post: postOrder.Entry, val version: Int )
      extends VertexLike

      def newVertex( value: A, pre: preOrder.Entry, post: postOrder.Entry, version: Int ) : V =
         new Vertex( value, pre, post, version )

      override def add( v: V ) : V = super.add( v )
   }

//   type V = Tree[ Unit ]#Vertex // [ Unit ]

   private def randomlyFilledTree( n: Int = TREE_SIZE ) = new {
      given( "a randomly filled tree, corresponding node orders and their quadtree" )
      val t       = new FullTree( () )
      val (treeSeq, parents) = {
         val rnd     = new util.Random( seed )
         var treeSeq = IndexedSeq( t.root )
         var parents = Map.empty[ t.Vertex, t.Vertex ]

         for( i <- 1 to n ) {
            try {
               val parent  = treeSeq( rnd.nextInt( i ))
               val child   = t.insertChild( parent, () )
               treeSeq :+= child
               parents += child -> parent
            } catch {
               case e =>
                  println( "(for i = " + i + ")" )
                  throw e
            }
         }
         (treeSeq, parents)
      }
   }

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

         // ancestor: left in pre-order, right in post-order
         // (thus, when pre-order is horizontally stored,
         // and post-order vertically, the quadrant is
         // is south west (2))
         when( "each vertex is asked for its parent node through NN search in the quadtree" )
         then( "the results should be identical to an independently maintained map" )
         val metric = DistanceMeasure2D.chebyshev.orthant( 2 )
         treeSeq.foreach { child => parents.get( child ).foreach { parent =>
            val point = Point2D( child.x - 1, child.y + 1 ) // make sure we skip the child itself
            val found = t.t.nearestNeighborOption( point, metric )
            assert( found == Some( parent ), "For child " + child + ", found " + found + " instead of " + parent )
         }}
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

         val t       = new FullTree( 0 )
         type V = t.Vertex
         val tm      = new MarkTree( 0 )
         val rnd     = new util.Random( seed )
         var treeSeq = IndexedSeq[ V ]( t.root )
         var parents    = Map.empty[ V, V ]
         val mPreList   = {
            implicit val m    = MaxKey( tm.preOrder.max )
            val res = LLSkipList.empty[ tm.preOrder.Entry ] // ( ord, m )
            res.add( tm.preOrder.root )
            res
         }
         val mPostList = {
            implicit def m    = MaxKey( tm.postOrder.max )
            val res = LLSkipList.empty[ tm.postOrder.Entry ] // ( ord, m )
            res.add( tm.postOrder.root )
            res
         }

         var preTagIsoMap     = Map( tm.preOrder.root -> t.root.pre )
         var postTagIsoMap    = Map( tm.postOrder.root -> t.root.post )
         var preTagValueMap   = Map( tm.preOrder.root -> 0 )
         var postTagValueMap  = Map( tm.postOrder.root -> 0 )
         var markSet          = Set( 0 )

         for( i <- 1 to TREE_SIZE ) {
            try {
               val parent  = treeSeq( rnd.nextInt( i ))
               val child   = t.insertChild( parent, i )
               treeSeq :+= child
               parents += child -> parent
               if( rnd.nextDouble() < MARKER_PERCENTAGE ) {

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
                  tm.add( new tm.Vertex( i, cmPre, cmPost ))
                  markSet += i
               }
            } catch {
               case e =>
                  println( "(for i = " + i + ")" )
                  throw e
            }
         }

         when( "full and marked tree are decomposed into pre and post order traversals" )

         val preVals    = treeSeq.sortBy( _.pre ).map( _.value )
         val postVals   = treeSeq.sortBy( _.post ).map( _.value )
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
               val id = v.value
               sb.append( "  " + id.toString )
               sb.append( "\n" )
            }
            parents.foreach { case (child, parent) =>
               sb.append( "  " + parent.value.toString + " -> " + child.value.toString + "\n" )
            }
            sb.append( "}\n" )
            println( sb.toString() )
         }

         when( "each vertex is asked for its nearest marked ancestor through mapping to the marked quadtree and NN search" )
         then( "the results should be identical to those obtained from independent brute force" )

         val metric = DistanceMeasure2D.chebyshev.orthant( 2 )
         treeSeq.foreach { child =>
            val preIso  = mPreList.isomorphicQuery  { e => preTagIsoMap.get(  e ).map( _.compare( child.pre  )).getOrElse( 1 )}
            val postIso = mPostList.isomorphicQuery { e => postTagIsoMap.get( e ).map( _.compare( child.post )).getOrElse( 1 )}
            val atPreIso= preTagIsoMap.get( preIso )
            val x       = if( atPreIso == Some( child.pre )) preIso.tag else preIso.tag - 1
            val y       = postIso.tag
            val point   = Point2D( x, y )

            val found = tm.t.nearestNeighborOption( point, metric ).map( _.value )
            val parent = {
               var p = child; while( !markSet.contains( p.value )) { p = parents( p )}
               p.value
            }
            assert( found == Some( parent ), "For child " + child + "(iso " + point + "), found " + found.orNull + " instead of " + parent )
         }
      }
   }
}