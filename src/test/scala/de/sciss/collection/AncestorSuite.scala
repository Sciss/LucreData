package de.sciss.collection

import org.scalatest.{GivenWhenThen, FeatureSpec}
import collection.immutable.IntMap

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.AncestorSuite
 * }}
 */
class AncestorSuite extends FeatureSpec with GivenWhenThen {
   def seed : Long         = System.currentTimeMillis()
   val TREE_SIZE           = 100000
   val MARKER_PERCENTAGE   = 0.2

   class Vertex[ A ]( val value: A, val pre: TotalOrder#Entry, val post: TotalOrder#Entry )
   extends PointLike {
      val preTail = pre.append() // insertAfter( () )

      def x: Int = pre.tag
      def y: Int = post.tag

      override def toString = "Vertex(" + value + ", " + x + ", " + y + ")"
   }

   class Tree[ A ]( _init: A ) {
      type V = Vertex[ A ]

      private val preObserver    = new OrderObserver
      private val postObserver   = new OrderObserver
      val root = new Vertex( _init, TotalOrder( preObserver ).root, TotalOrder( postObserver ).root )
      val quad = RandomizedSkipQuadTree.empty[ V ]( Quad( 0x40000000, 0x40000000, 0x40000000 ))

      add( root )

      private class OrderObserver extends TotalOrder.RelabelObserver {
         // XXX could eventually add again elem to total order
         // (would be Option[ V ] for pre order and V for post order)
         var map = Map.empty[ TotalOrder.EntryLike, V ]
         def beforeRelabeling( first: TotalOrder.EntryLike, num: Int ) {
            var e = first
            var i = 0; while( i < num ) {
               map.get( e ).foreach( quad.remove( _ ))
               e = e.next
               i += 1
            }
         }
         def afterRelabeling( first: TotalOrder.EntryLike, num: Int ) {
            var e = first
            var i = 0; while( i < num ) {
               map.get( e ).foreach( quad.add( _ ))
               e = e.next
               i += 1
            }
         }
      }

      private def add( v: V ) : V = {
         preObserver.map  += v.pre -> v
         postObserver.map += v.post -> v
         quad += v
         v
      }

      def insertChild( parent: V, value : A ) : V = {
         val cPre    = parent.preTail.prepend() // insertBefore( () )
         val cPost   = parent.post.prepend() // insertBefore( () )
         add( new Vertex( value, cPre, cPost ))
      }

      def validate() {
         when( "the size of the vertices is queried from the quadtree" )
         then( "it should be equal to the number of observed labelings and relabelings" )
         val qsz = quad.size
         assert( qsz == preObserver.map.size,
            "pre-observer size (" + preObserver.map.size + ") is different from quad size (" + qsz + ")" )
         assert( qsz == postObserver.map.size,
            "post-observer size (" + postObserver.map.size + ") is different from quad size (" + qsz + ")" )
      }
   }

   type V = Vertex[ Unit ]

   private def randomlyFilledTree( n: Int = TREE_SIZE ) : (Tree[ Unit ], IndexedSeq[ V ], Map[ V, V ]) = {
      given( "a randomly filled tree, corresponding node orders and their quadtree" )
      val t       = new Tree( () )
      val rnd     = new util.Random( seed )
      var treeSeq = IndexedSeq( t.root )
      var parents = Map.empty[ V, V ]

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
      (t, treeSeq, parents)
   }

   feature( "Tree parent node lookup should be possible in a quadtree representing pre- and post-order positions" ) {
      info( "The vertices of a tree are represented by their positions" )
      info( "in the tree's pre- and post-order traversals (as total orders)." )
      info( "NN search is possible with these orders representing" )
      info( "the x and y coordinates of a quadtree." )

      scenario( "Verifying parent node lookup" ) {
         val (t, treeSeq, parents) = randomlyFilledTree()
         t.validate()

         // ancestor: left in pre-order, right in post-order
         // (thus, when pre-order is horizontally stored,
         // and post-order vertically, the quadrant is
         // is south west (2))
         when( "each vertex is asked for its parent node through NN search in the quadtree" )
         then( "the results should be identical to an independently maintained map" )
         val metric = DistanceMeasure.chebyshev.quadrant( 2 )
         treeSeq.foreach { child => parents.get( child ).foreach { parent =>
            val point = Point( child.x - 1, child.y + 1 ) // make sure we skip the child itself
            val found = t.quad.nearestNeighborOption( point, metric )
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
         type V      = Vertex[ Int ]
         val t       = new Tree( 0 )
         val tm      = new Tree( 0 )
         val rnd     = new util.Random( seed )
         var treeSeq = IndexedSeq( t.root )
//         var markMap = IntMap( 0 -> t.root )  // root is 'fallback' always (as in the compressed path method)
         var parents    = Map.empty[ V, V ]
         var markMap  = Map( t.root -> tm.root )
         var markMap2 = Map( tm.root -> t.root )

         for( i <- 1 to TREE_SIZE ) {
            try {
               val parent  = treeSeq( rnd.nextInt( i ))
               val child   = t.insertChild( parent, i )
               treeSeq :+= child
               parents += child -> parent
               if( rnd.nextDouble() < MARKER_PERCENTAGE ) {
                  var p    = parent; while( !markMap.contains( p )) { p = parents( p )}
                  val pm   = markMap( p )
                  val cm   = tm.insertChild( pm, i )
                  markMap += child -> cm
                  markMap2 += cm -> child
               }
            } catch {
               case e =>
                  println( "(for i = " + i + ")" )
                  throw e
            }
         }

         when( "each vertex is asked for its nearest marked ancestor through mapping to the marked quadtree and NN search" )
         then( "the results should be identical to those obtained from independent brute force" )
         val metric = DistanceMeasure.chebyshev.quadrant( 2 )
         treeSeq.foreach { child =>
println( "testing... #" + child )
            val iso = tm.quad.isomorphicQuery { vm =>
               val v = markMap2( vm )
               val res = child.orient( v )
println( "...query (" + vm.x + ", " + vm.y + ") -> (" + v.x + ", " + v.y + ") -> " + res )
               res
            }
println( "iso-query yielded " + iso )
            val point = iso.topRight
            val found = tm.quad.nearestNeighborOption( point, metric )
            val parent = {
               var p = child; while( !markMap.contains( p )) { p = parents( p )}
               p
            }
            assert( found == Some( parent ), "For child " + child + ", found " + found + " instead of " + parent )
         }
      }
   }
}