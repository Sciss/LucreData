package de.sciss.collection
package mutable

import geom.{IntSpace, DistanceMeasure2D, IntPoint2D, IntSquare}
import org.scalatest.{FeatureSpec, GivenWhenThen}
import collection.breakOut
import collection.mutable.{Set => MSet}
import IntSpace.TwoDim

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.QuadtreeSuite
 * }}
 */
class QuadtreeSuite extends FeatureSpec with GivenWhenThen {
   val RANDOMIZED    = true
   val DETERMINISTIC = true
   val RANGE_SEARCH  = true
   val NN_SEARCH     = true
   val REMOVAL       = true

   val n             = 0x1000    // tree size ;  0xE0    // 0x4000 is the maximum acceptable speed
   val n2            = n >> 3    // 0x1000    // range query and nn

   val RND_SEED      = 43210L

   val rnd           = new util.Random()

//   val hyperCube = IntSquare( 0x20000000, 0x20000000, 0x20000000 )
   val quad          = IntSquare( 0x40000000, 0x40000000, 0x40000000 )
   if( RANDOMIZED ) {
      rnd.setSeed( RND_SEED )
      withTree( "randomized", RandomizedSkipQuadtree.empty[ TwoDim#Point ]( quad,
         coin = RandomizedSkipOctree.Coin( 98765L )))
   }
   if( DETERMINISTIC ) {
      rnd.setSeed( RND_SEED )
      withTree( "deterministic", DeterministicSkipQuadtree.empty[ TwoDim#Point ]( quad ))
   }

   private type SkipQuadtree[ A ] = SkipOctree[ TwoDim, A ]

   def randFill( t: SkipQuadtree[ TwoDim#Point ], m: MSet[ TwoDim#Point ]) {
      given( "a randomly filled structure" )

      // seed = 2
//      val n     = 0x4000 // 0x10000 // 0x467 // 0x467 // 0x2F80
      for( i <- 0 until n ) {
//         val k = IntPoint2D( rnd.nextInt( 0x40000000 ),
//                        rnd.nextInt( 0x40000000 ))
         val k = IntPoint2D( rnd.nextInt() & 0x7FFFFFFF,
                             rnd.nextInt() & 0x7FFFFFFF )
//         val v = rnd.nextInt()
//println( i.toString + " - " + k )
         t += k // .put( k, v )
         m += k // .put( k, v )
      }
   }

   def verifyConsistency( t: SkipQuadtree[ TwoDim#Point ]) {
      when( "the internals of the structure are checked" )
      then( "they should be consistent with the underlying algorithm" )
      val q = t.hyperCube
      var h = t.lastTree
      var currUnlinkedQuads   = Set.empty[ TwoDim#HyperCubeLike ]
      var currPoints          = Set.empty[ TwoDim#Point ]
      var prevs = 0
      do {
         assert( h.hyperCube == q, "Root level quad is " + h.hyperCube + " while it should be " + q + " in level n - " + prevs )
         val nextUnlinkedQuads   = currUnlinkedQuads
         val nextPoints          = currPoints
         currUnlinkedQuads       = Set.empty
         currPoints              = Set.empty
         def checkChildren( n: t.QNode, depth: Int ) {
            def assertInfo = " in level n-" + prevs + " / depth " + depth

            var i = 0; while( i < 4 ) {
               n.child( i ) match {
                  case c: t.QNode =>
                     val nq = n.hyperCube.orthant( i )
                     val cq = c.hyperCube
                     assert( nq.contains( cq ), "Node has invalid quad (" + cq + "), expected: " + nq + assertInfo )
                     c.nextOption match {
                        case Some( next ) =>
                           assert( next.prevOption == Some( c ), "Asymmetric next link " + cq + assertInfo )
                           assert( next.hyperCube == cq, "Next quad does not match (" + cq + " vs. " + next.hyperCube + ")" + assertInfo )
                        case None =>
                           assert( !nextUnlinkedQuads.contains( cq ), "Double missing link for " + cq + assertInfo )
                     }
                     c.prevOption match {
                        case Some( prev ) =>
                           assert( prev.nextOption == Some( c ), "Asymmetric prev link " + cq + assertInfo )
                           assert( prev.hyperCube == cq, "Next quad do not match (" + cq + " vs. " + prev.hyperCube + ")" + assertInfo )
                        case None => currUnlinkedQuads += cq
                     }
                     checkChildren( c, depth + 1 )
                  case l: t.QLeaf =>
                     currPoints += l.value
                  case _: t.QEmpty =>
               }
            i += 1 }
         }
         checkChildren( h, 0 )
         val pointsOnlyInNext    = nextPoints.filterNot( currPoints.contains( _ ))
         assert( pointsOnlyInNext.isEmpty, "Points in next which aren't in current (" + pointsOnlyInNext.take( 10 ) + "); in level n-" + prevs )
         h                       = h.prevOption.orNull
         prevs += 1
      } while( h != null )
   }

   def verifyElems( t: SkipQuadtree[ TwoDim#Point ], m: MSet[ TwoDim#Point ]) {
      when( "the structure t is compared to an independently maintained map m" )
      val onlyInM  = m.filterNot { e =>
//         println( "Contains ? " + e._1 )
         t.contains( e )
      }
      val onlyInT  = t.filterNot( e => m.contains( e ))
      val szT      = t.size
      val szM      = m.size
      then( "all elements of m should be contained in t" )
      assert( onlyInM.isEmpty, onlyInM.take( 10 ).toString() )
      then( "all elements of t should be contained in m" )
      assert( onlyInT.isEmpty, onlyInT.take( 10 ).toString() )
      then( "both should report the same size" )
      assert( szT == szM, "quadtree has size " + szT + " / map has size " + szM )
   }

   def verifyContainsNot( t: SkipQuadtree[ TwoDim#Point ], m: MSet[ TwoDim#Point ]) {
      when( "the structure t is queried for keys not in the independently maintained map m" )
      var testSet = Set.empty[ TwoDim#Point ]
      while( testSet.size < 100 ) {
         val x = IntPoint2D( rnd.nextInt(), rnd.nextInt() )
         if( !m.contains( x )) testSet += x
      }
      val inT = testSet.filter { p =>
//println( "testin " + p )
         t.contains( p )
      }
      then( "none of them should be contained in t" )
      assert( inT.isEmpty, inT.take( 10 ).toString() )
   }

   def verifyAddRemoveAll( t: SkipQuadtree[ TwoDim#Point ], m: MSet[ TwoDim#Point ]) {
      when( "all elements of the independently maintained map are added again to t" )
      val szBefore = t.size
      val newInT   = m.filter( e => t.update( e ).isEmpty )
      val szAfter  = t.size
      then( "all of the put operations should return 'Some'" )
      assert( newInT.isEmpty, newInT.take( 10 ).toString() )
      then( "the size of t should not change" )
      assert( szBefore == szAfter, "t had size " + szBefore + " before, but now reports " + szAfter )

      when( "all elements of the independently maintained map are removed from t" )
      val keptInT  = m.filter( e => t.removeAt( e ).isEmpty )
      val szAfter2 = t.size
      then( "all of the remove operations should return 'Some'" )
      assert( keptInT.isEmpty, keptInT.take( 10 ).toString() )
      then( "the size of t should be zero" )
      assert( szAfter2 == 0, szAfter2.toString )
   }

   def verifyRangeSearch( t: SkipQuadtree[ TwoDim#Point ], m: MSet[ TwoDim#Point ]) {
      when( "the quadtree is range searched" )
      val qs = Seq.fill( n2 )( IntSquare( rnd.nextInt( 0x7FFFFFFF ) - 0x40000000,
                                          rnd.nextInt( 0x7FFFFFFF ) - 0x40000000, rnd.nextInt( 0x40000000 )))
      val rangesT = qs.map( q => t.rangeQuery( q ).toSet )
      val ks      = m // keySet
      val rangesM = qs.map( q => ks.filter( q.contains( _ )))
      then( "the results should match brute force with the corresponding set" )
      rangesT.zip(rangesM).foreach { case (s1, s2) =>
         assert( s1 == s2, s1.toList.sortBy( p => (p.x, p.y) ).take( 10 ).toString + " -- " +
                           s2.toList.sortBy( p => (p.x, p.y) ).take( 10 ))
      }
   }

   def verifyNN( t: SkipQuadtree[ TwoDim#Point ], m: MSet[ TwoDim#Point ]) {
      when( "the quadtree is searched for nearest neighbours" )
      val ps0 = Seq.fill( n2 )( IntPoint2D( rnd.nextInt(), rnd.nextInt() ))
      // tricky: this guarantees that there are no 63 bit overflows,
      // while still allowing points outside the root hyperCube to enter the test
      val ps = ps0.filter( p => {
         val dx = if( p.x < quad.cx ) quad.right.toLong - p.x else p.x - quad.left
         val dy = if( p.y < quad.cy ) quad.bottom.toLong - p.y else p.y - quad.top
         dx <= 0xB504F300L && dy <= 0xB504F300L && dx * dx + dy * dy > 0L
      })
      val nnT: Map[ TwoDim#Point, TwoDim#Point ] = ps.map( p => p -> t.nearestNeighbor( p, DistanceMeasure2D.euclideanSq ))( breakOut )
      val ks   = m // .keySet
      val nnM: Map[ TwoDim#Point, TwoDim#Point ] = ps.map( p => p -> ks.minBy( _.distanceSq( p )))( breakOut )
      then( "the results should match brute force with the corresponding set" )
      assert( nnT == nnM, {
         (nnT.collect { case (q, v) if( nnM( q ) != v ) => (q, v, nnM( q ))}).take( 10 ).toString()
//         nnT.take( 10 ).toString + " -- " + nnM.take( 10 )
      })
   }

   def withTree( name: String, tf: => SkipQuadtree[ TwoDim#Point ]) {
      feature( "The " + name + " quadtree structure should be consistent" ) {
         info( "Several mass operations on the structure" )
         info( "are tried and expected behaviour verified" )

         scenario( "Consistency is verified on a randomly filled structure" ) {
            val time1 = System.currentTimeMillis()
            val t  = tf // ( None )
            val m  = MSet.empty[ TwoDim#Point ]
            randFill( t, m )

            verifyConsistency( t )
            verifyElems( t, m )
            verifyContainsNot( t, m )
            if( RANGE_SEARCH ) verifyRangeSearch( t, m )
            if( NN_SEARCH ) verifyNN( t, m )
            if( REMOVAL ) verifyAddRemoveAll( t, m )

            val time2 = System.currentTimeMillis()
            println( "For " + name + " the tests took " + TestUtil.formatSeconds( (time2 - time1) * 0.001 ))
         }
      }
   }
}