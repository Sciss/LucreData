package de.sciss.collection

import org.scalatest.{FeatureSpec, GivenWhenThen}
import collection.breakOut
import collection.mutable.{Map => MMap}
import sys.error

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.QuadTreeSuite
 * }}
 */
class QuadTreeSuite extends FeatureSpec with GivenWhenThen {
   val RANDOMIZED    = true
   val DETERMINISTIC = false     // currently doesn't pass tests
   val n             = 0xE0    // 0x4000 is the maximum acceptable speed

   val rnd   = new util.Random( 12L )

//   val quad = Quad( 0x20000000, 0x20000000, 0x20000000 )
   val quad = Quad( 0x40000000, 0x40000000, 0x40000000 )
   if( RANDOMIZED ) withTree( "randomized", RandomizedSkipQuadTree.empty[ Int ]( quad ))
   if( DETERMINISTIC ) withTree( "deterministic", DeterministicSkipQuadTree.empty[ Int ]( quad ))

   RandomizedSkipQuadTree.random.setSeed( 0L )

   def randFill( t: SkipQuadTree[ Int ], m: MMap[ Point, Int ]) {
      given( "a randomly filled structure" )

      // seed = 2
//      val n     = 0x4000 // 0x10000 // 0x467 // 0x467 // 0x2F80
      for( i <- 0 until n ) {
//         val k = Point( rnd.nextInt( 0x40000000 ),
//                        rnd.nextInt( 0x40000000 ))
         val k = Point( rnd.nextInt() & 0x7FFFFFFF,
                        rnd.nextInt() & 0x7FFFFFFF )
         val v = rnd.nextInt()
//println( "Putting " + k )
         t.put( k, v )
         m.put( k, v )
      }
   }

   def verifyConsistency( t: SkipQuadTree[ Int ]) {
      when( "the internals of the structure are checked" )
      then( "they should be consistent with the underlying algorithm" )
      val q = t.quad
      var h = t.lastTree
      var currUnlinkedQuads   = Set.empty[ Quad ]
      var currPoints          = Set.empty[ Point ]
      var prevs = 0
      do {
         assert( h.quad == q, "Root level quad is " + h.quad + " while it should be " + q + " in level n - " + prevs )
         val nextUnlinkedQuads   = currUnlinkedQuads
         val nextPoints          = currPoints
         currUnlinkedQuads       = Set.empty
         currPoints              = Set.empty
         def checkChildren( n: SkipQuadTree[ Int ]#QNode, depth: Int ) {
            def assertInfo = " in level n-" + prevs + " / depth " + depth

            var i = 0; while( i < 4 ) {
               n.child( i ) match {
                  case c: SkipQuadTree[ _ ]#QNode =>
                     val nq = n.quad.quadrant( i )
                     val cq = c.quad
                     assert( nq.contains( cq ), "Child has invalid quad (" + cq + "), expected: " + nq + assertInfo )
                     c.nextOption match {
                        case Some( next ) =>
                           assert( next.prevOption == Some( c ), "Asymmetric next link " + cq + assertInfo )
                           assert( next.quad == cq, "Next quad does not match (" + cq + " vs. " + next.quad + ")" + assertInfo )
                        case None =>
                           assert( !nextUnlinkedQuads.contains( cq ), "Double missing link for " + cq + assertInfo )
                     }
                     c.prevOption match {
                        case Some( prev ) =>
                           assert( prev.nextOption == Some( c ), "Asymmetric prev link " + cq + assertInfo )
                           assert( prev.quad == cq, "Next quad do not match (" + cq + " vs. " + prev.quad + ")" + assertInfo )
                        case None => currUnlinkedQuads += cq
                     }
                     checkChildren( c, depth + 1 )
                  case l: SkipQuadTree[ _ ]#QLeaf =>
                     currPoints += l.point
                  case _: SkipQuadTree[ _ ]#QEmpty =>
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

   def verifyElems( t: SkipQuadTree[ Int ], m: MMap[ Point, Int ]) {
      when( "the structure t is compared to an independently maintained map m" )
      val onlyInM  = m.filterNot { e =>
//         println( "Contains ? " + e._1 )
         t.contains( e._1 )
      }
      val onlyInT  = t.filterNot( e => m.contains( e._1 ))
      val szT      = t.size
      val szM      = m.size
      then( "all elements of m should be contained in t" )
      assert( onlyInM.isEmpty, onlyInM.take( 10 ).toString )
      then( "all elements of t should be contained in m" )
      assert( onlyInT.isEmpty, onlyInT.take( 10 ).toString )
      then( "both should report the same size" )
      assert( szT == szM, "quadtree has size " + szT + " / map has size " + szM )
   }

   def verifyContainsNot( t: SkipQuadTree[ Int ], m: MMap[ Point, Int ]) {
      when( "the structure t is queried for keys not in the independently maintained map m" )
      var testSet = Set.empty[ Point ]
      while( testSet.size < 100 ) {
         val x = Point( rnd.nextInt(), rnd.nextInt() )
         if( !m.contains( x )) testSet += x
      }
      val inT = testSet.filter { p =>
//println( "testin " + p )
         t.contains( p )
      }
      then( "none of them should be contained in t" )
      assert( inT.isEmpty, inT.take( 10 ).toString )
   }

   def verifyAddRemoveAll( t: SkipQuadTree[ Int ], m: MMap[ Point, Int ]) {
      when( "all elements of the independently maintained map are added again to t" )
      val szBefore = t.size
      val newInT   = m.filter( tup => t.put( tup._1, tup._2 ).isEmpty )
      val szAfter  = t.size
      then( "all of the put operations should return 'Some'" )
      assert( newInT.isEmpty, newInT.take( 10 ).toString )
      then( "the size of t should not change" )
      assert( szBefore == szAfter, "t had size " + szBefore + " before, but now reports " + szAfter )

      when( "all elements of the independently maintained map are removed from t" )
      val keptInT  = m.filter( tup => t.remove( tup._1 ).isEmpty )
      val szAfter2 = t.size
      then( "all of the remove operations should return 'Some'" )
      assert( keptInT.isEmpty, keptInT.take( 10 ).toString )
      then( "the size of t should be zero" )
      assert( szAfter2 == 0, szAfter2.toString )
   }

   def verifyRangeSearch( t: SkipQuadTree[ Int ], m: MMap[ Point, Int ]) {
      when( "the quadtree is range searched" )
      val qs = Seq.fill( 20 )( Quad( rnd.nextInt( 0x7FFFFFFF ) - 0x40000000,
                                     rnd.nextInt( 0x7FFFFFFF ) - 0x40000000, rnd.nextInt( 0x40000000 )))
      val rangesT = qs.map( q => t.rangeQuery( q ).map( _._1 ).toSet )
      val ks      = m.keySet
      val rangesM = qs.map( q => ks.filter( q.contains( _ )))
      then( "the results should match brute force with the corresponding set" )
      rangesT.zip(rangesM).foreach { case (s1, s2) =>
         assert( s1 == s2, s1.toList.sortBy( p => (p.x, p.y) ).take( 10 ).toString + " -- " +
                           s2.toList.sortBy( p => (p.x, p.y) ).take( 10 ))
      }
   }

   def verifyNN( t: SkipQuadTree[ Int ], m: MMap[ Point, Int ]) {
      when( "the quadtree is searched for nearest neighbours" )
      val ps0  = Seq.fill( 666 )( Point( rnd.nextInt(), rnd.nextInt() ))
println( "WARNING: restricting search to points inside the tree' quad for now" )
val ps = ps0.filter( t.quad.contains( _ ))
      val nnT: Map[ Point, Point ] = ps.map( p => p -> t.nearestNeighbor( p ).getOrElse( error( p.toString ))._1 )( breakOut )
      val ks   = m.keySet
      val nnM: Map[ Point, Point ] = ps.map( p => p -> ks.minBy( _.distanceSq( p )))( breakOut )
      then( "the results should match brute force with the corresponding set" )
      assert( nnT == nnM, {
         (nnT.collect { case (q, v) if( nnM( q ) != v ) => (q, v, nnM( q ))}).take( 10 ).toString
//         nnT.take( 10 ).toString + " -- " + nnM.take( 10 )
      })
   }

   def withTree( name: String, tf: => SkipQuadTree[ Int ]) {
      feature( "The " + name + " quadtree structure should be consistent" ) {
         info( "Several mass operations on the structure" )
         info( "are tried and expected behaviour verified" )

         scenario( "Consistency is verified on a randomly filled structure" ) {
            val t  = tf // ( None )
            val m  = MMap.empty[ Point, Int ]
            randFill( t, m )
            verifyConsistency( t )
//            verifyOrder( t )
            verifyElems( t, m )
            verifyContainsNot( t, m )
            verifyRangeSearch( t, m )
            verifyNN( t, m )
            verifyAddRemoveAll( t, m )
         }
      }
   }
}