package de.sciss.collection

import org.scalatest.{FeatureSpec, GivenWhenThen}
import collection.mutable.{Map => MMap}

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.QuadTreeSuite
 * }}
 */
class QuadTreeSuite extends FeatureSpec with GivenWhenThen {
   val RANDOMIZED    = true
   val DETERMINISTIC = false     // currently doesn't pass tests

   val rnd   = new util.Random( 2L )

   val quad = Quad( 0x20000000, 0x20000000, 0x20000000 )
   if( RANDOMIZED ) withTree( "randomized", RandomizedSkipQuadTree.empty[ Int ]( quad ))
   if( DETERMINISTIC ) withTree( "deterministic", DeterministicSkipQuadTree.empty[ Int ]( quad ))

   RandomizedSkipQuadTree.random.setSeed( 0L )

   def randFill( t: SkipQuadTree[ Int ], m: MMap[ Point, Int ]) {
      given( "a randomly filled structure" )

      // seed = 2
      val n     = 0x467 // 0x467 // 0x2F80
      for( i <- 0 until n ) {
         val k = Point( rnd.nextInt( 0x40000000 ),
                        rnd.nextInt( 0x40000000 ))
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

//   def verifyAddRemoveAll( l: SkipList[ Int ], s: MSet[ Int ]) {
//      when( "all elements of the independently maintained set are added again to l" )
//      val szBefore = l.size
//      val newInL   = s.filter( l.add( _ ))
//      val szAfter  = l.size
//      then( "none of the add operations should return 'true'" )
//      assert( newInL.isEmpty, newInL.take( 10 ).toString )
//      then( "the size of l should not change" )
//      assert( szBefore == szAfter, "l had size " + szBefore + " before, but now reports " + szAfter )
//
//      when( "all elements of the independently maintained set are removed from l" )
//      val keptInL  = s.filterNot( l.remove( _ ))
//      val szAfter2 = l.size
//      then( "all of the remove operations should return 'true'" )
//      assert( keptInL.isEmpty, keptInL.take( 10 ).toString )
//      then( "the size of l should be zero" )
//      assert( szAfter2 == 0, szAfter2.toString )
//   }

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
//            verifyAddRemoveAll( t, m )
         }
      }
   }
}