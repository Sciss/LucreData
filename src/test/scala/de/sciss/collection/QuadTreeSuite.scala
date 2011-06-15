package de.sciss.collection

import org.scalatest.{FeatureSpec, GivenWhenThen}
import collection.mutable.{Map => MMap, Set => MSet}

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.QuadTreeSuite
 * }}
 */
class QuadTreeSuite extends FeatureSpec with GivenWhenThen {
   val RANDOMIZED    = true
   val DETERMINISTIC = false     // currently doesn't pass tests

   val rnd   = new util.Random( 0L )

   val quad = Quad( 0x20000000, 0x20000000, 0x20000000 )
   if( RANDOMIZED ) withTree( "randomized", RandomizedSkipQuadTree.empty[ Int ]( quad ))
   if( DETERMINISTIC ) withTree( "deterministic", DeterministicSkipQuadTree.empty[ Int ]( quad ))

   def randFill( t: SkipQuadTree[ Int ], m: MMap[ Point, Int ]) {
      given( "a randomly filled structure" )
      val n     = 0x200000
      for( i <- 0 until n ) {
         val k = Point( rnd.nextInt( 0x40000000 ),
                        rnd.nextInt( 0x40000000 ))
         val v = rnd.nextInt()
//println( "Putting " + k )
         t.put( k, v )
         m.put( k, v )
      }
   }


   def verifyElems( t: SkipQuadTree[ Int ], m: MMap[ Point, Int ]) {
      when( "the structure t is compared to an independently maintained map m" )
      val onlyInM  = m.filterNot( e => t.contains( e._1 ))
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
println( "testin " + p )
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
//            verifyOrder( t )
            verifyElems( t, m )
            verifyContainsNot( t, m )
//            verifyAddRemoveAll( t, m )
         }
      }
   }
}