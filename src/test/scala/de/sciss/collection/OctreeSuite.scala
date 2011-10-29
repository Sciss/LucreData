package de.sciss.collection

import geom.{QueryShape, Point3D, DistanceMeasure, Space, Point3DLike, Cube}
import mutable.{RandomizedSkipOctree, SkipOctree, DeterministicSkipOctree}
import org.scalatest.{FeatureSpec, GivenWhenThen}
import collection.breakOut
import collection.mutable.{Set => MSet}

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.OctreeSuite
 * }}
 */
class OctreeSuite extends FeatureSpec with GivenWhenThen {
   val RANDOMIZED    = false
   val DETERMINISTIC = true
   val RANGE_SEARCH  = false
   val NN_SEARCH     = false
   val REMOVAL       = true     // not yet supported by Deterministic

   val n             = 0x09 // 0x1000    // tree size ;  0xE0    // 0x4000 is the maximum acceptable speed
   val n2            = n >> 3    // 0x1000    // range query and nn

   val rnd           = new util.Random( 2L ) // ( 12L )
   val coin          = RandomizedSkipOctree.Coin( 0L )

   val cube          = Cube( 0x40000000, 0x40000000, 0x40000000, 0x40000000 )
   if( RANDOMIZED ) {
      withTree( "randomized", RandomizedSkipOctree.empty[ Space.ThreeDim, Point3DLike ]( Space.ThreeDim, cube, coin ))
   }
   if( DETERMINISTIC ) {
      withTree( "deterministic", DeterministicSkipOctree.empty[ Space.ThreeDim, Point3DLike ]( Space.ThreeDim, cube ))
   }

   val pointFun3D = (mask: Int) => Point3D( rnd.nextInt() & mask, rnd.nextInt() & mask, rnd.nextInt() & mask )

   def randFill[ D <: Space[ D ]]( t: SkipOctree[ D, D#Point ], m: MSet[ D#Point ], pointFun: Int => D#Point ) {
      given( "a randomly filled structure" )

      for( i <- 0 until n ) {
         val k = pointFun( 0x7FFFFFFF )
         t += k
         m += k
      }
   }

   def verifyConsistency[ D <: Space[ D ]]( t: SkipOctree[ D, D#Point ]) {
      when( "the internals of the structure are checked" )
      then( "they should be consistent with the underlying algorithm" )
      val q = t.hyperCube
      var h = t.lastTree
      var currUnlinkedOcs  = Set.empty[ D#HyperCube ]
      var currPoints       = Set.empty[ D#Point ]
      var prevs = 0
      do {
         assert( h.hyperCube == q, "Root level quad is " + h.hyperCube + " while it should be " + q + " in level n - " + prevs )
         val nextUnlinkedOcs  = currUnlinkedOcs
         val nextPoints       = currPoints
         currUnlinkedOcs      = Set.empty
         currPoints           = Set.empty
         def checkChildren( n: t.QNode, depth: Int ) {
            def assertInfo = " in level n-" + prevs + " / depth " + depth

            var i = 0; while( i < 4 ) {
               n.child( i ) match {
                  case c: t.QNode =>
                     val nq = n.hyperCube.orthant( i )
                     val cq = c.hyperCube
                     assert( nq.contains( cq ), "Child has invalid hyper-cube (" + cq + "), expected: " + nq + assertInfo )
                     c.nextOption match {
                        case Some( next ) =>
                           assert( next.prevOption == Some( c ), "Asymmetric next link " + cq + assertInfo )
                           assert( next.hyperCube == cq, "Next hyper-cube does not match (" + cq + " vs. " + next.hyperCube + ")" + assertInfo )
                        case None =>
                           assert( !nextUnlinkedOcs.contains( cq ), "Double missing link for " + cq + assertInfo )
                     }
                     c.prevOption match {
                        case Some( prev ) =>
                           assert( prev.nextOption == Some( c ), "Asymmetric prev link " + cq + assertInfo )
                           assert( prev.hyperCube == cq, "Next hyper-cube do not match (" + cq + " vs. " + prev.hyperCube + ")" + assertInfo )
                        case None => currUnlinkedOcs += cq
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
         h = h.prevOption.orNull
         prevs += 1
      } while( h != null )
   }

   def verifyElems[ D <: Space[ D ]]( t: SkipOctree[ D, D#Point ], m: MSet[ D#Point ]) {
      when( "the structure t is compared to an independently maintained map m" )
      val onlyInM  = m.filterNot { e =>
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
      assert( szT == szM, "octree has size " + szT + " / map has size " + szM )
   }

   def verifyContainsNot[ D <: Space[ D ]]( t: SkipOctree[ D, D#Point ], m: MSet[ D#Point ], pointFun: Int => D#Point ) {
      when( "the structure t is queried for keys not in the independently maintained map m" )
      var testSet = Set.empty[ D#Point ]
      while( testSet.size < 100 ) {
         val x = pointFun( 0xFFFFFFFF )
         if( !m.contains( x )) testSet += x
      }
      val inT = testSet.filter { p =>
         t.contains( p )
      }
      then( "none of them should be contained in t" )
      assert( inT.isEmpty, inT.take( 10 ).toString() )
   }

   def verifyAddRemoveAll[ D <: Space[ D ]]( t: SkipOctree[ D, D#Point ], m: MSet[ D#Point ]) {
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

   val queryFun3D = (max: Int, off: Int, ext: Int) =>
      Cube( rnd.nextInt( max ) - off, rnd.nextInt( max ) - off, rnd.nextInt( max ) - off, rnd.nextInt( ext ))

   val sortFun3D = (p: Point3DLike) => (p.x, p.y, p.z)

   def verifyRangeSearch[ D <: Space[ D ], S : Ordering ]( t: SkipOctree[ D, D#Point ], m: MSet[ D#Point ],
                                                queryFun: (Int, Int, Int) => QueryShape[ D ],
                                                sortFun: D#Point => S ) {
      when( "the octree is range searched" )
      val qs = Seq.fill( n2 )( queryFun( 0x7FFFFFFF, 0x40000000, 0x40000000 ))
      val rangesT = qs.map( q => t.rangeQuery( q ).toSet )
      val ks      = m // keySet
      val rangesM = qs.map( q => ks.filter( q.contains( _ )))
      then( "the results should match brute force with the corresponding set" )
      rangesT.zip(rangesM).foreach { case (s1, s2) =>
         assert( s1 == s2, s1.toList.sortBy( sortFun ).take( 10 ).toString + " -- " +
                           s2.toList.sortBy( sortFun ).take( 10 ))
      }
   }

   val pointFilter3D = (p: Point3DLike) => {
      val dx = if( p.x < cube.cx ) (cube.cx + (cube.extent - 1)).toLong - p.x else p.x - (cube.cx - cube.extent)
      val dy = if( p.y < cube.cy ) (cube.cy + (cube.extent - 1)).toLong - p.y else p.y - (cube.cy - cube.extent)
      val dz = if( p.z < cube.cz ) (cube.cz + (cube.extent - 1)).toLong - p.z else p.z - (cube.cz - cube.extent)
      dx <= 0xB504F300L && dy <= 0xB504F300L && dz <= 0xB504F300L &&
         (dx * dx + dy * dy > 0L) &&
         (dx * dx + dz * dz > 0L) &&
         (dy * dy + dz * dz > 0L)
   }

   def verifyNN[ D <: Space[ D ]]( t: SkipOctree[ D, D#Point ], m: MSet[ D#Point ], pointFun: Int => D#Point,
                                   pointFilter: D#Point => Boolean, euclideanDist: DistanceMeasure[ D ]) {
      when( "the quadtree is searched for nearest neighbours" )
      val ps0 = Seq.fill( n2 )( pointFun( 0xFFFFFFFF ))
      // tricky: this guarantees that there are no 63 bit overflows,
      // while still allowing points outside the root hyperCube to enter the test
      val ps = ps0.filter( pointFilter )
      val nnT: Map[ D#Point, D#Point ] = ps.map( p => p -> t.nearestNeighbor( p, euclideanDist ))( breakOut )
      val ks   = m // .keySet
      val nnM: Map[ D#Point, D#Point ] = ps.map( p => p -> ks.minBy( _.distanceSq( p ))( t.space.bigOrdering ))( breakOut )
      then( "the results should match brute force with the corresponding set" )
      assert( nnT == nnM, {
         (nnT.collect { case (q, v) if( nnM( q ) != v ) => (q, v, nnM( q ))}).take( 10 ).toString()
      })
   }

   def withTree( name: String, tf: => SkipOctree[ Space.ThreeDim, Point3DLike ]) {
      feature( "The " + name + " octree structure should be consistent" ) {
         info( "Several mass operations on the structure" )
         info( "are tried and expected behaviour verified" )

         scenario( "Consistency is verified on a randomly filled structure" ) {
            val t  = tf // ( None )
            val m  = MSet.empty[ Point3DLike ]

            randFill[ Space.ThreeDim ]( t, m, pointFun3D )
            verifyConsistency[ Space.ThreeDim ]( t )
            verifyElems[ Space.ThreeDim ]( t, m )
            verifyContainsNot[ Space.ThreeDim ]( t, m, pointFun3D )

            if( RANGE_SEARCH ) verifyRangeSearch[ Space.ThreeDim, (Int, Int, Int) ]( t, m, queryFun3D, sortFun3D )
if( NN_SEARCH ) println( "WARNING: NN test not yet implemented" )
//            if( NN_SEARCH ) verifyNN( t, m )
            if( REMOVAL ) verifyAddRemoveAll[ Space.ThreeDim ]( t, m )
         }
      }
   }
}