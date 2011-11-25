package de.sciss.collection

import geom.{DistanceMeasure3D, QueryShape, Point3D, DistanceMeasure, Space, Cube}
import org.scalatest.{FeatureSpec, GivenWhenThen}
import collection.breakOut
import collection.mutable.{Set => MSet}
import Space.ThreeDim
import de.sciss.lucrestm.{InMemory, Sys}
import txn.{DeterministicSkipOctree, SkipOctree}

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.TxnOctreeSuite
 * }}
 */
class TxnOctreeSuite extends FeatureSpec with GivenWhenThen {
   val CONSISTENCY   = true
   val RANGE_SEARCH  = true
   val NN_SEARCH     = true
   val REMOVAL       = true
   val INMEMORY      = false
   val DATABASE      = true

   val n             = 200 // 0x1000    // tree size ;  0xE0    // 0x4000 is the maximum acceptable speed
   val n2            = n >> 3    // 0x1000    // range query and nn

   val rnd           = new util.Random( 2L ) // ( 12L )

   val cube          = Cube( 0x40000000, 0x40000000, 0x40000000, 0x40000000 )

   def withSys[ S <: Sys[ S ]]( sysName: String, sysCreator: () => S, sysCleanUp: S => Unit )
                              ( implicit smf: Manifest[ S ]) {
      withTree[ S ]( sysName, () => {
         implicit val sys = sysCreator()
         val t = sys.atomic { implicit tx =>
            import txn.geom.Space.{Point3DSerializer, CubeSerializer}
            txn.DeterministicSkipOctree.empty[ S, ThreeDim, Point3D ]( cube )
         }
         (t, () => sysCleanUp( sys ))
      })
   }

   if( INMEMORY ) {
      withSys[ InMemory ]( "deterministic", () => new InMemory, _ => () )
   }
   if( DATABASE ) {
      sys.error( "Not yet implemented" )
   }

   val pointFun3D = (mask: Int) => Point3D( rnd.nextInt() & mask, rnd.nextInt() & mask, rnd.nextInt() & mask )

   def randFill[ S <: Sys[ S ], D <: Space[ D ]]( t: SkipOctree[ S, D, D#Point ], m: MSet[ D#Point ], pointFun: Int => D#Point ) {
      given( "a randomly filled structure" )

      for( i <- 0 until n ) {
         val k = pointFun( 0x7FFFFFFF )
         t.system.atomic { implicit tx => t += k }
         m += k
      }
   }

   def verifyConsistency[ S <: Sys[ S ], D <: Space[ D ]]( t: DeterministicSkipOctree[ S, D, D#Point ]) {
      when( "the internals of the structure are checked" )
      then( "they should be consistent with the underlying algorithm" )

      import DeterministicSkipOctree.{Branch}

      val q = t.hyperCube
      var h: Branch[ S, D, D#Point ] = t.system.atomic { implicit tx => t.lastTree }
      var currUnlinkedOcs  = Set.empty[ D#HyperCube ]
      var currPoints       = Set.empty[ D#PointLike ]
      var prevs = 0
      do {
         assert( h.hyperCube == q, "Root level quad is " + h.hyperCube + " while it should be " + q + " in level n - " + prevs )
         val nextUnlinkedOcs  = currUnlinkedOcs
         val nextPoints       = currPoints
         currUnlinkedOcs      = Set.empty
         currPoints           = Set.empty

         def checkChildren( n: Branch[ S, D, D#Point ], depth: Int )( implicit tx: S#Tx ) {
            def assertInfo = " in level n-" + prevs + " / depth " + depth

            var i = 0; while( i < t.numOrthants ) {
               val c = n.child( i )
               if( c != null ) {
                  if( c.isBranch ) {
                     val cb = c.asBranch
                     val nq = n.hyperCube.orthant( i )
                     val cq = cb.hyperCube
                     assert( nq.contains( cq ), "Node has invalid hyper-cube (" + cq + "), expected: " + nq + assertInfo )
                     assert( n.hyperCube.indexOf( cq ) == i, "Mismatch between index-of and used orthant (" + i + "), with parent " + n.hyperCube + " and " + cq )
                     cb.nextOption match {
                        case Some( next ) =>
                           assert( next.prevOption == Some( cb ), "Asymmetric next link " + cq + assertInfo )
                           assert( next.hyperCube == cq, "Next hyper-cube does not match (" + cq + " vs. " + next.hyperCube + ")" + assertInfo )
                        case None =>
                           assert( !nextUnlinkedOcs.contains( cq ), "Double missing link for " + cq + assertInfo )
                     }
                     cb.prevOption match {
                        case Some( prev ) =>
                           assert( prev.nextOption == Some( cb ), "Asymmetric prev link " + cq + assertInfo )
                           assert( prev.hyperCube == cq, "Next hyper-cube do not match (" + cq + " vs. " + prev.hyperCube + ")" + assertInfo )
                        case None => currUnlinkedOcs += cq
                     }
                     checkChildren( cb, depth + 1 )
                  } else {
                     val l = c.asLeaf
                     currPoints += l.value
                  }
               }
            i += 1 }
         }
         t.system.atomic { implicit tx => checkChildren( h, 0 )}
         val pointsOnlyInNext    = nextPoints.filterNot( currPoints.contains( _ ))
         assert( pointsOnlyInNext.isEmpty, "Points in next which aren't in current (" + pointsOnlyInNext.take( 10 ) + "); in level n-" + prevs )
         h = h.prevOption.orNull
         prevs += 1
      } while( h != null )
   }

   def verifyElems[ S <: Sys[ S ], D <: Space[ D ]]( t: SkipOctree[ S, D, D#Point ], m: MSet[ D#Point ]) {
      when( "the structure t is compared to an independently maintained map m" )
      val onlyInM  = t.system.atomic { implicit tx => m.filterNot { e =>
         t.contains( e )
      }}
      val onlyInT  = t.system.atomic { implicit tx => t.iterator.filterNot( e => m.contains( e ))}
      val szT      = t.system.atomic { implicit tx => t.size }
      val szM      = m.size
      then( "all elements of m should be contained in t" )
      assert( onlyInM.isEmpty, onlyInM.take( 10 ).toString() )
      then( "all elements of t should be contained in m" )
      assert( onlyInT.isEmpty, onlyInT.take( 10 ).toString() )
      then( "both should report the same size" )
      assert( szT == szM, "octree has size " + szT + " / map has size " + szM )
   }

   def verifyContainsNot[ S <: Sys[ S ], D <: Space[ D ]]( t: SkipOctree[ S, D, D#Point ], m: MSet[ D#Point ], pointFun: Int => D#Point ) {
      when( "the structure t is queried for keys not in the independently maintained map m" )
      var testSet = Set.empty[ D#Point ]
      while( testSet.size < 100 ) {
         val x = pointFun( 0xFFFFFFFF )
         if( !m.contains( x )) testSet += x
      }
      val inT = t.system.atomic { implicit tx => testSet.filter { p =>
         t.contains( p )
      }}
      then( "none of them should be contained in t" )
      assert( inT.isEmpty, inT.take( 10 ).toString() )
   }

   def verifyAddRemoveAll[ S <: Sys[ S ], D <: Space[ D ]]( t: SkipOctree[ S, D, D#Point ], m: MSet[ D#Point ]) {
      when( "all elements of the independently maintained map are added again to t" )
      val szBefore = t.system.atomic { implicit tx => t.size }
//println( "BEFORE " + t.system.atomic { implicit tx => t.toList })
      val newInT   = t.system.atomic { implicit tx => m.filter( e =>
         t.update( e ).isEmpty
      )}
//println( "AFTER " + t.system.atomic { implicit tx => t.toList })
      val szAfter  = t.system.atomic { implicit tx => t.size }
      then( "all of the put operations should return 'Some'" )
      assert( newInT.isEmpty, newInT.take( 10 ).toString() )
      then( "the size of t should not change" )
      assert( szBefore == szAfter, "t had size " + szBefore + " before, but now reports " + szAfter )

      when( "all elements of the independently maintained map are removed from t" )
      val keptInT  = t.system.atomic { implicit tx => m.filter( e => t.removeAt( e ).isEmpty )}
      val szAfter2 = t.system.atomic { implicit tx => t.size }
      then( "all of the remove operations should return 'Some'" )
      assert( keptInT.isEmpty, keptInT.take( 10 ).toString() )
      then( "the size of t should be zero" )
      assert( szAfter2 == 0, szAfter2.toString )
   }

   val queryFun3D = (max: Int, off: Int, ext: Int) =>
      Cube( rnd.nextInt( max ) - off, rnd.nextInt( max ) - off, rnd.nextInt( max ) - off, rnd.nextInt( ext ))

   val sortFun3D = (p: ThreeDim#PointLike) => (p.x, p.y, p.z)

   def verifyRangeSearch[ S <: Sys[ S ], A, D <: Space[ D ], Sort : math.Ordering ]( t: SkipOctree[ S, D, D#Point ], m: MSet[ D#Point ],
                          queryFun: (Int, Int, Int) => QueryShape[ A, D ],
                          sortFun: D#PointLike => Sort ) {
      when( "the octree is range searched" )
      val qs = Seq.fill( n2 )( queryFun( 0x7FFFFFFF, 0x40000000, 0x40000000 ))
      val rangesT = t.system.atomic { implicit tx => qs.map( q => t.rangeQuery( q ).toSet )}
      val ks      = m // keySet
      val rangesM = qs.map( q => ks.filter( q.contains( _ )))
      then( "the results should match brute force with the corresponding set" )
      rangesT.zip(rangesM).foreach { case (s1, s2) =>
         assert( s1 == s2, s1.toList.sortBy( sortFun ).take( 10 ).toString + " -- " +
                           s2.toList.sortBy( sortFun ).take( 10 ))
      }
   }

   val pointFilter3D = (p: ThreeDim#PointLike) => {
      val dx = if( p.x < cube.cx ) (cube.cx + (cube.extent - 1)).toLong - p.x else p.x - (cube.cx - cube.extent)
      val dy = if( p.y < cube.cy ) (cube.cy + (cube.extent - 1)).toLong - p.y else p.y - (cube.cy - cube.extent)
      val dz = if( p.z < cube.cz ) (cube.cz + (cube.extent - 1)).toLong - p.z else p.z - (cube.cz - cube.extent)
      dx <= 0xB504F300L && dy <= 0xB504F300L && dz <= 0xB504F300L &&
         (dx * dx + dy * dy > 0L) &&
         (dx * dx + dz * dz > 0L) &&
         (dy * dy + dz * dz > 0L)
   }

   val euclideanDist3D = DistanceMeasure3D.euclideanSq

   def verifyNN[ S <: Sys[ S ], @specialized( Long ) M : math.Ordering, D <: Space[ D ]](
      t: SkipOctree[ S, D, D#Point ], m: MSet[ D#Point ], pointFun: Int => D#Point,
      pointFilter: D#PointLike => Boolean, euclideanDist: DistanceMeasure[ M, D ]) {

      when( "the quadtree is searched for nearest neighbours" )
      val ps0 = Seq.fill( n2 )( pointFun( 0xFFFFFFFF ))
      // tricky: this guarantees that there are no 63 bit overflows,
      // while still allowing points outside the root hyperCube to enter the test
      val ps = ps0.filter( pointFilter )
      val nnT: Map[ D#Point, D#Point ] = t.system.atomic { implicit tx =>
         ps.map( p => p -> t.nearestNeighbor( p, euclideanDist ))( breakOut )
      }
      val ks   = m // .keySet
//      val nnM: Map[ D#Point, D#Point ] = ps.map( p => p -> ks.minBy( _.distanceSq( p ))( t.space.bigOrdering ))( breakOut )
      val nnM: Map[ D#Point , D#Point ] = ps.map( p => p -> ks.minBy( p2 => euclideanDist.distance( p2, p )))( breakOut )
      then( "the results should match brute force with the corresponding set" )
      assert( nnT == nnM, {
         (nnT.collect { case (q, v) if( nnM( q ) != v ) => (q, v, nnM( q ))}).take( 10 ).toString()
      })
   }

   def withTree[ S <: Sys[ S ]]( name: String, tf: () => (DeterministicSkipOctree[ S, ThreeDim, ThreeDim#Point ], () => Unit) ) {
      feature( "The " + name + " octree structure should be consistent" ) {
         info( "Several mass operations on the structure" )
         info( "are tried and expected behaviour verified" )

         def scenarioWithTime( descr: String )( body: => Unit ) {
            scenario( descr ) {
               val t1 = System.currentTimeMillis()
               body
               val t2 = System.currentTimeMillis()
               println( "For " + name + " the tests took " + TestUtil.formatSeconds( (t2 - t1) * 0.001 ))
            }
         }

         scenarioWithTime( "Consistency is verified on a randomly filled structure" ) {
            val (t, cleanUp)  = tf()
            try {
               val m  = MSet.empty[ ThreeDim#Point ]

               randFill[ S, ThreeDim ]( t, m, pointFun3D )
               if( CONSISTENCY ) verifyConsistency[ S, ThreeDim ]( t )
               verifyElems[ S, ThreeDim ]( t, m )
               verifyContainsNot[ S, ThreeDim ]( t, m, pointFun3D )

               if( RANGE_SEARCH ) verifyRangeSearch[ S, BigInt, ThreeDim, (Int, Int, Int) ]( t, m, queryFun3D, sortFun3D )
               if( NN_SEARCH ) verifyNN[ S, BigInt, ThreeDim ]( t, m, pointFun3D, pointFilter3D, euclideanDist3D )
               if( REMOVAL ) verifyAddRemoveAll[ S, ThreeDim ]( t, m )

            } finally {
               cleanUp()
            }
         }
      }
   }
}