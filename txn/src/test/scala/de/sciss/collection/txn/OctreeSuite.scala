package de.sciss.collection
package txn

import geom.{DistanceMeasure3D, QueryShape, IntPoint3D, DistanceMeasure, Space, IntCube}
import org.scalatest.{FeatureSpec, GivenWhenThen}
import collection.breakOut
import collection.mutable.{Set => MSet}
import Space.ThreeDim
import java.io.File
import de.sciss.lucre.stm.impl.BerkeleyDB
import de.sciss.lucre.stm.{Cursor, Durable, InMemory, Sys}

/**
 *
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.txn.OctreeSuite
 * }}
 */
class OctreeSuite extends FeatureSpec with GivenWhenThen {
   val CONSISTENCY   = true
   val RANGE_SEARCH  = true
   val NN_SEARCH     = true
   val REMOVAL       = true
   val INMEMORY      = true
   val DATABASE      = true

   val n             = 0x1000    // tree size ;  0xE0    // 0x4000 is the maximum acceptable speed
   val n2            = n >> 3    // 0x1000    // range query and nn

   val rnd           = new util.Random( 2L ) // ( 12L )

   val cube          = IntCube( 0x40000000, 0x40000000, 0x40000000, 0x40000000 )

   def withSys[ S <: Sys[ S ] with Cursor[ S ]]( sysName: String, sysCreator: () => S, sysCleanUp: (S, Boolean) => Unit ) {
      withTree[ S ]( sysName, () => {
         implicit val sys = sysCreator()
         val t = sys.step { implicit tx =>
            import SpaceSerializers.{Point3DSerializer, CubeSerializer}
            implicit val pointView = (p: IntPoint3D, _: Any) => p
            txn.DeterministicSkipOctree.empty[ S, ThreeDim, IntPoint3D ]( cube )
         }
         (sys, t, succ => sysCleanUp( sys, succ ))
      })
   }

   if( INMEMORY ) {
      withSys[ InMemory ]( "Mem", () => InMemory() : InMemory /* please IDEA */, (_, _) => () )
   }
   if( DATABASE ) {
//BerkeleyDB.DB_CONSOLE_LOG_LEVEL = "ALL"
      withSys[ Durable ]( "BDB", () => {
         val dir     = File.createTempFile( "octree", "_database" )
         dir.delete()
         dir.mkdir()
         println( dir.getAbsolutePath )
         Durable( BerkeleyDB.open( dir )) : Durable /* please IDEA */
      }, { case (bdb, success) =>
//         println( "FINAL DB SIZE = " + bdb.numUserRecords )
         if( success ) {
            val sz = bdb.step( bdb.numUserRecords( _ ))
//            if( sz != 0 ) bdb.step( implicit tx => bdb.debugListUserRecords() ).foreach( println )
            assert( sz == 0, "Final DB user size should be 0, but is " + sz )
         }
         bdb.close()
      })
   }

   val pointFun3D = (mask: Int) => IntPoint3D( rnd.nextInt() & mask, rnd.nextInt() & mask, rnd.nextInt() & mask )

   def randFill[ S <: Sys[ S ], D <: Space[ D ]]( t: SkipOctree[ S, D, D#Point ], m: MSet[ D#Point ],
                                                  pointFun: Int => D#Point )( implicit cursor: Cursor[ S ]) {
      given( "a randomly filled structure" )

      for( i <- 0 until n ) {
         val k = pointFun( 0x7FFFFFFF )
         cursor.step { implicit tx => t += k }
         m += k
      }
   }

   def verifyConsistency[ S <: Sys[ S ], D <: Space[ D ]]( t: DeterministicSkipOctree[ S, D, D#Point ])
                                                         ( implicit cursor: Cursor[ S ]) {
      when( "the internals of the structure are checked" )
      then( "they should be consistent with the underlying algorithm" )

      val q = t.hyperCube
      var h: t.Branch = cursor.step { implicit tx => t.lastTreeImpl }
      var currUnlinkedOcs  = Set.empty[ D#HyperCube ]
      var currPoints       = Set.empty[ D#PointLike ]
      var prevs = 0
      do {
         assert( h.hyperCube == q, "Root level quad is " + h.hyperCube + " while it should be " + q + " in level n - " + prevs )
         val nextUnlinkedOcs  = currUnlinkedOcs
         val nextPoints       = currPoints
         currUnlinkedOcs      = Set.empty
         currPoints           = Set.empty

         def checkChildren( n: t.Branch, depth: Int )( implicit tx: S#Tx ) {
            def assertInfo = " in level n-" + prevs + " / depth " + depth

            var i = 0; while( i < t.numOrthants ) {
               n.child( i ) match {
                  case cb: t.Branch =>
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

                  case l: t.Leaf =>
                     currPoints += l.value

                  case _ =>
               }
            i += 1 }
         }
         cursor.step { implicit tx => checkChildren( h, 0 )}
         val pointsOnlyInNext    = nextPoints.filterNot( currPoints.contains( _ ))
         assert( pointsOnlyInNext.isEmpty, "Points in next which aren't in current (" + pointsOnlyInNext.take( 10 ) + "); in level n-" + prevs )
         h = h.prevOption.orNull
         prevs += 1
      } while( h != null )
   }

   def verifyElems[ S <: Sys[ S ], D <: Space[ D ]]( t: SkipOctree[ S, D, D#Point ],
                                                     m: MSet[ D#Point ])( implicit cursor: Cursor[ S ]) {
      when( "the structure t is compared to an independently maintained map m" )
      val onlyInM  = cursor.step { implicit tx => m.filterNot { e =>
         t.contains( e )
      }}
      val onlyInT  = cursor.step { implicit tx => t.iterator.toList.filterNot( e => m.contains( e ))}
      val szT      = cursor.step { implicit tx => t.size }
      val szM      = m.size
      then( "all elements of m should be contained in t" )
      assert( onlyInM.isEmpty, onlyInM.take( 10 ).toString() )
      then( "all elements of t should be contained in m" )
      assert( onlyInT.isEmpty, onlyInT.take( 10 ).toString() )
      then( "both should report the same size" )
      assert( szT == szM, "octree has size " + szT + " / map has size " + szM )
   }

   def verifyContainsNot[ S <: Sys[ S ], D <: Space[ D ]]( t: SkipOctree[ S, D, D#Point ],
                                                           m: MSet[ D#Point ],
                                                           pointFun: Int => D#Point )
                                                         ( implicit cursor: Cursor[ S ]) {
      when( "the structure t is queried for keys not in the independently maintained map m" )
      var testSet = Set.empty[ D#Point ]
      while( testSet.size < 100 ) {
         val x = pointFun( 0xFFFFFFFF )
         if( !m.contains( x )) testSet += x
      }
      val inT = cursor.step { implicit tx => testSet.filter { p =>
         t.contains( p )
      }}
      then( "none of them should be contained in t" )
      assert( inT.isEmpty, inT.take( 10 ).toString() )
   }

   def verifyAddRemoveAll[ S <: Sys[ S ], D <: Space[ D ]]( t: SkipOctree[ S, D, D#Point ],
                                                            m: MSet[ D#Point ])
                                                          ( implicit cursor: Cursor[ S ]) {
      when( "all elements of the independently maintained map are added again to t" )
      val szBefore = cursor.step { implicit tx => t.size }
//println( "BEFORE " + t.system.step { implicit tx => t.toList })
      val newInT   = cursor.step { implicit tx => m.filter( e => t.update( e ).isEmpty )}
//println( "AFTER " + t.system.step { implicit tx => t.toList })
      val szAfter  = cursor.step { implicit tx => t.size }
      then( "all of the put operations should return 'Some'" )
      assert( newInT.isEmpty, newInT.take( 10 ).toString() )
      then( "the size of t should not change" )
      assert( szBefore == szAfter, "t had size " + szBefore + " before, but now reports " + szAfter )

      when( "all elements of the independently maintained map are removed from t" )
      val keptInT  = cursor.step { implicit tx => m.filter( e => t.removeAt( e ).isEmpty )}
      val szAfter2 = cursor.step { implicit tx => t.size }
      then( "all of the remove operations should return 'Some'" )
      assert( keptInT.isEmpty, keptInT.take( 10 ).toString() )
      then( "the size of t should be zero" )
      assert( szAfter2 == 0, szAfter2.toString )
   }

   val queryFun3D = (max: Int, off: Int, ext: Int) =>
      IntCube( rnd.nextInt( max ) - off, rnd.nextInt( max ) - off, rnd.nextInt( max ) - off, rnd.nextInt( ext ))

   val sortFun3D = (p: ThreeDim#PointLike) => (p.x, p.y, p.z)

   def verifyRangeSearch[ S <: Sys[ S ], A, D <: Space[ D ], Sort ]( t: SkipOctree[ S, D, D#Point ], m: MSet[ D#Point ],
                          queryFun: (Int, Int, Int) => QueryShape[ A, D ],
                          sortFun: D#PointLike => Sort )( implicit ord: math.Ordering[ Sort ], cursor: Cursor[ S ]) {
      when( "the octree is range searched" )
      val qs = Seq.fill( n2 )( queryFun( 0x7FFFFFFF, 0x40000000, 0x40000000 ))
      val rangesT = cursor.step { implicit tx => qs.map( q => t.rangeQuery( q ).toSet )}
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

   // JUHUUUUU SPECIALIZATION BROKEN ONCE MORE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   def verifyNN[ S <: Sys[ S ], M, D <: Space[ D ]](
      t: SkipOctree[ S, D, D#Point ], m: MSet[ D#Point ], pointFun: Int => D#Point,
      pointFilter: D#PointLike => Boolean, euclideanDist: DistanceMeasure[ M, D ])( implicit ord: math.Ordering[ M ], cursor: Cursor[ S ]) {

      when( "the quadtree is searched for nearest neighbours" )
      val ps0 = Seq.fill( n2 )( pointFun( 0xFFFFFFFF ))
      // tricky: this guarantees that there are no 63 bit overflows,
      // while still allowing points outside the root hyperCube to enter the test
      val ps = ps0.filter( pointFilter )
      val nnT: Map[ D#Point, D#Point ] = cursor.step { implicit tx =>
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

//   def no[ S <: Sys[ S ], @specialized( Long ) M, D <: Space[ D ]](
//      t: SkipOctree[ S, D, D#Point ], euclideanDist: DistanceMeasure[ M, D ]) {
//      t.system.step( implicit tx => () )
//   }
//
//   def yes1[ S <: Sys[ S ], @specialized( Long ) M, D <: Space[ D ]](
//      t: SkipOctree[ S, D, D#Point ], euclideanDist: DistanceMeasure[ M, D ]) {
//      t.system.step( tx => () )
//   }
//
//   def yes2[ S <: Sys[ S ], M, D <: Space[ D ]](
//      t: SkipOctree[ S, D, D#Point ], euclideanDist: DistanceMeasure[ M, D ]) {
//      t.system.step( implicit tx => () )
//   }

//   def yes[ S <: Sys[ S ], D <: Space[ D ]]( t: SkipOctree[ S, D, D#Point ], pointFun: Int => D#Point ) {
//      val ps = Seq.empty[ D#Point ]
//      t.system.step { implicit tx =>
//         ps.map( p => p -> p )
//      }
//   }

   def withTree[ S <: Sys[ S ]]( name: String, tf: () => (S with Cursor[ S ], DeterministicSkipOctree[ S, ThreeDim, ThreeDim#Point ], Boolean => Unit) ) {
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
            val (_sys, t, cleanUp)  = tf()
            implicit val system    = _sys
            var success = false
            try {
               val m  = MSet.empty[ ThreeDim#Point ]

               randFill[ S, ThreeDim ]( t, m, pointFun3D )
               if( CONSISTENCY ) verifyConsistency[ S, ThreeDim ]( t )
               verifyElems[ S, ThreeDim ]( t, m )
               verifyContainsNot[ S, ThreeDim ]( t, m, pointFun3D )

               if( RANGE_SEARCH ) verifyRangeSearch[ S, BigInt, ThreeDim, (Int, Int, Int) ]( t, m, queryFun3D, sortFun3D )
               if( NN_SEARCH ) verifyNN[ S, BigInt, ThreeDim ]( t, m, pointFun3D, pointFilter3D, euclideanDist3D )
               if( REMOVAL ) verifyAddRemoveAll[ S, ThreeDim ]( t, m )

               system.step { implicit tx =>
                  try {
                     t.clear()
                     t.dispose()
                  } catch {
                     case e =>
                        e.printStackTrace()
                        throw e
                  }
               }
               success = true

            } finally {
               cleanUp( success )
            }
         }
      }
   }
}