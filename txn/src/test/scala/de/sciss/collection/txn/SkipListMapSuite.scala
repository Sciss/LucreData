package de.sciss.collection.txn

import collection.immutable.{IndexedSeq => IIdxSeq}
import collection.mutable.{Set => MSet}
import org.scalatest.{GivenWhenThen, FeatureSpec}
import de.sciss.lucre.stm.{Cursor, InMemory, Sys}

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.txn.SkipListMapSuite
 * }}
 */
class SkipListMapSuite extends FeatureSpec with GivenWhenThen {
   val SEED          = 0L
   val N             = 1000

   type S            = InMemory
   val rnd           = new util.Random( SEED )

   def scenarioWithTime( name: String, descr: String )( body: => Unit ) {
      scenario( descr ) {
         val t1 = System.currentTimeMillis()
         body
         val t2 = System.currentTimeMillis()
         println( "For " + name + " the tests took " + TestUtil.formatSeconds( (t2 - t1) * 0.001 ))
      }
   }

   feature( "The skip list map structure should be consistent" ) {
      info( "Several mass operations on the structure" )
      info( "are tried and expected behaviour verified" )

      scenarioWithTime( "Consistency", "Consistency is verified on a randomly filled structure" ) {
         val system = InMemory()
         def atomic[ A ]( fun: S#Tx => A ) : A = system.step( fun )

         val map = atomic { implicit tx => SkipList.Map.empty[ S, Int, Int ]}

         def onEmptyList() {
            when( "the (size, isEmpty, nonEmpty) are queried" )
            val szTup = atomic { implicit tx => (map.size, map.isEmpty, map.nonEmpty) }
            then( "they should be (0, true, false)" )
            assert( szTup == (0, true, false), "found " + szTup )
            when( "the floor or ceil of a number if queried" )
            val flcl = atomic { implicit tx => (map.floor( 42 ), map.ceil( 42 ))}
            then( "they should be undefined" )
            assert( flcl == (None, None), "found " + flcl )
         }

         onEmptyList()

         val seq = atomic { implicit tx =>
            val _s   = IIdxSeq.tabulate( N )( n => rnd.nextInt() -> n )
            _s.foreach( map += _ )
            _s
         }

         when( "the size is queried" )
         val sz = atomic { implicit tx => map.size }
         then( "it should be N (" + N + ")" )
         assert( sz == N, "found " + sz )
      }
   }
}
