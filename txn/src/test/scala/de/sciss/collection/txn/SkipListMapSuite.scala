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

      val system = InMemory()
      def atomic[ A ]( fun: S#Tx => A ) : A = system.step( fun )

      val map = atomic { implicit tx => SkipList.Map.empty[ S, Int, Int ]}

      scenarioWithTime( "empty", "Consistency is verified on an empty map" ) {
         def onEmptyList() {
            when( "the (size, isEmpty, nonEmpty, height) are queried" )
            val szTup = atomic { implicit tx => (map.size, map.isEmpty, map.nonEmpty, map.height) }
            then( "they should be (0, true, false, 0)" )
            assert( szTup == (0, true, false, 0), "found " + szTup )

            when( "the (floor, ceil, contains) of a number if queried" )
            val flcl = atomic { implicit tx => (map.floor( 42 ), map.ceil( 42 ), map.contains( 42 ))}
            then( "they should be (None, None, false)" )
            assert( flcl == (None, None, false), "found " + flcl )

            when( "the iterator and sequence methods are called" )
            val colls = atomic { implicit tx => (map.iterator.isEmpty, map.toSeq.isEmpty, map.toSet.isEmpty, map.toIndexedSeq.isEmpty) }
            then( "they should all return empty collections" )
            assert( colls == (true, true, true, true), "found " + colls )
         }

         onEmptyList()
      }

      scenarioWithTime( "filled", "Consistency is verified on a randomly filled map" ) {
         val seq = atomic { implicit tx =>
            val _s   = IIdxSeq.tabulate( N )( n => rnd.nextInt() -> n )
            _s.foreach( map += _ )
            _s
         }

         val maxHeight = math.ceil( math.log( N + 1 ) / math.log( map.minGap )).toInt

         when( "the (size, height) is queried" )
         val (sz, h) = atomic { implicit tx => (map.size, map.height) }
         then( "it should be (N, <=log_minGap(N+1)) (" + (N -> maxHeight) + ")" )
         assert( sz == N, "found size " + sz )
         assert( h <= maxHeight, "found height " + h )
      }
   }
}
