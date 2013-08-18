package de.sciss.lucre
package data

import collection.immutable.{Vector => Vec}    // see SI-6150
import org.scalatest.{GivenWhenThen, FeatureSpec}
import stm.InMemory

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

      def onEmptyList() {
         When( "the (size, isEmpty, nonEmpty, height) are queried" )
         val szTup = atomic { implicit tx => (map.size, map.isEmpty, map.nonEmpty, map.height) }
         Then( "they should be (0, true, false, 0)" )
         assert( szTup == (0, true, false, 0), "found " + szTup )

         When( "the (floor, ceil, contains) of a number if queried" )
         val flcl = atomic { implicit tx => (map.floor( 42 ), map.ceil( 42 ), map.contains( 42 ))}
         Then( "they should be (None, None, false)" )
         assert( flcl == (None, None, false), "found " + flcl )

         When( "the iterator and sequence methods are called" )
         val colls = atomic { implicit tx => (map.iterator.isEmpty, map.toSeq.isEmpty, map.toSet.isEmpty, map.toIndexedSeq.isEmpty) }
         Then( "they should all return empty collections" )
         assert( colls == (true, true, true, true), "found " + colls )
      }

      scenarioWithTime( "empty", "Consistency is verified on an empty map" ) {
         onEmptyList()
      }

      val seq = Vec.tabulate( N )( n => rnd.nextInt() -> n )

      scenarioWithTime( "filled", "Consistency is verified on a randomly filled map" ) {
         atomic { implicit tx =>
            seq.foreach( map += _ )
         }

         val maxHeight = math.ceil( math.log( N + 1 ) / math.log( map.minGap )).toInt

         When( "the (size, isEmpty, nonEmpty, height) is queried" )
         val (sz, isEmpty, nonEmpty, h) = atomic { implicit tx => (map.size, map.isEmpty, map.nonEmpty, map.height) }
         Then( "it should be (N, <=log_minGap(N+1)) (" + (N -> maxHeight) + ")" )
         assert( sz == N, "found size " + sz )
         assert( !isEmpty && nonEmpty, "isEmpty = " + isEmpty + "; nonEmpty = " + nonEmpty )
         assert( h <= maxHeight, "found height " + h )

         When( "the floor and ceil values for a random number of elements are queried" )
         val q    = Vec.fill( N )( rnd.nextInt() )
         val fc   = atomic { implicit tx => q.map { v => (map.floor( v ).getOrElse( (Int.MinValue, 0) ),
                                                          map.ceil(  v ).getOrElse( (Int.MaxValue, 0) ))}}
         Then( "they should be the same as with brute force search" )
         val brute = q.map { v =>
            seq.foldLeft( ((Int.MinValue, 0), (Int.MaxValue, 0)) ) { case ((fl, cl), v2) =>
               (if( fl._1 >= v2._1 || v2._1 > v ) fl else v2) -> (if( cl._1 <= v2._1 || v2._1 < v ) cl else v2)
            }
         }
         assert( fc == brute, "Differ: " + (fc.take( 5 ), brute.take( 5 )) )

         When( "the floor and ceil values for the elements are asked" )
         val allHit = atomic { implicit tx => seq.forall { tup => map.floor( tup._1 ) == Some( tup ) &&
                                                                  map.ceil(  tup._1 ) == Some( tup )}}
         Then( "they should return the elements themselves" )
         assert( allHit )
      }

      scenarioWithTime( "emptied", "Consistency is verified When emptying the map" ) {
         When( "all elements are removed" )
         val rem = atomic { implicit tx => seq.flatMap( v => map.remove( v._1 ))}
         Then( "the returned elements should be same as the seq" )
         val seqVals = seq.map( _._2 )
         assert( rem == seqVals, "Differ: " + (rem.take( 5 ), seqVals.take( 5 )))

         onEmptyList()
      }
   }
}
