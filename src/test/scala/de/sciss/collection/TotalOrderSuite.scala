package de.sciss.collection

import mutable.TotalOrder
import org.scalatest.{GivenWhenThen, FeatureSpec}
import TotalOrder.{EntryLike, RelabelObserver}

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.TotalOrderSuite
 * }}
 */
class TotalOrderSuite extends FeatureSpec with GivenWhenThen {
   val MONITOR_LABELING = false

   feature( "The ordering of the structure should be consistent" ) {
      info( "Each two successive elements of the structure" )
      info( "should yield '<' in comparison" )

      scenario( "Ordering is verified on a randomly filled structure" ) {
         given( "a randomly filled structure" )
         val to    = TotalOrder( new RelabelObserver {
            def beforeRelabeling(first: EntryLike, num: Int) {
               if( MONITOR_LABELING ) println( "...relabeling " + num + " entries" )
            }

            def afterRelabeling(first: EntryLike, num: Int) {}
         })
         val rnd   = new util.Random() // ( 0 )
         // would be nice to test maximum possible number of labels
         // but we're running out of heap space ...
         val n     = 0x200000 // 113042 // 3041
//        to        = to.append() // ( 0 )
         var e = to.root
         for( i <- 1 until n ) {
            if( rnd.nextBoolean() ) {
               e = e.append() // to.insertAfter( i )
            } else {
               e = e.prepend() // to.insertBefore( i )
            }
         }

         when( "the structure size is determined" )
         val sz = to.size
//        val sz = {
//           var i = 1; var x = to; while( !x.isHead ) { x = x.prev; i +=1 }
//           x = to; while( !x.isLast ) { x = x.next; i += 1 }
//           i
//        }
         then( "it should be equal to the number of elements inserted" )
         assert( sz == n, sz.toString + " != " + n )

         when( "the structure is mapped to its pairwise comparisons" )
         var result= Set.empty[ Int ]
         var prev  = to.head
         var next  = prev.next
         while( !next.isEnd ) {
            result += prev compare next
            prev    = next
            next    = next.next
         }

         then( "the resulting set should only contain -1" )
         assert( result == Set( -1 ), result.toString + " -- " + to.head.tagList  )
      }
   }
}