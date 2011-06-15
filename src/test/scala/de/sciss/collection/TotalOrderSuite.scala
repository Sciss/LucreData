package de.sciss.collection

import org.scalatest.{GivenWhenThen, FeatureSpec}

class TotalOrderSuite extends FeatureSpec with GivenWhenThen {
  feature( "The ordering of the structure should be consistent" ) {
     info( "Each two successive elements of the structure" )
     info( "should yield '<' in comparison" )

     scenario( "Ordering is verified on a randomly filled structure" ) {
        given( "a randomly filled structure" )
        var to    = TotalOrder[ Int ]()
        val rnd   = new util.Random() // ( 0 )
        // would be nice to test maximum possible number of labels
        // but we're running out of heap space ...
        val n     = 0x200000 // 113042 // 3041
        to        = to.append( 0 )
        for( i <- 1 until n ) {
           if( rnd.nextBoolean() ) {
              to.insertAfter( i )
           } else {
              to = to.insertBefore( i )
           }
        }

        when( "the structure size is determined" )
        val sz = to.size
        then( "it should be equal to the number of elements inserted" )
        assert( sz == n, sz.toString + " != " + n )

        when( "the structure is mapped to its pairwise comparisons" )
        var result= Set.empty[ Int ]
        var prev  = to
        var next  = to.next
        while( next.nonEmpty ) {
           result += prev compare next
           prev    = next
           next    = next.next
        }

        then( "the resulting set should only contain -1" )
        assert( result == Set( -1 ), result.toString + " -- " + to.tagList  )
     }
  }
}