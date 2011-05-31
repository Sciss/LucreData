import de.sciss.tree.TotalOrder
import org.scalatest.{GivenWhenThen, FeatureSpec}

class TotalOrderSuite extends FeatureSpec with GivenWhenThen {
  feature( "The ordering of the structure should be consistent" ) {
     info( "Each two successive elements of the structure" )
     info( "should yield '<' in comparison" )

     scenario( "Ordering is verified on a randomly filled structure" ) {
        given( "a randomly filled structure" )
        val to    = TotalOrder[ Int ]()
        val rnd   = new util.Random( 0 )
        val n     = 3042 // 3041
        var pred  = to.append( 0 )
        for( i <- 1 until n ) {
           pred   = if( rnd.nextBoolean() ) {
              pred.insertAfter( i )
           } else {
              pred.insertBefore( i )
           }
        }

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