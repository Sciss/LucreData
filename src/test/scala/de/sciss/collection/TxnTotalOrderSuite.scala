package de.sciss.collection

import txn.TotalOrder
import org.scalatest.{GivenWhenThen, FeatureSpec}
import TotalOrder.{RelabelObserver}
import de.sciss.lucrestm.{BerkeleyDB, InMemory, Sys}
import java.io.File

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.TxnTotalOrderSuite
 * }}
 */
class TxnTotalOrderSuite extends FeatureSpec with GivenWhenThen {
   val MONITOR_LABELING = false
   val INMEMORY         = true
   val DATABASE         = true

   val NUM              = 0x10000 // 0x80000  // 0x200000
   val RND_SEED         = 0L

   if( INMEMORY ) {
      withSys[ InMemory ]( "Mem", () => new InMemory(), _ => () )
   }
   if( DATABASE ) {
//      BerkeleyDB.DB_CONSOLE_LOG_LEVEL = "ALL"
      withSys[ BerkeleyDB ]( "BDB", () => {
         val dir     = File.createTempFile( "tree", "_database" )
         dir.delete()
         dir.mkdir()
         val f       = new File( dir, "data" )
         println( f.getAbsolutePath )
         val bdb = BerkeleyDB.open( f )
//         println( "INITIAL DB SIZE = " + bdb.numRefs )
         bdb
      }, bdb => {
         //println( "FINAL   DB SIZE = " + bdb.numRefs )
         val sz = bdb.numUserRecords
         assert( sz == 0, "Final DB user size should be 0, but is " + sz )
         bdb.close()
      })
   }

   def withSys[ S <: Sys[ S ]]( sysName: String, sysCreator: () => S, sysCleanUp: S => Unit ) {
      def scenarioWithTime( descr: String )( body: => Unit ) {
         scenario( descr ) {
            val t1 = System.currentTimeMillis()
            body
            val t2 = System.currentTimeMillis()
            println( "For " + sysName + " the tests took " + TestUtil.formatSeconds( (t2 - t1) * 0.001 ))
         }
      }

      feature( "The ordering of the structure should be consistent" ) {
         info( "Each two successive elements of the structure" )
         info( "should yield '<' in comparison" )

         scenarioWithTime( "Ordering is verified on a randomly filled " + sysName + " structure" ) {
            given( "a randomly filled structure (" + sysName + ")" )

            type E = TotalOrder.Set.Entry[ S ]
            implicit val system = sysCreator()
            try {
               val to = system.atomic { implicit tx =>
                  TotalOrder.Set.empty[ S ]( new RelabelObserver[ S#Tx, E ] {
                     def beforeRelabeling( first: E, num: Int )( implicit tx: S#Tx ) {
                        if( MONITOR_LABELING ) {
   //                     Txn.afterCommit( _ =>
                              println( "...relabeling " + num + " entries" )
   //                     )
                        }
                     }

                     def afterRelabeling( first: E, num: Int )( implicit tx: S#Tx ) {}
                  })
               }
               val rnd   = new util.Random( RND_SEED )
               // would be nice to test maximum possible number of labels
               // but we're running out of heap space ...
               val n     = NUM // 113042 // 3041
      //        to        = to.append() // ( 0 )

               val set = system.atomic { implicit tx =>
                  var e = to.root
                  var coll = Set[ TotalOrder.Set.Entry[ S ]]() // ( e )
                  for( i <- 1 until n ) {
//if( (i % 1000) == 0 ) println( "i = " + i )
                     if( rnd.nextBoolean() ) {
                        e = to.insertAfter( e ) // to.insertAfter( i )
                     } else {
                        e = to.insertBefore( e ) // e.prepend() // to.insertBefore( i )
                     }
                     coll += e
                  }
                  coll
               }
//println( "AQUI" )

               when( "the structure size is determined" )
               val sz = system.atomic { implicit tx => to.size }
      //        val sz = {
      //           var i = 1; var x = to; while( !x.isHead ) { x = x.prev; i +=1 }
      //           x = to; while( !x.isLast ) { x = x.next; i += 1 }
      //           i
      //        }
               then( "it should be equal to the number of elements inserted" )
               assert( sz == n, sz.toString + " != " + n )

               when( "the structure is mapped to its pairwise comparisons" )
               val result = system.atomic { implicit tx =>
                  var res   = Set.empty[ Int ]
                  var prev  = to.head
                  var next  = prev.nextOption.orNull
                  while( next != null ) {
//                  res     += prev compare next
                     res    += prev.tag compare next.tag
                     prev    = next
                     next    = next.nextOption.orNull
                  }
                  res
               }

               then( "the resulting set should only contain -1" )
               assert( result == Set( -1 ), result.toString + " -- " + system.atomic( implicit tx => to.tagList( to.head )))

               when( "the structure is emptied" )
               val sz2 = system.atomic { implicit tx =>
                  set.foreach( to.remove( _ ))
                  to.size
               }
               then( "the order should have size 1" )
               assert( sz2 == 1, "Size is " + sz2 + " and not 1" )

               system.atomic { implicit tx =>
                  set.foreach( _.dispose() )
                  to.dispose()
               }

            } finally {
               sysCleanUp( system )
            }
         }
      }
   }
}