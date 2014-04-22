package de.sciss
package lucre
package data

import org.scalatest.{GivenWhenThen, FeatureSpec}
import stm.store.BerkeleyDB
import java.io.File
import TotalOrder.Map.RelabelObserver
import stm.{Cursor, Durable, InMemory, Sys}
import collection.immutable.{Vector => Vec}
import serial.{DataInput, DataOutput, Writable}

/*
 To run this test copy + paste the following into sbt:

test-only de.sciss.lucre.data.TotalOrderSuite

 */
object TotalOrderSuite {
  object MapHolder {
    final class Serializer[S <: Sys[S]](observer: RelabelObserver[S#Tx, MapHolder[S]], tx0: S#Tx)
      extends serial.Serializer[S#Tx, S#Acc, MapHolder[S]] {

      val map = TotalOrder.Map.empty[S, MapHolder[S]](observer, _.entry)(tx0, this)

      def write(v: MapHolder[S], out: DataOutput): Unit = v.write(out)

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): MapHolder[S] = {
        val num = in.readInt()
        val e = map.readEntry(in, access)
        new MapHolder(num, e)
      }
    }
  }
  final case class MapHolder[S <: Sys[S]](num: Int, entry: TotalOrder.Map.Entry[S, MapHolder[S]])
    extends Writable {

    def write(out: DataOutput): Unit = {
      out.writeInt(num)
      entry.write(out)
    }
  }
}
class TotalOrderSuite extends FeatureSpec with GivenWhenThen {
  import TotalOrderSuite.MapHolder

//   val MONITOR_LABELING = false
   val INMEMORY         = true
   val DATABASE         = true
   val TEST1            = true   // large consistency test
   val TEST2            = true   // small boundary cases test

   val NUM              = 0x10000 // 0x80000  // 0x200000
   val RND_SEED         = 0L

   if( INMEMORY ) {
      withSys[ InMemory ]( "Mem", () => InMemory(): InMemory /* please IDEA */, _ => () )
   }

   if( DATABASE ) {
//      BerkeleyDB.DB_CONSOLE_LOG_LEVEL = "ALL"
      withSys[ Durable ]( "BDB", () => {
         val dir     = File.createTempFile( "tree", "_database" )
         dir.delete()
         dir.mkdir()
         println( dir.getAbsolutePath )
         val bdb = BerkeleyDB.open( dir )
//         println( "INITIAL DB SIZE = " + bdb.numRefs )
         Durable( bdb ) : Durable   /* please IDEA */
      }, bdb => {
         //println( "FINAL   DB SIZE = " + bdb.numRefs )
         val sz = bdb.step( bdb.numUserRecords( _ ))
         assert( sz == 0, "Final DB user size should be 0, but is " + sz )
         bdb.close()
      })
   }

   def withSys[ S <: Sys[ S ]]( sysName: String, sysCreator: () => S with Cursor[ S ],
                                sysCleanUp: S => Unit ): Unit = {
      def scenarioWithTime( descr: String )( body: => Unit ): Unit = {
         scenario( descr ) {
            val t1 = System.currentTimeMillis()
            body
            val t2 = System.currentTimeMillis()
            println( "For " + sysName + " the tests took " + TestUtil.formatSeconds( (t2 - t1) * 0.001 ))
         }
      }

      if( TEST1 ) feature( "The ordering of the structure should be consistent" ) {
         info( "Each two successive elements of the structure" )
         info( "should yield '<' in comparison" )

         scenarioWithTime( "Ordering is verified on a randomly filled " + sysName + " structure" ) {
            Given( "a randomly filled structure (" + sysName + ")" )

//            type E = TotalOrder.Set.Entry[ S ]
            implicit val system = sysCreator()
            try {
               val to = system.step { implicit tx =>
                  TotalOrder.Set.empty[ S ] /* ( new RelabelObserver[ S#Tx, E ] {
                     def beforeRelabeling( first: E, num: Int )( implicit tx: S#Tx ): Unit = {
                        if( MONITOR_LABELING ) {
   //                     Txn.afterCommit( _ =>
                              println( "...relabeling " + num + " entries" )
   //                     )
                        }
                     }

                     def afterRelabeling( first: E, num: Int )( implicit tx: S#Tx ) = ()
                  }) */
               }
               val rnd   = new util.Random( RND_SEED )
               // would be nice to test maximum possible number of labels
               // but we're running out of heap space ...
               val n     = NUM // 113042 // 3041
      //        to        = to.append() // ( 0 )

               val set = system.step { implicit tx =>
                  var e = to.root
                  var coll = Set[ TotalOrder.Set.Entry[ S ]]() // ( e )
                  for( i <- 1 until n ) {
//if( (i % 1000) == 0 ) println( "i = " + i )
                     if( rnd.nextBoolean() ) {
                        e = e.append() // to.insertAfter( e ) // to.insertAfter( i )
                     } else {
                        e = e.prepend() // to.insertBefore( e ) // e.prepend() // to.insertBefore( i )
                     }
                     coll += e
                  }
                  coll
               }
//println( "AQUI" )

               When( "the structure size is determined" )
               val sz = system.step { implicit tx => to.size }
      //        val sz = {
      //           var i = 1; var x = to; while( !x.isHead ) { x = x.prev; i +=1 }
      //           x = to; while( !x.isLast ) { x = x.next; i += 1 }
      //           i
      //        }
               Then( "it should be equal to the number of elements inserted" )
               assert( sz == n, sz.toString + " != " + n )

               When( "the structure is mapped to its pairwise comparisons" )
               val result = system.step { implicit tx =>
                  var res   = Set.empty[ Int ]
                  var prev  = to.head
                  var next  = prev.next.orNull
                  while( next != null ) {
//                  res     += prev compare next
                     res    += prev.tag compare next.tag
                     prev    = next
                     next    = next.next.orNull
                  }
                  res
               }

               Then( "the resulting set should only contain -1" )
               assert( result == Set( -1 ), result.toString + " -- " + system.step( implicit tx => to.tagList( to.head )))

               When( "the structure is emptied" )
               val sz2 = system.step { implicit tx =>
//                  set.foreach( _.remove() )
                  set.foreach( _.removeAndDispose() )
                  to.size
               }
               Then( "the order should have size 1" )
               assert( sz2 == 1, "Size is " + sz2 + " and not 1" )

               system.step { implicit tx =>
//                  set.foreach( _.removeAndDispose() )
                  to.dispose()
               }

            } finally {
               sysCleanUp( system )
            }
         }
      }

      if( TEST2 ) feature( "The structure should accept boundary cases" ) {
         scenarioWithTime( "Triggering overflows at the boundaries in a " + sysName + " structure" ) {
            implicit val system = sysCreator()
            try {
               Given( "an empty map structure" )

               val order = system.step { implicit tx =>
                  val ser = new MapHolder.Serializer( new RelabelObserver[ S#Tx, MapHolder[ S ]] {
                     def beforeRelabeling( dirty: Iterator[ S#Tx, MapHolder[ S ]])( implicit tx: S#Tx ): Unit = {
                        dirty.toIndexedSeq   // just to make sure the iterator succeeds
                     }

                     def afterRelabeling( clean: Iterator[ S#Tx, MapHolder[ S ]])( implicit tx: S#Tx ): Unit = {
                        clean.toIndexedSeq   // just to make sure the iterator succeeds
                     }
                  }, tx )
                  ser.map
               }

               When( "the structure is filled by 1000x repeated appending" )
               val rootHolder = new MapHolder( 0, order.root )
               var e = rootHolder
               var holders = Vec( e )
               for( i <- 1 to 1000 ) {
                  e = system.step { implicit tx =>
                     val prev = e
                     val next = new MapHolder( i, order.insert() )
                     order.placeAfter( prev, next )
                     next
                  }
                  holders :+= e
               }
               Then( "the resulting sequence should match (0 to 1000)" )
               val checkApp = system.step { implicit tx =>
                  holders.sliding( 2, 1 ).forall {
                     case Seq( prev, next ) => prev.num.compare( next.num) == prev.entry.compare( next.entry )
                  }
               }
               assert( checkApp )

               When( "all elements are removed" )
               val szApp = system.step { implicit tx =>
                  holders.drop( 1 ).foreach { h =>
                     h.entry.removeAndDispose()
                  }
                  order.size
               }
               Then( "the structure should have size 1 (root)" )
               assert( szApp === 1 )

               When( "the structure is filled by 1000x repeated prepending" )
               e = rootHolder
               holders = Vec( e )
               for( i <- 1 to 1000 ) {
                  e = system.step { implicit tx =>
                     val prev = e
                     val next = new MapHolder( i, order.insert() )
                     order.placeBefore( prev, next )
                     next
                  }
                  holders :+= e
               }
               Then( "the resulting sequence should match (0 to 1000)" )
               val checkPrep = system.step { implicit tx =>
                  holders.sliding( 2, 1 ).forall {
                     case Seq( prev, next ) => next.num.compare( prev.num ) == prev.entry.compare( next.entry )
                  }
               }
               assert( checkPrep )

               When( "all elements are removed" )
               val szPrep = system.step { implicit tx =>
                  holders.drop( 1 ).foreach { h =>
                     h.entry.removeAndDispose()
                  }
                  order.size
               }
               Then( "the structure should have size 1 (root)" )
               assert( szPrep === 1 )

               // dispose
               system.step { implicit tx => order.dispose() }

            } finally {
               sysCleanUp( system )
            }
         }
      }
   }
}