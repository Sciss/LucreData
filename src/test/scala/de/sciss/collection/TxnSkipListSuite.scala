package de.sciss.collection

import scala.collection.immutable.IntMap
import scala.collection.mutable.{Set => MSet}
import org.scalatest.{GivenWhenThen, FeatureSpec}
import txn.{SkipList, HASkipList}
import concurrent.stm.{Ref, InTxn, TxnExecutor}

/**
 * To run this test copy + paste the following into sbt:
 * {{
 * test-only de.sciss.collection.TxnSkipListSuite
 * }}
 */
class TxnSkipListSuite extends FeatureSpec with GivenWhenThen {
   val CONSISTENCY   = true
   val OBSERVATION   = true
   val REMOVAL       = true

   // large
   val NUM1          = 0x040000  // 0x200000
   val NUM2          = 0x020000  // 0x100000

   // small
   val NUM3          = 10

   val rnd   = new util.Random( 0L )

   withList( "HA-1", (oo, txn) => HASkipList.empty[ Int ]( minGap = 1, keyObserver = oo ))
   withList( "HA-2", (oo, txn) => HASkipList.empty[ Int ]( minGap = 2, keyObserver = oo ))

   def atomic[ A ]( fun: InTxn => A ) : A = TxnExecutor.defaultAtomic( fun )

   def randFill( l: SkipList[ Int ], s: MSet[ Int ]) {
      given( "a randomly filled structure" )
      for( i <- 0 until NUM1 ) {
         val x = rnd.nextInt( 0x7FFFFFFF )
//println( "i = " + i )
         atomic { implicit tx => l.add( x )}
         s.add( x )
      }
   }

   def randFill2 : Set[ Int ] = {
      given( "a set of random numbers" )
      var res   = Set.empty[ Int ]
      for( i <- 0 until NUM2 ) {
         res += rnd.nextInt() & ~1   // any int except MaxValue
      }
      res
   }

   def randFill3 : Set[ Int ] = {
      var res   = Set.empty[ Int ]
      for( i <- 0 until NUM3 ) {
         res += rnd.nextInt( 100 )
      }
      given( "a small set of numbers : " + res.mkString( ", " ))
      res
   }

   def verifyOrder( l: SkipList[ Int ]) {
      when( "the structure is mapped to its pairwise comparisons" )
      val result = atomic { implicit tx =>
         var res  = Set.empty[ Int ]
         val iter = l.iterator
         var prev = -2
         while( iter.hasNext ) {
            val next  = iter.next()
            res      += prev compare next
            prev      = next
         }
         res
      }

      then( "the resulting set should only contain -1" )
      assert( result == Set( -1 ), result.toString() )
   }
   def verifyElems( l: SkipList[ Int ], s: MSet[ Int ]) {
      when( "the structure l is compared to an independently maintained set s" )
      val onlyInS  = atomic { implicit tx => s.filterNot( l.contains( _ ))}
      val onlyInL  = atomic { implicit tx => l.toList.filterNot( s.contains( _ ))}
      val szL      = atomic { implicit tx => l.size }
      val szS      = s.size
      then( "all elements of s should be contained in l" )
      assert( onlyInS.isEmpty, onlyInS.take( 10 ).toString() )
      then( "all elements of l should be contained in s" )
      assert( onlyInL.isEmpty, onlyInL.take( 10 ).toString() )
      then( "both should report the same size" )
      assert( szL == szS, "skip list has size " + szL + " / set has size " + szS )
   }

   def verifyContainsNot( l: SkipList[ Int ], s: MSet[ Int ]) {
      when( "the structure l is queried for keys not in the independently maintained set s" )
      var testSet = Set.empty[ Int ]
      atomic { implicit tx =>
         while( testSet.size < 100 ) {
            val x = rnd.nextInt()
            if( !s.contains( x )) testSet += x
         }
      }
      val inL = atomic { implicit tx => testSet.filter( l.contains( _ ))}
      then( "none of them should be contained in l" )
      assert( inL.isEmpty, inL.take( 10 ).toString() )
   }

   def verifyAddRemoveAll( l: SkipList[ Int ], s: MSet[ Int ]) {
      when( "all elements of the independently maintained set are added again to l" )
      val szBefore = atomic { implicit tx => l.size }
      val newInL   = atomic { implicit tx => s.filter( l.add( _ ))}
      val szAfter  = atomic { implicit tx => l.size }
      then( "none of the add operations should return 'true'" )
      assert( newInL.isEmpty, newInL.take( 10 ).toString() )
      then( "the size of l should not change" )
      assert( szBefore == szAfter, "l had size " + szBefore + " before, but now reports " + szAfter )

      if( REMOVAL ) {
         when( "all elements of the independently maintained set are removed from l" )
         val keptInL  = atomic { implicit tx => s.filterNot( l.remove( _ ))}
         val szAfter2 = atomic { implicit tx => l.size }
         then( "all of the remove operations should return 'true'" )
         assert( keptInL.isEmpty, keptInL.take( 10 ).toString() )
         then( "the size of l should be zero" )
         assert( szAfter2 == 0, szAfter2.toString )
      }
   }

   private def withList( name: String, lf: (SkipList.KeyObserver[ Int ], InTxn) => SkipList[ Int ]) {
      def scenarioWithTime( descr: String )( body: => Unit ) {
         scenario( descr ) {
            val t1 = System.currentTimeMillis()
            body
            val t2 = System.currentTimeMillis()
            println( "For " + name + " the tests took " + TestUtil.formatSeconds( (t2 - t1) * 0.001 ))
         }
      }

      if( CONSISTENCY ) {
         feature( "The " + name + " skip list structure should be consistent" ) {
            info( "Several mass operations on the structure" )
            info( "are tried and expected behaviour verified" )

            scenarioWithTime( "Consistency is verified on a randomly filled structure" ) {
               val l  = atomic { tx => lf( SkipList.NoKeyObserver, tx )}
               val s  = MSet.empty[ Int ]
               randFill( l, s )
               verifyOrder( l )
               verifyElems( l, s )
               verifyContainsNot( l, s )
               verifyAddRemoveAll( l, s )
            }
         }
      }

      if( OBSERVATION ) {
         feature( "The " + name + " skip list structure should be observable" ) {
            info( "Several operations are performed on the structure while an" )
            info( "observer monitors key promotions and demotions" )

            scenarioWithTime( "Observation is verified on a randomly filled structure" ) {
               val obs   = new Obs
               val l     = atomic { tx => lf( obs, tx )}
               val s     = randFill2 // randFill3
               when( "all the elements of the set are added" )
               var uppedInsKey = false
               atomic { implicit tx =>
                  s.foreach { i =>
                     obs.oneUp.transform( _ - i )
                     l.add( i )
                     uppedInsKey |= obs.oneUp().contains( i )
                  }
               }
               then( "none was ever promoted during their insertion" )
               assert( !uppedInsKey )
               then( "there haven't been any demotions" )
               assert( atomic { implicit tx => obs.allDn() } isEmpty, atomic { implicit tx => obs.allDn() } take( 10 ) toString() )
               then( "there were several promotions" )
               assert( atomic { implicit tx => obs.allUp() } nonEmpty )
               then( "the height of the list is equal to the maximally promoted key + 1" )
               val maxProm = atomic { implicit tx => obs.allUp().maxBy( _._2 )._2 }
               val h  = atomic { implicit tx => l.height }
               assert( h == maxProm + 1, "Height is reported as " + h + ", while keys were maximally promoted " + maxProm + " times" )
               val sz = s.size + 1 // account for the 'maxKey'
               val minH = math.ceil( math.log( sz ) / math.log( l.maxGap + 1 )).toInt
               val maxH = math.ceil( math.log( sz ) / math.log( l.minGap + 1 )).toInt
               then( "ceil(log(n+1)/log(maxGap+1)) <= height-1 <= ceil(log(n+1)/log(minGap+1))" )
               assert( minH <= (h-1) && (h-1) <= maxH, "Not: " + minH + " <= " + (h-1) + " <= " + maxH )

               if( REMOVAL ) {
                  when( "all the elements are removed again" )
                  var uppedDelKey = false
                  atomic { implicit tx =>
                     s.foreach { i =>
                        obs.oneUp.transform( _ - i )
                        l.remove( i )
                        uppedDelKey |= obs.oneUp().contains( i )
                     }
                  }
                  then( "none was ever promoted during their deletion" )
                  assert( !uppedDelKey )
                  val upsNotInS = atomic { implicit tx => obs.allUp().keys.filterNot( s.contains( _ ))}
                  then( "no key was ever promoted which was not in s" )
                  assert( upsNotInS.isEmpty, upsNotInS.take( 10 ).toString() )
                  val dnsNotInS = atomic { implicit tx => obs.allDn().keys.filterNot( s.contains( _ ))}
                  then( "no key was ever demoted which was not in s" )
                  assert( dnsNotInS.isEmpty, dnsNotInS.take( 10 ).toString() )
                  val unbal = atomic { implicit tx =>
                     s.map( i => i -> (obs.allUp().getOrElse( i, 0 ) - obs.allDn().getOrElse( i, 0 ))).filterNot( _._2 == 0 )
                  }
                  then( "the number of promotions and demotions for every key is equal" )
                  assert( unbal.isEmpty, unbal.take( 10 ).toString() )
               }
            }
         }
      }
   }

   class Obs extends SkipList.KeyObserver[ Int ] {
      var allUp = Ref( IntMap.empty[ Int ])
      var allDn = Ref( IntMap.empty[ Int ])
      var oneUp = Ref( IntMap.empty[ Int ])

      def keyUp( key: Int )( implicit tx: InTxn ) {
         allUp.transform( m => m + (key -> (m.getOrElse( key, 0 ) + 1)))
         oneUp.transform( m => m + (key -> (m.getOrElse( key, 0 ) + 1)))
      }

      def keyDown( key: Int )( implicit tx: InTxn ) {
         allDn.transform( m => m + (key -> (m.getOrElse( key, 0 ) + 1)))
      }
   }
}