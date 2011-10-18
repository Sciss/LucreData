package de.sciss.collection

import mutable.{HASkipList, LLSkipList, SkipList}
import scala.collection.immutable.IntMap
import scala.collection.mutable.{Set => MSet}
import org.scalatest.{GivenWhenThen, FeatureSpec}

class SkipListSuite extends FeatureSpec with GivenWhenThen {
   val CONSISTENCY   = true
   val OBSERVATION   = true
   val LL            = true
   val HA            = false     // currently doesn't pass tests

   val rnd   = new util.Random()

   if( LL ) withList( "LL", oo => LLSkipList.empty[ Int ]( oo.getOrElse( SkipList.NoKeyObserver )))
   if( HA ) withList( "HA", oo => HASkipList.empty[ Int ]( keyObserver = oo.getOrElse( SkipList.NoKeyObserver )))

   def randFill( l: SkipList[ Int ], s: MSet[ Int ]) {
      given( "a randomly filled structure" )
      val n     = 0x200000
      for( i <- 0 until n ) {
         val x = rnd.nextInt( 0x7FFFFFFF )
         l.add( x )
         s.add( x )
      }
   }

   def randFill2 : Set[ Int ] = {
      given( "a set of random numbers" )
      val n     = 0x100000
      var res   = Set.empty[ Int ]
      for( i <- 0 until n ) {
         res += rnd.nextInt() & ~1   // any int except MaxValue
      }
      res
   }

   def randFill3 : Set[ Int ] = {
      val n     = 10
      var res   = Set.empty[ Int ]
      for( i <- 0 until n ) {
         res += rnd.nextInt( 100 )
      }
      given( "a small set of numbers : " + res.mkString( ", " ))
      res
   }

   def verifyOrder( l: SkipList[ Int ]) {
      when( "the structure is mapped to its pairwise comparisons" )
      var result= Set.empty[ Int ]
      val iter  = l.iterator
      var prev  = -2
      while( iter.hasNext ) {
         val next  = iter.next
         result   += prev compare next
         prev      = next
      }

      then( "the resulting set should only contain -1" )
      assert( result == Set( -1 ), result.toString )
   }
   def verifyElems( l: SkipList[ Int ], s: MSet[ Int ]) {
      when( "the structure l is compared to an independently maintained set s" )
      val onlyInS  = s.filterNot( l.contains( _ ))
      val onlyInL  = l.filterNot( s.contains( _ ))
      val szL      = l.size
      val szS      = s.size
      then( "all elements of s should be contained in l" )
      assert( onlyInS.isEmpty, onlyInS.take( 10 ).toString )
      then( "all elements of l should be contained in s" )
      assert( onlyInL.isEmpty, onlyInL.take( 10 ).toString )
      then( "both should report the same size" )
      assert( szL == szS, "skip list has size " + szL + " / set has size " + szS )
   }

   def verifyContainsNot( l: SkipList[ Int ], s: MSet[ Int ]) {
      when( "the structure l is queried for keys not in the independently maintained set s" )
      var testSet = Set.empty[ Int ]
      while( testSet.size < 100 ) {
         val x = rnd.nextInt()
         if( !s.contains( x )) testSet += x
      }
      val inL = testSet.filter( l.contains( _ ))
      then( "none of them should be contained in l" )
      assert( inL.isEmpty, inL.take( 10 ).toString )
   }

   def verifyAddRemoveAll( l: SkipList[ Int ], s: MSet[ Int ]) {
      when( "all elements of the independently maintained set are added again to l" )
      val szBefore = l.size
      val newInL   = s.filter( l.add( _ ))
      val szAfter  = l.size
      then( "none of the add operations should return 'true'" )
      assert( newInL.isEmpty, newInL.take( 10 ).toString )
      then( "the size of l should not change" )
      assert( szBefore == szAfter, "l had size " + szBefore + " before, but now reports " + szAfter )

      when( "all elements of the independently maintained set are removed from l" )
      val keptInL  = s.filterNot( l.remove( _ ))
      val szAfter2 = l.size
      then( "all of the remove operations should return 'true'" )
      assert( keptInL.isEmpty, keptInL.take( 10 ).toString )
      then( "the size of l should be zero" )
      assert( szAfter2 == 0, szAfter2.toString )
   }

   def withList( name: String, lf: Option[ SkipList.KeyObserver[ Int ]] => SkipList[ Int ]) {
      if( CONSISTENCY ) {
         feature( "The " + name + " skip list structure should be consistent" ) {
            info( "Several mass operations on the structure" )
            info( "are tried and expected behaviour verified" )

            scenario( "Consistency is verified on a randomly filled structure" ) {
               val l  = lf( None )
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

            scenario( "Observation is verified on a randomly filled structure" ) {
               val obs   = new Obs
               val l     = lf( Some( obs ))
               val s     = randFill2 // randFill3
               when( "all the elements of the set are added" )
               var uppedInsKey = false
               s.foreach { i =>
                  obs.oneUp -= i
                  l.add( i )
                  uppedInsKey |= obs.oneUp.contains( i )
               }
               then( "none was ever promoted during their insertion" )
               assert( !uppedInsKey )
               then( "there haven't been any demotions" )
               assert( obs.allDn.isEmpty, obs.allDn.take( 10 ).toString )
               then( "there were several promotions" )
               assert( obs.allUp.nonEmpty )
               then( "the height of the list is equal to the maximally promoted key + 1" )
               val maxProm = obs.allUp.maxBy( _._2 )._2
               val h  = l.height
               assert( h == maxProm + 1, "Height is reported as " + h + ", while keys were maximally promoted " + maxProm + " times" )
               val sz = s.size + 1 // account for the 'maxKey'
               val minH = math.ceil( math.log( sz ) / math.log( l.maxGap + 1 )).toInt
               val maxH = math.ceil( math.log( sz ) / math.log( l.minGap + 1 )).toInt
               then( "ceil(log(n+1)/log(maxGap+1)) <= height-1 <= ceil(log(n+1)/log(minGap+1))" )
               assert( minH <= (h-1) && (h-1) <= maxH, "Not: " + minH + " <= " + (h-1) + " <= " + maxH )

               when( "all the elements are removed again" )
               var uppedDelKey = false
               s.foreach { i =>
                  obs.oneUp -= i
                  l.remove( i )
                  uppedDelKey |= obs.oneUp.contains( i )
               }
               then( "none was ever promoted during their deletion" )
               assert( !uppedDelKey )
               val upsNotInS = obs.allUp.keys.filterNot( s.contains( _ ))
               then( "no key was ever promoted which was not in s" )
               assert( upsNotInS.isEmpty, upsNotInS.take( 10 ).toString )
               val dnsNotInS = obs.allDn.keys.filterNot( s.contains( _ ))
               then( "no key was ever demoted which was not in s" )
               assert( dnsNotInS.isEmpty, dnsNotInS.take( 10 ).toString )
               val unbal = s.map( i => i -> (obs.allUp.getOrElse( i, 0 ) - obs.allDn.getOrElse( i, 0 ))).filterNot( _._2 == 0 )
               then( "the number of promotions and demotions for every key is equal" )
               assert( unbal.isEmpty, unbal.take( 10 ).toString )
            }
         }
      }
   }

   class Obs extends SkipList.KeyObserver[ Int ] {
      var allUp = IntMap.empty[ Int ]
      var allDn = IntMap.empty[ Int ]
      var oneUp = IntMap.empty[ Int ]

      def keyUp( key: Int ) {
         allUp += key -> (allUp.getOrElse( key, 0 ) + 1)
         oneUp += key -> (oneUp.getOrElse( key, 0 ) + 1)
      }

      def keyDown( key: Int ) {
         allDn += key -> (allDn.getOrElse( key, 0 ) + 1)
      }
   }
}