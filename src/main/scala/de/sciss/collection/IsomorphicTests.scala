package de.sciss.collection

import sys.error
import collection.immutable.IntMap

object IsomorphicTests {
   def main( args: Array[ String ]) { run }

   class Vertex[ A ]( val value: A, val pre: TotalOrder#Entry, val post: TotalOrder#Entry )
   extends PointLike {
      val preTail = pre.append() // insertAfter( () )

      def x: Int = pre.tag
      def y: Int = post.tag

//      def insertChild( value: A ) = {
//         val cPre    = preTail.insertBefore( () )
//         val cPost   = post.insertBefore( () )
//         new Vertex( value, cPre, cPost )
//      }
//
//      def insertRetroParent( value: A ) = {
//         val cPre    = pre.insertBefore( () )
//         val cPost   = post.insertAfter( () )
//         new Vertex( value, cPre, cPost )
//      }
//
//      def insertRetroChild( value: A ) = {
//         val cPre    = pre.insertAfter( () )
//         val cPost   = post.insertBefore( () )
//         new Vertex( value, cPre, cPost )
//      }
   }

   class Tree[ A ]( _init: A ) {
      type V = Vertex[ A ]

      private val preObserver    = new OrderObserver
      private val postObserver   = new OrderObserver
      val root = new Vertex( _init, TotalOrder( preObserver ).root, TotalOrder( postObserver ).root )
      val quad = RandomizedSkipQuadTree.empty[ V ]( Quad( 0x40000000, 0x40000000, 0x40000000 ))

      add( root )

      private class OrderObserver extends TotalOrder.RelabelObserver {
         var map = Map.empty[ TotalOrder.EntryLike, V ]
         def beforeRelabeling( first: TotalOrder.EntryLike, num: Int ) {
            var e = first
            var i = 0; while( i < num ) {
               map.get( e ).foreach( quad.remove( _ ))
               e = e.next
               i += 1
            }
         }
         def afterRelabeling( first: TotalOrder.EntryLike, num: Int ) {
            var e = first
            var i = 0; while( i < num ) {
               map.get( e ).foreach( quad.add( _ ))
               e = e.next
               i += 1
            }
         }
      }

      private def add( v: V ) : V = {
         preObserver.map  += v.pre -> v
         postObserver.map += v.post -> v
         quad += v
         v
      }

      def insertChild( parent: Vertex[ A ], value : A ) : Vertex[ A ] = {
         val cPre    = parent.preTail.prepend() // insertBefore( () )
         val cPost   = parent.post.prepend() // insertBefore( () )
         add( new Vertex( value, cPre, cPost ))
      }
   }

   def run() {
      val t       = new Tree( () )
      val rnd     = new util.Random()
      val n       = 10000
      var treeSeq = IndexedSeq( t.root )

      for( i <- 1 to n ) {
         treeSeq :+= t.insertChild( treeSeq( rnd.nextInt( n )), () )
      }
   }
}