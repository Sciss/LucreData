package de.sciss.collection

import sys.error

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
      val root    = new Vertex( _init, TotalOrder().root, TotalOrder().root )
      val quad    = RandomizedSkipQuadTree[ Vertex[ A ]]( Quad( 0x40000000, 0x40000000, 0x40000000 ))( root )

      def insertChild( parent: Vertex[ A ], value : A ) : Vertex[ A ] = {
         val cPre    = parent.preTail.prepend() // insertBefore( () )
         val cPost   = parent.post.prepend() // insertBefore( () )
         val v       = new Vertex( value, cPre, cPost )
         quad       += v
         v
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