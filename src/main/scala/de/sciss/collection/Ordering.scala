package de.sciss.collection

object Ordering {
   implicit object Int extends Ordering[ scala.Int ] {
      def compare( a: scala.Int, b: scala.Int ) : scala.Int                = if( a < b ) -1 else if( a > b ) 1 else 0
      override def lt( a: scala.Int, b: scala.Int ) : Boolean              = a < b
      override def lteq( a: scala.Int, b: scala.Int ) : Boolean            = a <= b
      override def gt( a: scala.Int, b: scala.Int ) : Boolean              = a > b
      override def gteq( a: scala.Int, b: scala.Int ) : Boolean            = a >= b
      override def equiv( a: scala.Int, b: scala.Int ) : Boolean           = a == b
      override def nequiv( a: scala.Int, b: scala.Int ) : Boolean          = a != b
      override def max( a: scala.Int, b: scala.Int ) : scala.Int           = if( a >= b ) a else b
      override def min( a: scala.Int, b: scala.Int ) : scala.Int           = if( a < b ) a else b
   }

   implicit object Float extends Ordering[ scala.Float ] {
      def compare( a: scala.Float, b: scala.Float ) : scala.Int            = if( a < b ) -1 else if( a > b ) 1 else 0
      override def lt( a: scala.Float, b: scala.Float ) : Boolean          = a < b
      override def lteq( a: scala.Float, b: scala.Float ) : Boolean        = a <= b
      override def gt( a: scala.Float, b: scala.Float ) : Boolean          = a > b
      override def gteq( a: scala.Float, b: scala.Float ) : Boolean        = a >= b
      override def equiv( a: scala.Float, b: scala.Float ) : Boolean       = a == b
      override def nequiv( a: scala.Float, b: scala.Float ) : Boolean      = a != b
      override def max( a: scala.Float, b: scala.Float ) : scala.Float     = if( a >= b ) a else b
      override def min( a: scala.Float, b: scala.Float ) : scala.Float     = if( a < b ) a else b
   }

   implicit object Long extends Ordering[ scala.Long ] {
      def compare( a: scala.Long, b: scala.Long ) : scala.Int              = if( a < b ) -1 else if( a > b ) 1 else 0
      override def lt( a: scala.Long, b: scala.Long ) : Boolean            = a < b
      override def lteq( a: scala.Long, b: scala.Long ) : Boolean          = a <= b
      override def gt( a: scala.Long, b: scala.Long ) : Boolean            = a > b
      override def gteq( a: scala.Long, b: scala.Long ) : Boolean          = a >= b
      override def equiv( a: scala.Long, b: scala.Long ) : Boolean         = a == b
      override def nequiv( a: scala.Long, b: scala.Long ) : Boolean        = a != b
      override def max( a: scala.Long, b: scala.Long ) : scala.Long        = if( a >= b ) a else b
      override def min( a: scala.Long, b: scala.Long ) : scala.Long        = if( a < b ) a else b
   }

   implicit object Double extends Ordering[ scala.Double ] {
      def compare( a: scala.Double, b: scala.Double ) : scala.Int          = if( a < b ) -1 else if( a > b ) 1 else 0
      override def lt( a: scala.Double, b: scala.Double ) : Boolean        = a < b
      override def lteq( a: scala.Double, b: scala.Double ) : Boolean      = a <= b
      override def gt( a: scala.Double, b: scala.Double ) : Boolean        = a > b
      override def gteq( a: scala.Double, b: scala.Double ) : Boolean      = a >= b
      override def equiv( a: scala.Double, b: scala.Double ) : Boolean     = a == b
      override def nequiv( a: scala.Double, b: scala.Double ) : Boolean    = a != b
      override def max( a: scala.Double, b: scala.Double ) : scala.Double  = if( a >= b ) a else b
      override def min( a: scala.Double, b: scala.Double ) : scala.Double  = if( a < b ) a else b
   }

   implicit def fromMath[ A ]( implicit underlying: math.Ordering[ A ]) : Ordering[ A ] = new Wrapper[ A ]( underlying )

   private final class Wrapper[ A ]( underlying: math.Ordering[ A ]) extends Ordering[ A ] {
      def compare( a: A, b: A ) : Int = underlying.compare( a, b )
   }
}
trait Ordering[ @specialized( Int, Float, Long, Double ) A ] {
   def compare( a: A, b: A ) : Int
   def lt( a: A, b: A ) : Boolean      = compare( a, b ) < 0
   def lteq( a: A, b: A ) : Boolean    = compare( a, b ) <= 0
   def gt( a: A, b: A ) : Boolean      = compare( a, b ) > 0
   def gteq( a: A, b: A ) : Boolean    = compare( a, b ) >= 0
   def equiv( a: A, b: A ) : Boolean   = compare( a, b ) == 0
   def nequiv( a: A, b: A ) : Boolean  = compare( a, b ) != 0
   def max( a: A, b: A ) : A           = if( compare( a, b ) >= 0 ) a else b
   def min( a: A, b: A ) : A           = if( compare( a, b ) < 0 )  a else b
}