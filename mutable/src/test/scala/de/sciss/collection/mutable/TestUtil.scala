package de.sciss.collection
package mutable

object TestUtil {
   def formatSeconds( seconds: Double ) : String = {
      val millisR    = (seconds * 1000).toInt
      val sb         = new StringBuilder( 10 )
      val secsR      = millisR / 1000
      val millis     = millisR % 1000
      val mins       = secsR / 60
      val secs       = secsR % 60
      if( mins > 0 ) {
         sb.append( mins )
         sb.append( ':' )
         if( secs < 10 ) {
            sb.append( '0' )
         }
      }
      sb.append( secs )
      sb.append( '.' )
      if( millis < 10 ) {
         sb.append( '0' )
      }
      if( millis < 100 ) {
         sb.append( '0' )
      }
      sb.append( millis )
      sb.append( 's' )
      sb.toString()
   }
}