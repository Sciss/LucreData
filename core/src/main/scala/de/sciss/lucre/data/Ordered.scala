package de.sciss.lucre
package data

trait Ordered[ -Tx, -A ] {
   def compare( that: A )( implicit tx: Tx ) : Int
}