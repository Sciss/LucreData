package de.sciss.collection.txn

trait Ordered[ -Tx, -A ] {
   def compare( that: A )( implicit tx: Tx ) : Int
}