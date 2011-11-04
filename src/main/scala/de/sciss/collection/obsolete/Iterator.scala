//package de.sciss.collection.obsolete
//
//import concurrent.stm.InTxn
//
//trait Iterator[ @specialized( Int, Long ) A ] {
//   def hasNext( implicit tx: InTxn ) : Boolean
//   def next()( implicit tx: InTxn ) : A
//}