package de.sciss.tree

trait SkipQuadTree[ V ] {
   def insert( point: Point, value: V ) : Unit
}