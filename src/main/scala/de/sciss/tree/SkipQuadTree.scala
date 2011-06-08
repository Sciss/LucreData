package de.sciss.tree

import collection.mutable.{Map => MMap}

trait SkipQuadTree[ V ] extends MMap[ Point, V ] {
   def +=( point: Point, value: V ) : this.type
}