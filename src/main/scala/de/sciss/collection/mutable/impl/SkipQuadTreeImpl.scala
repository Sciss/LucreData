package de.sciss.collection.mutable
package impl

import de.sciss.collection.geom.{PointLike, DistanceMeasure}

trait SkipQuadTreeImpl[ A ] extends SkipQuadTree[ A ] {
   // ---- map support ----

   final def +=( elem: A ) : this.type = {
      insertLeaf( elem )
      this
   }

   final override def add( elem: A ) : Boolean = {
      val oldLeaf = insertLeaf( elem )
      if( oldLeaf == null ) true else oldLeaf.value != elem
   }

   final def update( elem: A ) : Option[ A ] = {
      val oldLeaf = insertLeaf( elem )
      if( oldLeaf == null ) None else Some( oldLeaf.value )
   }

   final override def remove( elem: A ) : Boolean = {
      val oldLeaf = removeLeaf( pointView( elem ))
      oldLeaf != null
   }

   final def removeAt( point: PointLike ) : Option[ A ] = {
      val oldLeaf = removeLeaf( point )
      if( oldLeaf == null ) None else Some( oldLeaf.value )
   }

   final def -=( elem: A ) : this.type = {
      removeLeaf( pointView( elem ))
      this
   }

   final override def contains( elem: A ) : Boolean = {
      val point = pointView( elem )
      if( !quad.contains( point )) return false
      val l = findLeaf( point )
      if( l == null ) false else l.value == elem
   }

   final override def isDefinedAt( point: PointLike ) : Boolean = {
      if( !quad.contains( point )) return false
      findLeaf( point ) != null
   }

   final def get( point: PointLike ) : Option[ A ] = {
      if( !quad.contains( point )) return None
      val l = findLeaf( point )
      if( l == null ) None else Some( l.value )
   }

   final override def nearestNeighbor( point: PointLike, metric: DistanceMeasure ) : A = {
      val res = nn( point, metric )
      if( res != null ) res.value else throw new NoSuchElementException( "nearestNeighbor on an empty tree" )
   }

   final def nearestNeighborOption( point: PointLike, metric: DistanceMeasure ) : Option[ A ] = {
      val res = nn( point, metric )
      if( res != null ) Some( res.value ) else None
   }

   final override def isEmpty : Boolean = {
      val n = headTree
      var i = 0; while( i < 4 ) {
         n.child( i ) match {
            case e: QEmpty =>
            case _ => return false
         }
      i += 1 }
      true
   }

   final def numLevels : Int = {
      var n = headTree
      val t = lastTree
      var i = 1; while( !(n eq t) ) {
         n = n.nextOption.orNull
         i += 1
      }
      i
   }

   protected def insertLeaf( elem: A ) : QLeaf
   protected def removeLeaf( point: PointLike ) : QLeaf
   protected def findLeaf( point: PointLike ) : QLeaf
   protected def nn( point: PointLike, metric: DistanceMeasure ) : QLeaf
}
