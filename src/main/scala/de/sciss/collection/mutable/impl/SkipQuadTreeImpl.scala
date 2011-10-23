package de.sciss.collection.mutable
package impl

import de.sciss.collection.geom.{QueryShape, PointLike, DistanceMeasure}
import annotation.tailrec
import collection.mutable.{PriorityQueue, Queue => MQueue}

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

   final def rangeQuery( qs: QueryShape ) : Iterator[ A ] = new RangeQuery( qs )

   protected def insertLeaf( elem: A ) : QLeaf
   protected def removeLeaf( point: PointLike ) : QLeaf
   protected def findLeaf( point: PointLike ) : QLeaf
//   protected def nn( point: PointLike, metric: DistanceMeasure ) : QLeaf

   private final class RangeQuery( qs: QueryShape ) extends Iterator[ A ] {
      val stabbing      = MQueue.empty[ (QNode, Long) ]
      val in            = MQueue.empty[ QNonEmpty ]
      var current : A   = _
      var hasNext       = true

      stabbing += headTree -> qs.overlapArea( headTree.quad )
      findNextValue()

      def rangeQueryLeft( node: QNode, area: Long, qs: QueryShape ) : QNode = {
         var i = 0; while( i < 4 ) {
            node.child( i ) match {
               case n2: QNode =>
                  val a2 = qs.overlapArea( n2.quad )
                  if( a2 == area ) {
                     val next = node.next
                     if( next != null ) {
                        rangeQueryLeft( next, area, qs )
                     } else {
                        rangeQueryLeft( n2, a2, qs )
                     }
                  }
               case _ =>
            }
         i += 1 }
         node
      }

      def rangeQueryRight( node: QNode, area: Long, qs: QueryShape ) : QNode = {
         var i = 0; while( i < 4 ) {
            node.child( i ) match {
               case n2: QNode =>
                  val a2 = qs.overlapArea( n2.quad )
                  if( a2 == area ) {
                     val next = node.next
                     if( next != null ) {
//                        next.rangeQueryRight( area, qs )
                        rangeQueryRight( next, area, qs )
                     } else {
//                        n2.rangeQueryLeft( a2, qs )
                        rangeQueryLeft( n2, a2, qs )
                     }
//                     node.nextOption match {
//                        case Some( next ) => rangeQueryRight( next, area, qs )
//                        case None         => rangeQueryLeft( n2, a2, qs )
//                     }
                  }
               case _ =>
            }
         i += 1 }
         // at this point, we know `this` is critical
//         if( prev != null ) prev else this
         val prev = node.prev
         if( prev != null ) prev else node
//         node.prevOption.getOrElse( node )
      }

      def next() : A = {
         if( !hasNext ) throw new NoSuchElementException( "next on empty iterator" )
         val res = current
         findNextValue()
         res
      }

      def findNextValue() : Unit = while( true ) {
         if( in.isEmpty ) {
            if( stabbing.isEmpty ) {
               hasNext = false
               return
            }
            val tup  = stabbing.dequeue()
            val ns   = tup._1                         // stabbing node
            val as   = tup._2
//            val nc   = ns.rangeQueryRight( as, qs )   // critical node
            val nc   = rangeQueryRight( ns, as, qs )   // critical node
            var i = 0; while( i < 4 ) {
               nc.child( i ) match {
                  case cl: QLeaf =>
                     if( qs.contains( pointView( cl.value ))) in += cl
                  case cn: QNode =>
                     val q    = cn.quad
                     val ao   = qs.overlapArea( q )
                     if( ao > 0 ) {
                        if( ao < q.area ) {           // stabbing
                           stabbing += cn -> ao
                        } else {                      // in
                           in += cn
                        }
                     }
                  case _ =>
               }
            i += 1 }

         } else in.dequeue() match {
            case l: QLeaf =>
               current = l.value
               return
            case n: QNode =>
               var i = 0; while( i < 4 ) {
                  n.child( i ) match {
                     case ne: QNonEmpty => in += ne // sucky `enqueue` creates intermediate Seq because of varargs
                     case _ =>
                  }
               i += 1 }
         }
      }
   }

   private def nn( point: PointLike, metric: DistanceMeasure ) : QLeaf = {
      var bestLeaf: QLeaf     = null
      var bestDist            = Long.MaxValue   // all distances here are squared!
      val pri                 = PriorityQueue.empty[ VisitedNode ]
      val acceptedChildren    = new Array[ VisitedNode ]( 4 )
      var numAcceptedChildren = 0
      var rmax                = Long.MaxValue
//         val abortSq    = {
//            val al = abort.toLong
//            al * al
//         }

      def recheckRMax {
         var j = 0; while( j < numAcceptedChildren ) {
            if( acceptedChildren( j ).minDist > rmax ) {  // immediately kick it out
               numAcceptedChildren -= 1
               var k = j; while( k < numAcceptedChildren ) {
                  acceptedChildren( k ) = acceptedChildren( k + 1 )
               k += 1 }
            }
         j += 1 }
      }

      @tailrec def findNNTail( n0: QNode ) {
         numAcceptedChildren = 0
         var accept1Idx = 0
         val oldRMax1 = rmax
         var i = 0; while( i < 4 ) {
            n0.child( i ) match {
               case l: QLeaf =>
                  val ldist = metric.distance( point, pointView( l.value ))
                  if( ldist < bestDist ) {
                     bestDist = ldist
                     bestLeaf = l
                     if( bestDist < rmax ) {
//println( "      : leaf " + l.point + " - " + bestDist )
                        rmax = bestDist
                     }
                  }

               case c: QNode =>
                  val cq            = c.quad
                  val cMinDist      = metric.minDistance( point, cq )
                  if( cMinDist <= rmax ) {   // otherwise we're out already
                     val cMaxDist   = metric.maxDistance( point, cq )
                     if( cMaxDist < rmax ) {
//println( "      : node " + cq + " " + identify( c ) + " - " + cMaxDist )
                        rmax = cMaxDist
                     }
                     acceptedChildren( numAcceptedChildren ) = new VisitedNode( c, cMinDist /*, cMaxDist */)
                     accept1Idx = i
                     numAcceptedChildren += 1
                  }

               case _ =>
            }
         i += 1 }

         if( rmax != oldRMax1 ) recheckRMax

         // Unless exactly one child is accepted, round is over
         if( numAcceptedChildren != 1 ) return

         // Otherwise find corresponding node in highest level, and descend
         var dn   = acceptedChildren( 0 ).n
         val qdn  = dn.quad
         var succ = n0.next
         while( succ != null ) {
            succ.child( accept1Idx ) match {
               case dn2: QNode if( dn2.quad == qdn ) =>
                  dn    = dn2
                  succ  = succ.next
               case _ =>
                  succ  = null
            }
         }

         // now go left
         while( dn.prev != null ) dn = dn.prev
         findNNTail( dn )
      }

      var n0 = headTree
      while( true ) {
//println( "ROUND : " + identify( n0 ))
         findNNTail( n0 )
         if( bestDist <= 0L ) return bestLeaf
         var i = 0; while( i < numAcceptedChildren ) {
//println( "++ " + identify( acceptedChildren( i ).n ) + " - " + acceptedChildren( i ).minDist )
            pri += acceptedChildren( i )
         i += 1 }
         var vis: VisitedNode = null
         do {
            if( pri.isEmpty ) {
               return bestLeaf
            } else {
               vis = pri.dequeue()
            }
         } while( vis.minDist > rmax )
         n0 = vis.n
      }
      sys.error( "never here" )
   }

   private final class VisitedNode( val n: QNode, val minDist: Long /* , maxDist: Long */) extends Ordered[ VisitedNode ] {
      def compare( that: VisitedNode ) = -(minDist.compareTo( that.minDist ))
   }
}