package de.sciss.lucre
package data

import stm.InMemory

/*
Output:

PRE:
a: 0, b: 188, f: 209, n: 221, s: 232, v: 243, t: 254, u: 265, o: 276, p: 287, r: 298, q: 309, g: 321, j: 332, c: 350, e: 363, m: 374, w: 385, k: 396, l: 407, d: 420, h: 432, x: 443, i: 454

POST:
v: 189, u: 200, t: 211, s: 222, r: 233, q: 244, p: 254, o: 265, n: 276, j: 289, g: 300, f: 312, w: 331, m: 342, l: 353, k: 364, e: 375, x: 389, i: 400, h: 411, d: 422, c: 436, b: 456, a: 500

 */
object ThesisPlaneTest extends App {
  val m = InMemory()
  type S = InMemory

  m.step { implicit tx =>
    type E  = TotalOrder.Set.Entry[S]
    val tot = TotalOrder.Set.empty[S](0)

    var pre   = Map[Char, E]('a' -> tot.root)
    var post  = Map[Char, E]('a' -> tot.root.appendMax)

    def insert(parent: Char, child: Char) {
      val ppre  = pre(parent)
      val cpre  = ppre.append() // 0.125
      val cpost = cpre.append() // 0.875
      pre  += child -> cpre
      post += child -> cpost
    }

    insert('a', 'b')
    insert('b', 'c')
    insert('c', 'd')
    insert('c', 'e')
    insert('b', 'f')
    insert('f', 'g')
    insert('d', 'h')
    insert('h', 'i')
    insert('g', 'j')
    insert('e', 'k')
    insert('k', 'l')
    insert('e', 'm')
    insert('f', 'n')
    insert('n', 'o')
    insert('o', 'p')
    insert('p', 'q')
    insert('p', 'r')
    insert('n', 's')
    insert('s', 't')
    insert('t', 'u')
    insert('s', 'v')
    insert('m', 'w')
    insert('h', 'x')

    def fix(m: Map[Char, E]) = m.map { case (key, e) => (key, e.tag) }

    val preSnap   = fix(pre)
    val postSnap  = fix(post)
    val max       = math.max(preSnap.maxBy(_._2)._2, postSnap.maxBy(_._2)._2)

    def scale(m: Map[Char, Int]) = {
      val rel = m.mapValues(t => math.sqrt(t.toDouble / max)).toIndexedSeq.sortBy(_._2)
      val dist = rel
//      rel.zipWithIndex.map { case ((key, t), idx) =>
//        val adj = (idx.toDouble / (rel.size - 1)) * 0.5 + t * 0.5
//        (key, adj)
//      }
      val str = dist.map {
        case (key, v) => f"$key: ${v*500}%1.0f"
      }
      str.mkString(", ")
    }

    println("PRE:")
    println(scale(preSnap))
    println("\nPOST:")
    println(scale(postSnap))
  }
}