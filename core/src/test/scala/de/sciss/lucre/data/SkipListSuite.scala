package de.sciss.lucre
package data

import collection.immutable.IntMap
import collection.mutable.{Set => MSet}
import org.scalatest.{GivenWhenThen, FeatureSpec}
import concurrent.stm.{InTxn, TxnExecutor, Ref}
import stm.store.BerkeleyDB
import java.io.File
import stm.{Cursor, Durable, InMemory, Sys}

/*
 To run this test copy + paste the following into sbt:

test-only de.sciss.collection.txn.SkipListSuite

 */
class SkipListSuite extends FeatureSpec with GivenWhenThen {
  val CONSISTENCY   = true
  val OBSERVATION   = true
  val REMOVAL       = true
  val TWO_GAP_SIZES = true
  val INMEMORY      = true
  val DATABASE      = true

  // large
  val NUM1 = 0x040000
  val NUM2 = 0x020000 // 0x100000

  require(NUM1 >= 1 && NUM2 >= 6)

  // small
  val NUM3 = 10

  val SEED = 0L

  val rnd = new util.Random(SEED)

  def withSys[S <: Sys[S] with Cursor[S]](sysName: String, sysCreator: () => S, sysCleanUp: S => Unit): Unit = {
    if (TWO_GAP_SIZES) {
      withList[S]("HA-1 (" + sysName + ")", { oo =>
        implicit val sys = sysCreator()
        val l = sys.step { implicit tx =>
          HASkipList.Set.empty[S, Int](minGap = 1, keyObserver = oo)
        }
        (sys, l, () => sysCleanUp(sys))
      })
    }
    withList[S]("HA-2 (" + sysName + ")", { oo =>
      implicit val sys = sysCreator()
      val l = sys.step { implicit tx =>
        HASkipList.Set.empty[S, Int](minGap = 2, keyObserver = oo)
      }
      (sys, l, () => sysCleanUp(sys))
    })
  }

  if (INMEMORY) withSys("Mem", () => InMemory(), (_: InMemory) => ())
  if (DATABASE) {
    withSys[Durable]("BDB", () => {
      val dir = File.createTempFile("skiplist", "_database")
      dir.delete()
      dir.mkdir()
      println(dir.getAbsolutePath)
      Durable(BerkeleyDB.open(dir)): Durable /* please IDEA */
    }, bdb => {
      val sz = bdb.step(bdb.numUserRecords(_))
      //         println( "FINAL DB SIZE = " + sz )
      assert(sz == 0, "Final DB user size should be 0, but is " + sz)
      bdb.close()
    })
  }

  def atomic[A](fun: InTxn => A): A = TxnExecutor.defaultAtomic(fun)

  def randFill[S <: Sys[S]](l: SkipList.Set[S, Int], s: MSet[Int])(implicit cursor: Cursor[S]): Unit = {
    Given("a randomly filled structure")
    for (i <- 0 until NUM1) {
      val x = rnd.nextInt(0x7FFFFFFF)
      //println( "i = " + i + " ; x = " + x )
      //if( i == 3 ) {
      //   println()
      //}
      s.add(x)
      //println( "\n\n#" + (i+1) + " added " + x + "\n" )
      //println( atomic { implicit tx => l.debugPrint })
    }
    cursor.step { implicit tx =>
      s.foreach(l.add)
    }
  }

  def randFill2: Set[Int] = {
    Given("a set of random numbers")
    var res = Set.empty[Int]
    for (i <- 0 until NUM2) {
      res += rnd.nextInt() & ~1 // any int except MaxValue
    }
    res
  }

  def randFill3: Set[Int] = {
    var res = Set.empty[Int]
    for (i <- 0 until NUM3) {
      res += rnd.nextInt(100)
    }
    Given("a small set of numbers : " + res.mkString(", "))
    res
  }

  def verifyOrder[S <: Sys[S]](l: SkipList.Set[S, Int])(implicit cursor: Cursor[S]): Unit = {
    When("the structure is mapped to its pairwise comparisons")
    //atomic { implicit tx => println( l.toList )}
    var res = Set.empty[Int]
    val seq = cursor.step(l.toIndexedSeq(_))
    //      val iter = atomic( l.iterator( _ ))
    var prev = -2
    seq.foreach { next =>
      res += prev compare next
      prev = next
    }
    res

    Then("the resulting set should only contain -1")
    assert(res == Set(-1), res.toString())
  }

  def verifyElems[S <: Sys[S]](l: SkipList.Set[S, Int], s: MSet[Int])(implicit cursor: Cursor[S]): Unit = {
    When("the structure l is compared to an independently maintained set s")
    val ll      = cursor.step(l.toIndexedSeq(_))
    val onlyInS = cursor.step { implicit tx => s.filterNot(l.contains) }
    val onlyInL = ll.filterNot(s.contains)
    val szL     = cursor.step { implicit tx => l.size }
    val szS     = s.size
    Then("all elements of s should be contained in l")
    assert(onlyInS.isEmpty, onlyInS.take(10).toString())
    Then("all elements of l should be contained in s")
    assert(onlyInL.isEmpty, onlyInL.take(10).toString())
    Then("both should report the same size")
    assert(szL == szS, "skip list has size " + szL + " / set has size " + szS)

    When("the structure l is compared to the output from its iterator")
    Then("both should have the same size")
    assert(ll.size == szL, "skip list has size " + szL + " / iterator has size " + ll.size)
  }

  def verifyContainsNot[S <: Sys[S]](l: SkipList.Set[S, Int], s: MSet[Int])(implicit cursor: Cursor[S]): Unit = {
    When("the structure l is queried for keys not in the independently maintained set s")
    var testSet = Set.empty[Int]
    val inL = cursor.step { implicit tx =>
      while (testSet.size < 100) {
        val x = rnd.nextInt()
        if (!s.contains(x)) testSet += x
      }
      testSet.filter(l.contains)
    }
    Then("none of them should be contained in l")
    assert(inL.isEmpty, inL.take(10).toString())
  }

  def verifyAddRemoveAll[S <: Sys[S]](l: SkipList.Set[S, Int], s: MSet[Int])(implicit cursor: Cursor[S]): Unit = {
    When("all elements of the independently maintained set are added again to l")
    val szBefore  = cursor.step { implicit tx => l.size }
    val newInL    = cursor.step { implicit tx => s.filter(l.add) }
    val szAfter   = cursor.step { implicit tx => l.size }
    Then("none of the add operations should return 'true'")
    assert(newInL.isEmpty, newInL.take(10).toString())
    Then("the size of l should not change")
    assert(szBefore == szAfter, "l had size " + szBefore + " before, but now reports " + szAfter)

    if (REMOVAL) {
      When("all elements of the independently maintained set are removed from l")
      val keptInL   = cursor.step { implicit tx => s.filterNot(l.remove) }
      val szAfter2  = cursor.step { implicit tx => l.size }
      Then("all of the remove operations should return 'true'")
      assert(keptInL.isEmpty, "the following elements were not found in removal: " + keptInL.take(10).toString())
      Then("the size of l should be zero")
      assert(szAfter2 == 0, szAfter2.toString)
    }
  }

  private def withList[S <: Sys[S]](name: String,
                                    lf: SkipList.KeyObserver[S#Tx, Int] => (S with Cursor[S], SkipList.Set[S, Int], () => Unit)): Unit = {
    def scenarioWithTime(descr: String)(body: => Unit): Unit =
      scenario(descr) {
        val t1 = System.currentTimeMillis()
        body
        val t2 = System.currentTimeMillis()
        println("For " + name + " the tests took " + TestUtil.formatSeconds((t2 - t1) * 0.001))
      }

    if (CONSISTENCY) {
      feature("The " + name + " skip list structure should be consistent") {
        info("Several mass operations on the structure")
        info("are tried and expected behaviour verified")

        scenarioWithTime("Consistency is verified on a randomly filled structure") {
          val (_sys, l, cleanUp) = lf(SkipList.NoKeyObserver)
          implicit val system = _sys
          try {
            val s = MSet.empty[Int]
            randFill(l, s)
            verifyOrder(l)
            verifyElems(l, s)
            verifyContainsNot(l, s)
            verifyAddRemoveAll(l, s)
            system.step { implicit tx =>
              l.dispose()
            }
          } finally {
            cleanUp()
          }
        }
      }
    }

    if (OBSERVATION) {
      feature("The " + name + " skip list structure should be observable") {
        info("Several operations are performed on the structure while an")
        info("observer monitors key promotions and demotions")

        scenarioWithTime("Observation is verified on a randomly filled structure") {
          val obs = new Obs[S]
          val (_sys, l, cleanUp) = lf(obs)
          implicit val system = _sys
          try {
            val s = randFill2 // randFill3
            When("all the elements of the set are added")
            var uppedInsKey = false
            system.step { implicit tx =>
              s.foreach { i =>
                obs.oneUp.transform(_ - i)(tx.peer)
                l.add(i)
                uppedInsKey |= obs.oneUp()(tx.peer).contains(i)
              }
            }
            Then("none was ever promoted during their insertion")
            assert(!uppedInsKey)
            Then("there haven't been any demotions")
            assert(obs.allDn.single().isEmpty, obs.allDn.single().take(10).toString())
            Then("there were several promotions")
            assert( obs.allUp.single().nonEmpty )
            Then("the height of the list is equal to the maximally promoted key + 1")
            val maxProm = obs.allUp.single().maxBy(_._2)._2
            val h       = system.step { implicit tx => l.height }
            //println( "height = " + h )
            assert(h == maxProm + 1, "Height is reported as " + h + ", while keys were maximally promoted " + maxProm + " times")
            val sz      = s.size + 1 // account for the 'maxKey'
            val minH    = math.ceil(math.log(sz) / math.log(l.maxGap + 1)).toInt
            val maxH    = math.ceil(math.log(sz) / math.log(l.minGap + 1)).toInt
            Then("ceil(log(n+1)/log(maxGap+1)) <= height <= ceil(log(n+1)/log(minGap+1))")
            assert(minH <= h && h <= maxH, "Not: " + minH + " <= " + h + " <= " + maxH)

            if (REMOVAL) {
              When("all the elements are removed again")
              var uppedDelKey = false
              system.step { implicit tx =>
                s.foreach { i =>
                  obs.oneUp.transform(_ - i)(tx.peer)
                  l.remove(i)
                  uppedDelKey |= obs.oneUp()(tx.peer).contains(i)
                }
              }
              Then("none was ever promoted during their deletion")
              assert(!uppedDelKey, "elements were promoted during their deletion")
              val upsNotInS = obs.allUp.single().keys.filterNot(s.contains)
              Then("no key was ever promoted which was not in s")
              assert(upsNotInS.isEmpty, upsNotInS.take(10).toString())
              val dnsNotInS = obs.allDn.single().keys.filterNot(s.contains)
              Then("no key was ever demoted which was not in s")
              assert(dnsNotInS.isEmpty, dnsNotInS.take(10).toString())
              val unbal = atomic { implicit tx =>
                s.map(i => i -> (obs.allUp().getOrElse(i, 0) - obs.allDn().getOrElse(i, 0))).filterNot(_._2 == 0)
              }
              Then("the number of promotions and demotions for every key is equal")
              assert(unbal.isEmpty, unbal.take(10).toString())
            }
            system.step { implicit tx =>
              l.dispose()
            }

          } finally {
            cleanUp()
          }
        }
      }
    }
  }

  final class Obs[S <: Sys[S]] extends SkipList.KeyObserver[S#Tx, Int] {
    val allUp = Ref(IntMap.empty[Int])
    val allDn = Ref(IntMap.empty[Int])
    val oneUp = Ref(IntMap.empty[Int])

    def keyUp(key: Int)(implicit tx: S#Tx) : Unit ={
      implicit val itx = tx.peer
      allUp.transform(m => m + (key -> (m.getOrElse(key, 0) + 1)))
      oneUp.transform(m => m + (key -> (m.getOrElse(key, 0) + 1)))
    }

    def keyDown(key: Int)(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      allDn.transform(m => m + (key -> (m.getOrElse(key, 0) + 1)))
    }
  }
}