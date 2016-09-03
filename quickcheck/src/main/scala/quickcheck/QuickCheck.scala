package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    k <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(k, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("del") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(deleteMin(h)) == math.max(a, b)
  }
  
  property("link") = forAll { (a: Int, b: Int, c: Int) =>
    val h1 = insert(a, insert(b, empty))
    val h2 = insert(c, empty)
    val h = deleteMin(meld(h1, h2))


    findMin(h) == List(a, b, c).sorted.tail.head
  }

  property("sorted") = forAll { (h: H) =>
    def elems(h: H): List[Int] =
      if (isEmpty(h)) Nil
      else findMin(h) :: elems(deleteMin(h))
    elems(h) == elems(h).sorted
  }

  property("meld") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m3 = findMin(meld(h1, h2))
    m3 == math.min(m1, m2)
  }

}
