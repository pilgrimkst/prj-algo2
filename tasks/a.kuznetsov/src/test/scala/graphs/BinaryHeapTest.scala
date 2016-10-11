package graphs

import org.scalatest.{FunSuite, Matchers}

class BinaryHeapTest extends FunSuite with Matchers {

  test("poll() On empty heap should throw nu such element exception") {
    val heap = new BinaryHeap()
    assertThrows[NoSuchElementException]({
      heap.poll()
    })
  }

  test("peek() On empty heap should throw nu such element exception") {
    val heap = new BinaryHeap()
    assertThrows[NoSuchElementException]({
      heap.peek()
    })
  }

  test("size() On empty heap should return 0") {
    val heap = new BinaryHeap()
    heap.size() should be(0)
  }

  test("BinaryHeap.heapify should build heap from collection") {
    val xs = Seq(1, 5, 4, 8, 3, 9)
    val h = graphs.BinaryHeap(xs)

    h.poll() should be(1)
    h.poll() should be(3)
    h.poll() should be(4)
    h.poll() should be(5)
    h.poll() should be(8)
    h.poll() should be(9)
  }

  test("BinaryHeap.push should add elements to heap") {
    val h = new BinaryHeap()
    h.push(10)
    h.push(2)
    h.push(3)
    h.size should be(3)

  }

  test("BinaryHeap.peek should return minimum element from heap") {
    val h = new BinaryHeap()

    h.push(10)
    h.peek() should be(10)

    h.push(2)
    h.peek() should be(2)

    h.push(3)
    h.peek() should be(2)

  }

  test("BinaryHeap.poll should eject minimum element from heap") {
    val h = new BinaryHeap()

    h.push(10)
    h.push(2)

    h.poll() should be(2)


    h.push(3)
    h.poll() should be(3)

    h.push(100)

    h.poll() should be(10)
    h.poll() should be(100)

  }
}
