package graphs

trait Heap[T] {

  def peek(): T

  def poll(): T

  def push(x: T)

  def size(): Int
}

class BinaryHeap extends Heap[Int] {
  private var tail = -1
  private var data: Array[Int] = Array.fill(2)(Int.MaxValue)

  override def peek(): Int = {
    if (!nodeExists(0)) throw new NoSuchElementException()
    data(0)
  }

  override def poll(): Int = {
    val head = peek()
    if (tail > 0) {
      var cur = 0
      data(cur) = data(tail)
      while (
        (nodeExists(left(cur)) && data(cur) > data(left(cur)))
          || nodeExists(right(cur)) && data(cur) > data(right(cur))) {
        val min = minChild(cur)
        swap(data, cur, min)
        cur = min
      }
      tail -= 1
    } else {
      data(0) = Int.MaxValue
    }
    head
  }

  private def nodeExists(n: Int) = n <= tail && data(n) != Int.MaxValue

  private def minChild(n: Int) = {
    val l = left(n)
    val r = right(n)
    if (!nodeExists(r) || data(l) < data(r)) {
      l
    } else if (nodeExists(r)) {
      r
    } else {
      throw new IllegalStateException("min child executed on both empty nodes l and r")
    }
  }

  private def ensureCapacity(xs: Array[Int], index: Int): Array[Int] = {
    if (index > (xs.length - 1)) {
      val doubledSizeCopy = Array.fill(xs.length * 2) {
        Int.MaxValue
      }
      Array.copy(xs, 0, doubledSizeCopy, 0, xs.length)
      doubledSizeCopy
    } else {
      xs
    }
  }

  override def push(x: Int): Unit = {
    tail += 1
    data = ensureCapacity(data, tail)
    data(tail) = x

    var p = parent(tail)
    var c = tail
    while (p >= 0 && data(p) > data(c)) {
      swap(data, p, c)
      c = p
      p = parent(p)
    }
  }

  private def swap(xs: Array[Int], from: Int, to: Int) = {
    val tmp = xs(from)
    xs(from) = xs(to)
    xs(to) = tmp
  }

  private def left(n: Int): Int = 2 * n + 1

  private def right(n: Int): Int = 2 * n + 2

  private def parent(n: Int): Int = Math.floor((n - 1).toDouble / 2).toInt

  override def size(): Int = {
    if (nodeExists(0)) {
      tail + 1
    } else {
      0
    }
  }
}

object BinaryHeap {
  def apply(xs: Seq[Int]): Heap[Int] = {
    val h = new BinaryHeap()
    xs.foreach(x => h.push(x))
    h
  }
}
