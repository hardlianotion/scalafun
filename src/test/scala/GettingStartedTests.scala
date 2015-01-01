/**
 * Created by etuka on 23/12/14.
 */

import datastructures.{Cons, Nil}
import gettingstarted.Examples
import Examples.{fib, isSorted}
import org.scalatest._

class GettingStartedTests extends FunSuite {

  test("fib(0) = Fib_n") {
    assert(fib(0) == 0)
    assert(fib(1) == 1)
    assert(fib(2) == 1)
    assert(fib(3) == 2)
    assert(fib(4) == 3)
    assert(fib(5) == 5)
    assert(fib(6) == 8)
    assert(fib(7) == 13)
  }

  test("Sorting function should distinguish between sorted and unsorted arrays.") {

    val sorted = Array(2,5,6,7,8,12)
    val unsorted = Array(2.0, 4.9, 5.0, 3.2, 7.6)

    assert(isSorted(sorted, (x: Int,y: Int) => x < y) == true)
    assert(isSorted(unsorted, (x: Double,y: Double) => x < y) == false)
  }
}

class DataStructuresTests extends FunSuite {

  test("Testing pattern matching to extract the correct cons statement.") {
    assert(datastructures.List.x == 3)
  }

  test("List should have a proper tail if non-trivial.") {
    val x = datastructures.List(1,2,3,4,5)
    assert(datastructures.List.tail(x) == datastructures.List(2,3,4,5))
  }

  test("Setting the head should not affect the tail.") {
    val list = datastructures.List(2,2,3,4,5)
    val tail = datastructures.List.tail(list)
    val newList = datastructures.List.setHead(list, 1)

    assert(datastructures.List.tail(newList) == tail)
    assert(datastructures.List.setHead(Cons(2, Nil),1) == Cons(1, Nil) )
  }

  test("drop 3 elements should leave a 5-list with two elements.") {
    val list = datastructures.List(1,2,3,4,5)

    assert(datastructures.List.drop(list, 3) == Cons(4,Cons(5,Nil)))
    assert(datastructures.List.drop(list,5) == Nil)
  }

  test("dropWhile should drop negative numbers.") {
    val list = datastructures.List(-1, 1, 2, -2, 3)
    val list2 = datastructures.List(1,-1, 2, -2)
    val list3 = datastructures.List(-1)
    val list4 = datastructures.List(1)
    val list5 = datastructures.List(-1,-1, -2, -2)
    val list6 = datastructures.List(-1,-1, -2, 2)

    assert(datastructures.List.dropWhile(list, (x: Int) => x < 0) == Cons(1, Cons(2, Cons(-2, Cons(3, Nil)))))
    assert(datastructures.List.dropWhile(list2, (x: Int) => x < 0) == Cons(1, Cons(-1, Cons(2, Cons(-2, Nil)))))
    assert(datastructures.List.dropWhile(list3, (x: Int) => x < 0) == Nil)
    assert(datastructures.List.dropWhile(list4, (x: Int) => x < 0) == Cons(1, Nil))
    assert(datastructures.List.dropWhile(list5, (x: Int) => x < 0) == Nil)
    assert(datastructures.List.dropWhile(list6, (x: Int) => x < 0) == Cons(2, Nil))

    assert(datastructures.List.dropWhile2(list)(_ < 0) == Cons(1, Cons(2, Cons(-2, Cons(3, Nil)))))
    assert(datastructures.List.dropWhile2(list2)(_ < 0) == Cons(1, Cons(-1, Cons(2, Cons(-2, Nil)))))
    assert(datastructures.List.dropWhile2(list3)(_ < 0) == Nil)
    assert(datastructures.List.dropWhile2(list4)(_ < 0) == Cons(1, Nil))
    assert(datastructures.List.dropWhile2(list5)(_ < 0) == Nil)
    assert(datastructures.List.dropWhile2(list6)(_ < 0) == Cons(2, Nil))
  }

  test("init should remove all but last elt.") {
    val list = datastructures.List(1,2,3)
    val list2 = datastructures.List(1)
    assert(datastructures.List.init(list) == Cons(1, Cons(2, Nil)))
    assert(datastructures.List.init(list2) == Nil)
  }

  test("Sum product and length of a list with foldLeft.") {
    val list = datastructures.List(1,2,3,4)

    assert(datastructures.List.sum(list) == 10)
    assert(datastructures.List.productL(list) == 24)
    assert(datastructures.List.lengthL(list) == 4)
    assert(datastructures.List.lengthR(list) == 4)
  }
}
