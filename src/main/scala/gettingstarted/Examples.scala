package gettingstarted

import scala.annotation.tailrec

/**
 * Created by etuka on 23/12/14.
 */

object Examples {
  //Ex 2.1
  def fib(n: Int):Int = {
    @tailrec
    def impl(n: Int, acc1: Int, acc2: Int): Int = {
      n match {
        case 0 => acc1
        case 1 => acc2
        case _ => impl(n-1, acc2, acc1 + acc2)
      }
    }

    impl(n, 0, 1)
  }
  //Ex 2.2
  def isSorted[A](array: Array[A], op: (A,A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int, opRes: Boolean): Boolean = {
      n match {
        case 0 => loop(1, opRes)
        case m => {
          if (m == array.length - 1)
            true
          else if(opRes == op(array(n), array(n+1)))
            loop(m+1, opRes)
          else
            false
        }
      }
    }
    loop(0,op(array(0), array(1)))
  }
  //Ex 2.3
  def curry[A,B,C](f:(A,B) => C):A => (B => C) = (a: A) => {(b: B) => f(a,b)}
  //Ex 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a: A, b: B) => f(a)(b)
  //Ex 2.5
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a: A) => f(g(a))
}
