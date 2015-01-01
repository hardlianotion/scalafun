package datastructures

import spire.algebra.Monoid

import scala.annotation.tailrec
import spire.math.Numeric
import spire.implicits._

/**
 * Created by etuka on 26/12/2014.
 */
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def head[A](xs: List[A]): A = xs match {
    case Nil => sys.error("Empty list has no head")
    case Cons(h,_) => h
  }

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => sys.error("Empty list has no tail.")
    case Cons(_, tl) => tl
  }

  def setHead[A](xs: List[A], h: A): List[A] = xs match {
    case Nil => sys.error("Empty list has no head.")
    case Cons(oh, ot) => Cons(h,ot)
  }

  def drop[A](xs: List[A], n: Int): List[A] = {
    @tailrec
    def impl(xs: List[A], n: Int): List[A] = (xs, n) match {
      case (_,0) => xs
      case (Nil, _) => sys.error("Can't drop anything from empty list.")
      case (Cons(_, tl),_) => if (n > 0) impl(tl,n-1) else xs
    }
    if (n < 0)
      sys.error("Can't drop a negative number of items.")
    else
      impl(xs, n)
  }

  def dropWhile[A](xs: List[A], f:A => Boolean): List[A] = {
    @tailrec
    def impl(ls: List[A]): List[A] = ls match {
      case Nil => Nil
      case Cons(h,tl) => if(f(h)) impl(tl) else ls
    }
    impl(xs)
  }

  def dropWhile2[A](xs: List[A])(f:A => Boolean): List[A] = {
    @tailrec
    def impl(ls: List[A]): List[A] = ls match {
      case Nil => Nil
      case Cons(h,tl) => if(f(h)) impl(tl) else ls
    }
    impl(xs)
  }

  def reverse[A](ls: List[A]): List[A] = {
    @tailrec
    def impl(hl: List[A], tl: List[A]): List[A] = (hl, tl) match {
      case (hl, Nil) => hl
      case (hl, Cons(h,t)) => impl(Cons(h,hl), t)
    }
    impl(Nil, ls)
  }

  def init[A](ls: List[A]): List[A] = {
    @tailrec
    def impl(hl: List[A], tl: List[A]): List[A] = (hl, tl) match {
      case (hl, Nil) => reverse(hl)
      case (hl, Cons(h,Nil)) => impl(hl, Nil)
      case (hl, Cons(h,t)) => impl(Cons(h,hl), t)
    }
    impl(Nil, ls)
  }

  def foldRight[A,B](ls: List[A], z: B)(f: (A,B) => B): B = ls match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def productR(ls: List[Double]): Double = foldRight(ls, 1.0)(_ * _)

  def lengthR[A](ls: List[A]):Int = foldRight(ls, 0)((a,b) => b + 1)

  @tailrec
  def foldLeft[A,B](ls: List[A], z: B)(f: (B,A) => B): B = ls match{
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z,x))(f)
  }

  def sumL[A: Numeric](ls: List[A])(implicit A: Numeric[A]): A
    = foldLeft(ls, A.zero)(_ + _)

//  def sumL(ls: List[Int]): Int
//      = foldLeft(ls, 0)(_ + _)

  def productL(ls:List[Int]): Int
      = foldLeft(ls,1)(_ * _)

  def lengthL(ls:List[Int]): Int
  = foldLeft(ls,0)((x,y)=>x+1)

}
