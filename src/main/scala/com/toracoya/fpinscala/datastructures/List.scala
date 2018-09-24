package com.toracoya.fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, tail) => tail
  }

  def setHead[A](list: List[A], e: A): List[A] = list match {
    case Nil => Cons(e, Nil)
    case Cons(_, tail) => Cons(e, tail)
  }

  def drop[A](list: List[A], n: Int): List[A] = {
    if (n <= 0) {
      list
    } else {
      list match {
        case Nil => Nil
        case Cons(_, tail) => drop(tail, n - 1)
      }
    }
  }

  def dropWhile[A](list: List[A], p: A => Boolean): List[A] = list match {
    case Cons(head, tail) if p(head) => dropWhile(tail, p)
    case _ => list
  }

  def append[A](list1: List[A], list2: List[A]): List[A] = {
    list1 match {
      case Nil => list2
      case Cons(head, tail) => Cons(head, append(tail, list2))
    }
  }

  def init[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def foldRight[A, B](list: List[A], z: B)(f: (A, B) => B): B = list match {
    case Nil => z
    case Cons(head, tail) =>
      f(head, foldRight(tail, z)(f))
  }

  def sum2(list: List[Int]): Int = foldRight(list, 0)(_ + _)

  def product2(list: List[Double]): Double = foldRight(list, 1.0)(_ * _)

  def length[A](list: List[A]): Int = foldRight(list, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](list: List[A], z: B)(f: (B, A) => B): B = list match {
    case Nil => z
    case Cons(head, tail) =>
      foldLeft(tail, f(z, head))(f)
  }

  def sum3(list: List[Int]): Int = foldLeft(list, 0)(_ + _)

  def product3(list: List[Double]): Double = foldLeft(list, 1.0)(_ * _)

  def length2[A](list: List[A]): Int = foldLeft(list, 0)((acc, _) => acc + 1)

  //def reverse[A](list: List[A]): List[A] = foldRight(list, Nil: List[A])((e, acc) => Cons(e, acc))
  def reverse[A](list: List[A]): List[A] = foldLeft(list, Nil: List[A])((acc, e) => Cons(e, acc))

  def foldRightViaFoldLeft[A, B](list: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(list), z)((b, a) => f(a, b))
  }

  def foldLeftViaFoldRight[A, B](list: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(list, (b: B) => b)((a, g) => b => g(f(a, b)))(z)
  }

}
