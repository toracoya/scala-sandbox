package com.toracoya.fpinscala.laziness

import com.toracoya.fpinscala.laziness.Stream._

import scala.annotation.tailrec

trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 =>
      cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty[A])
    case _ => empty[A]
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t() takeWhile p)
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean = {
    foldRight(false)((a, b) => p(a) || b)
    /*
    this match {
      case Cons(h, t) => p(h()) || t().exists(p)
      case _ => false
    }*/
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = {
    foldRight(true)((a, b) => p(a) && b)
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = {
    foldRight(empty[A]) { (a, b) =>
      if (p(a)) {
        cons(a, b)
      } else {
        empty
      }
    }
  }

  def headOptionViaFoldRight: Option[A] = {
    foldRight(None[A]) { (a, _) => Option(a) }
  }

  def map[B](f: A => B): Stream[B] = {
    foldRight(empty[B]){ (a, b) =>
      cons(f(a), b)
    }
  }

  def filter(p: A => Boolean): Stream[A] = {
    foldRight(empty[A]){ (a, b) =>
      if (p(a)) {
        cons(a, b)
      } else {
        b
      }
    }
  }

  def append[B >: A](s: => Stream[B]): Stream[B] = {
    foldRight(s)(cons)
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B]) { (a, b) =>
      f(a).append(b)
    }
  }

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) Empty
    else cons(as.head, apply(as.tail: _*))
  }

  def from(n: Int): Stream[Int] = {
    cons(n, from(n + 1))
  }

  def flibs(): Stream[Int] = {
    def inner(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, inner(f1, f0 + f1))
    }
    inner(0, 1)
  }
}
