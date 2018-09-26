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
    foldRight(Option.empty[A]) { (a, _) => Option(a) }
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
    foldRight(s)((h, t) => cons(h, t))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    foldRight(empty[B]) { (a, b) =>
      f(a).append(b)
    }
  }

  def mapViaUnfold[B](f: A => B): Stream[B] = {
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }
  }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), nn) if nn > 1 => Some(h(), (t(), nn - 1))
      case _ => None
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), _) =>
        Some((Some(h1()), None), (t1(), empty))
      case (_, Cons(h2, t2)) =>
        Some((None, Some(h2())), (empty, t2()))
      case _ =>
        None
    }
  }

  def startsWith[A](s2: Stream[A]): Boolean = {
    zipAll(s2).takeWhile { case (_, b) =>
      b.nonEmpty
    } forAll {
      case (h, h2) => h == h2
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case s =>
        Some(s, s.drop(1))
    }
  }

  def scanRight[B](z: B)(f: => (A, => B) => B): Stream[B] = {
    foldRight((z, Stream(z))) { (a, p0) =>
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    }._2
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

  def fibs: Stream[Int] = {
    def inner(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, inner(f1, f0 + f1))
    }
    inner(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((aa, zz)) => cons(aa, unfold(zz)(f))
      case None => empty
    }
  }

  def fibsViaUnfold: Stream[Int] = {
    unfold((0, 1): (Int, Int)){ case (f0, f1) =>
      Some(f0 -> (f1, f0 + f1))
    }
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n) { a =>
      Some(a -> (a + 1))
    }
  }

  def constantViaUnfold[A](a: A): Stream[A] = {
    unfold(a) { _ =>
      Some(a -> a)
    }
  }

  def onesViaUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))
}
