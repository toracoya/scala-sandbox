package com.toracoya.fpinscala.parallelism

import java.util.concurrent.{Callable, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit

object Par {

  type Par[A] = ExecutorService => Future[A]

  def run[A](s : ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] = {
    map2(pa, unit())((a, _) => f(a))
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    es => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }
  }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    map2(map2(a, b)((a, b) => (c: C) => f(a, b, c)), c)(_(_))
  }

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def sequence[A](as: List[Par[A]]): Par[List[A]] = {
    map(sequenceBalanced(as.toIndexedSeq))(_.toList)
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](l: List[A])(p: A => Boolean): Par[List[A]] = {
    val pars = l map asyncF(a => if (p(a)) List(a) else Nil)
    map(sequence(pars))(_.flatten)
  }

  def fork[A](a: => Par[A]): Par[A] = {
    es =>
      es.submit(new Callable[A] {
        def call: A = a(es).get
      })
  }

  def delay[A](fa: => Par[A]): Par[A] = es => fa(es)

  def asyncF[A, B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es => {
      val ind = run(es)(n).get
      run(es)(choices(ind))
    }
  }

  def choiceViaChoiceN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map(cond)(b => if (b) 0 else 1))(List(t, f))
  }

  def choiceMap[A, B](p: Par[A])(choices: Map[A, Par[B]]): Par[B] = {
    es => {
      val k = run(es)(p).get
      run(es)(choices(k))
    }
  }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es => {
      val a = run(es)(pa).get
      run(es)(choices(a))
    }
  }

  def join[A](a: Par[Par[A]]): Par[A] = {
    flatMap(a)(identity)
  }

  def flatMap[A, B](pa: Par[A])(f: A => Par[B]): Par[B] = {
    es => {
      val a = run(es)(pa).get
      run(es)(f(a))
    }
  }

  def equal[A](e: ExecutorService)(p1: Par[A], p2: Par[A]): Boolean = {
    p1(e).get == p2(e).get
  }

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    override def isCancelled: Boolean = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }
}
