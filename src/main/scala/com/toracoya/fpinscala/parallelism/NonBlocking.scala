package com.toracoya.fpinscala.parallelism

import java.util.concurrent.{Callable, CountDownLatch, ExecutorService}
import java.util.concurrent.atomic.AtomicReference

object NonBlocking {

  trait Future[A] {
    private[parallelism] def apply(k: A => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = _ => k => k(a)

  def fork[A](a: => Par[A]): Par[A] = es => k => eval(es)(a(es)(k))

  def eval(es: ExecutorService)(r: => Unit): Unit = es.submit(new Callable[Unit] {
    override def call(): Unit = r
  })

  def map2[A, B, C](p1: Par[A], p2: Par[B])(f: (A, B) => C): Par[C] = {
    es => cb => {
      var ar = Option.empty[A]
      var br = Option.empty[B]
      val combiner = Actor[Either[A, B]](es) {
        case Left(a) => br match {
          case None => ar = Some(a)
          case Some(b) => eval(es)(cb(f(a, b)))
        }
        case Right(b) => ar match {
          case None => br = Some(b)
          case Some(a) => eval(es)(cb(f(a, b)))
        }
      }
      p1(es)(a => combiner ! Left(a))
      p2(es)(b => combiner ! Right(b))
    }
  }

  def run[A](es: ExecutorService)(p: Par[A]): A = {
    val ref = new AtomicReference[A]
    val latch = new CountDownLatch(1)
    p(es) { a =>
      ref.set(a)
      latch.countDown()
    }
    latch.await()
    ref.get
  }
}
