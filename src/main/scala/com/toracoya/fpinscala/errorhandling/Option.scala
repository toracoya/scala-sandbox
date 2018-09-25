package com.toracoya.fpinscala.errorhandling

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case Some(get) => Some(f(get))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(get) => get
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this map (Some(_)) getOrElse ob
  }

  def filter(p: A => Boolean): Option[A] = {
    flatMap { v =>
      if (p(v)) Some(v)
      else None
    }
  }

}

case class Some[+A](get: A) extends Option[A] {

  override def map[B](f: A => B): Option[B] = Some(f(get))

  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)

  override def getOrElse[B >: A](default: => B): B = get

  override def orElse[B >: A](ob: => Option[B]): Option[B] = this

  override def filter(p: A => Boolean): Option[A] =
    if (p(get)) this
    else None
}

case object None extends Option[Nothing] {

  override def map[B](f: Nothing => B): Option[B] = None

  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => Option[B]): Option[B] = ob

  override def filter(p: Nothing => Boolean): Option[Nothing] = None
}

object Option {

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap { m =>
      val vs = xs.map { x =>
        math.pow(x - m, 2)
      }
      mean(vs)
    }
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap { aa =>
      b.map { bb =>
        f(aa, bb)
      }
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case head :: tail =>
      head.flatMap { hh =>
        sequence(tail) map (hh :: _)
      }
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((e, acc) => map2(f(e), acc)(_ :: _))
    /*
    a.foldRight[Option[List[B]]](Some(Nil)){ (e, acc) =>
      acc.map { aa =>
        f(e) match {
          case Some(v) => v :: aa
          case None => aa
        }
      }
    }
    */
  }
}
