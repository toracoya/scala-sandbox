package com.toracoya.sandbox

object TypeConstraints extends App {

  // http://blog.bruchez.name/2015/11/generalized-type-constraints-in-scala.html
  // https://qiita.com/mtoyoshi/items/fbf2c744e3c7fd69a438

  // Option#flatten
  //./collection/TraversableOnce.scala#toMap

  /*

  - Scalaの型制約

  - ジェネリックなメソッドに型制約をつけるもの
    - 型があわないとコンパイルエラーになる
  - Option#flattenやSeq#toMapとかで使われてる
  - 型制約の仕組み
    - Predefでの定義
    - 暗黙のパラメータの探索
  - 他のやり方
    - Upper boundの例
    - パターンマッチx? classTagを使う方法
  - まとめ

   */

  val nested = Option(Option(1))

  List(1,2,3).sum

  println(nested)
  println(nested.flatten)

  sealed trait Animal {
    def bark: Unit
  }
  case class Cat() extends Animal {

    override def bark: Unit = {
      println("にゃー")
    }
  }

  // class Pair[K, V] extends (K, V)

  case class Box[A](something: A)

  def bark[A](box: Box[A])(implicit ev: A <:< Animal): Unit = {
    box.something.bark
  }

  def bark3[A <: Animal](box: Box[A]): Unit = {
    box.something.bark
  }

  def tupleIfSubtype[T <: U, U](t: T, u: U) = (t, u)

  println(tupleIfSubtype("aaa", 10))

  def tupleIfSubtype2[T, U](t: T, u: U)(implicit ev: T <:< U) = (t, u)

  println(tupleIfSubtype2("aaa", "bbb"))

  /*
  def bark[Cat](box: Box[Cat])(implicit ev: Cat <:< Animal): Unit = {
    // implicit def $conforms[Cat]: Cat <:< Cat
    // implicit def $conforms[Cat]: Animal <:< Animal
  }*/

  val box1 = Box(Cat())
  val box2 = Box(1)

  bark(box1)

  def bark2[A](box: Box[A])(implicit ev: A =:= Animal): Unit = {
    box.something.bark
  }
  //bark2(box1)
  bark3(box1)
  //bark3(box2)

  /*
  implicit val ev: Int <:< Animal = new <:<[Int, Animal] {
    override def apply(v: Int): Animal = Cat()
  }


  bark(box2)
  */

}
