package u03

import u02.AnonymousFunctions.l
import u03.Optionals.Optional
import u02.Modules.*
import u02.Modules.Person.*
import u02.Tuples.t
import scala.annotation.tailrec

object Sequences: // Essentially, generic linkedlists

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] =
      (first, second) match
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
        case _                            => Nil()

    def take[A](l: Sequence[A])(n: Int): Sequence[A] = (l, n) match
      case (Cons(h, t), n) if n == 0 => Nil()
      case (Cons(h, t), n)           => Cons(h, take(t)(n - 1))
      case (Nil(), n)                => Nil()

    def concat[A](l1: Sequence[A], l2: Sequence[A]): Sequence[A] =
      (l1: Sequence[A], l2: Sequence[A]) match
        case (Cons(h1, t1), l2) => Cons(h1, concat(t1, l2))
        case (Nil(), l2)        => l2
        case _                  => Nil()

    def flatMap[A, B](l: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
      l match
        case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
        case _          => Nil()

    def min(l: Sequence[Int]): Optional[Int] =
      import Optional.*
      l match
        case Cons(head, Nil()) => Just(head)
        case Cons(head, tail)  => min(filter(l)(_ <= head))
        case _                 => Empty()

    def coursesOfTeachers(teachers: Sequence[Person]): Sequence[String] =
      flatMap(teachers)(v =>
        v match
          case Teacher(n, c) => Cons(c, Nil())
          case _             => Nil()
      )

    def mapWithFlatMap[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] =
      flatMap(l)(v => Cons(mapper(v), Nil()))

    def filterWithFlatMap[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] =
      flatMap(l1)(v =>
        v match
          case v if pred(v) => Cons(v, Nil())
          case _            => Nil()
      )

    def minWithoutFilter(l: Sequence[Int]): Optional[Int] =
      import Optional.*
      def _min(l: Sequence[Int], min: Int): Int = l match
        case Cons(h, t) if h < min => _min(t, h)
        case Cons(_, t)            => _min(t, min)
        case _                     => min
      l match
        case Cons(h, t) => Just(_min(t, h))
        case _          => Empty()

    def foldLeft[A](l: Sequence[A])(starting: A)(
        mapper: (a: A, b: A) => A
    ): A = l match
      case Cons(h, t) => foldLeft(t)(mapper(starting, h))(mapper)
      case _          => starting

@main def trySequences =
  import Sequences.*
  val l =
    Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
