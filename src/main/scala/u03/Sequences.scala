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

    def sum(s: Sequence[Int]): Int = s match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    /*    def map[A, B](s: Sequence[A])(mapper: A => B): Sequence[B] = s match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](s: Sequence[A])(pred: A => Boolean): Sequence[A] = s match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03
    // 1 b
    def zip[A, B](s: Sequence[A])(second: Sequence[B]): Sequence[(A, B)] =
      (s, second) match
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
        case _                            => Nil()
    // 1 a
    def take[A](l: Sequence[A])(n: Int): Sequence[A] = (l, n) match
      case (Cons(h, t), n) if n == 0 => Nil()
      case (Cons(h, t), n)           => Cons(h, take(t)(n - 1))
      case (Nil(), n)                => Nil()
    // 1 c
    def concat[A](s: Sequence[A])(s2: Sequence[A]): Sequence[A] =
      (s: Sequence[A], s2: Sequence[A]) match
        case (Cons(h1, t1), l2) => Cons(h1, concat(t1, l2))
        case (Nil(), l2)        => l2
        case _                  => Nil()

    // 1 c
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
      s match
        case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
        case _          => Nil()

    // 1 d
    def mapWithFlatMap[A, B](s: Sequence[A])(mapper: A => B): Sequence[B] =
      flatMap(s)(v => Cons(mapper(v), Nil()))

    def filterWithFlatMap[A](s: Sequence[A])(pred: A => Boolean): Sequence[A] =
      flatMap(s)(v =>
        v match
          case v if pred(v) => Cons(v, Nil())
          case _            => Nil()
      )

    // 2
    def min(s: Sequence[Int]): Optional[Int] =
      import Optional.*
      s match
        case Cons(head, Nil()) => Just(head)
        case Cons(head, tail)  => min(filter(s)(_ <= head))
        case _                 => Empty()

    def minWithoutFilter(s: Sequence[Int]): Optional[Int] =
      import Optional.*
      def _min(l: Sequence[Int], min: Int): Int = l match
        case Cons(h, t) if h < min => _min(t, h)
        case Cons(_, t)            => _min(t, min)
        case _                     => min
      s match
        case Cons(h, t) => Just(_min(t, h))
        case _          => Empty()

    // 3
    def coursesOfTeachers(s: Sequence[Person]): Sequence[String] =
      flatMap(s)(v =>
        v match
          case Teacher(n, c) => Cons(c, Nil())
          case _             => Nil()
      )

    // 4
    def foldLeft[A](s: Sequence[A])(starting: A)(
        mapper: (a: A, b: A) => A
    ): A = s match
      case Cons(h, t) => foldLeft(t)(mapper(starting, h))(mapper)
      case _          => starting
     */
    // 5
    extension [A](s: Sequence[A])
      def map[B](mapper: A => B): Sequence[B] = s match
        case Cons(h, t) => Cons(mapper(h), t.map(mapper))
        case Nil()      => Nil()

      def mapWithFlatMap[B](mapper: A => B): Sequence[B] =
        s.flatMap(v => Cons(mapper(v), Nil()))

      def filterWithFlatMap(pred: A => Boolean): Sequence[A] = s.flatMap(v =>
        v match
          case v if pred(v) => Cons(v, Nil())
          case _            => Nil()
      )

      def filter(pred: A => Boolean): Sequence[A] = s match
        case Cons(h, t) if pred(h) => Cons(h, t.filter(pred))
        case Cons(_, t)            => t.filter(pred)
        case Nil()                 => Nil()

      def zip[B](second: Sequence[B]): Sequence[(A, B)] = (s, second) match
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), t1.zip(t2))
        case _                            => Nil()

      def take(n: Int): Sequence[A] = (s, n) match
        case (Cons(h, t), n) if n == 0 => Nil()
        case (Cons(h, t), n)           => Cons(h, t.take(n - 1))
        case (Nil(), n)                => Nil()

      def concat(s2: Sequence[A]): Sequence[A] = (s, s2) match
        case (Cons(h1, t1), l2) => Cons(h1, t1.concat(l2))
        case (Nil(), l2)        => l2
        case _                  => Nil()

      def flatMap[B](mapper: A => Sequence[B]): Sequence[B] = s match
        case Cons(h, t) => mapper(h).concat(t.flatMap(mapper))
        case _          => Nil()

      def coursesOfTeachers: Sequence[String] = s.flatMap(v =>
        v match
          case Teacher(n, c) => Cons(c, Nil())
          case _             => Nil()
      )

      def foldLeft(starting: A)(mapper: (a: A, b: A) => A): A = s match
        case Cons(h, t) => t.foldLeft(mapper(starting, h))(mapper)
        case _          => starting

    extension (s: Sequence[Int])
      def min: Optional[Int] =
        import Optional.*
        s match
          case Cons(head, Nil()) => Just(head)
          case Cons(head, tail)  => s.filter(_ <= head).min
          case _                 => Empty()

      def minWithoutFilter: Optional[Int] =
        import Optional.*
        def _min(l: Sequence[Int], min: Int): Int = l match
          case Cons(h, t) if h < min => _min(t, h)
          case Cons(_, t)            => _min(t, min)
          case _                     => min
        s match
          case Cons(h, t) => Just(_min(t, h))
          case _          => Empty()

@main def trySequences =
  import Sequences.*
  val l =
    Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
