package u03

import Optionals.Optional.*
import org.junit.*
import org.junit.Assert.*
import u02.Modules.Person.*

class SequenceTest:
  import u03.Sequences.*
  import Sequence.*

  val l: Sequence[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testTake() =
    assertEquals(Cons(10, Cons(20, Nil())), take(l)(2))
    assertEquals(Cons(10, Cons(20, Cons(30, Nil()))), take(l)(3))
    assertEquals(Nil(), take(l)(0))
    assertEquals(Nil(), take(Nil())(2))

  @Test def testZip() =
    val l2: Sequence[String] = Cons("10", Cons("20", Cons("30", Nil())))
    assertEquals(
      Cons((10, "10"), Cons((20, "20"), Cons((30, "30"), Nil()))),
      zip(l)(l2)
    )
    assertEquals(Nil(), zip(l)(Nil()))
    assertEquals(Nil(), zip(Nil())(l2))
    assertEquals(Nil(), zip(Nil())(Nil()))

  @Test def testConcat() =
    val l2: Sequence[Int] = Cons(40, Cons(50, Nil()))
    assertEquals(
      Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))),
      concat(l)(l2)
    )
    assertEquals(Cons(40, Cons(50, Nil())), concat(Nil())(l2))

  @Test def testFlatMap() =
    assertEquals(
      Cons(11, Cons(21, Cons(31, Nil()))),
      flatMap(l)(v => Cons(v + 1, Nil()))
    )
    assertEquals(Nil(), flatMap(Nil())(v => Cons(v, Nil())))

  @Test def testMin() =
    assertEquals(Just(10), min(l))
    assertEquals(Just(1), min(Cons(1, Nil())))
    assertEquals(Empty(), min(Nil()))

  @Test def testCourseOfTeachers =
    val teachers = Cons(
      Teacher("mirko", "pps"),
      Cons(Teacher("carb", "programmazione"), Nil())
    )
    val expected = Cons("pps", Cons("programmazione", Nil()))
    assertEquals(expected, coursesOfTeachers(teachers))

  @Test def testMapWithFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapWithFlatMap(l)(_ + 1))
    assertEquals(
      Cons("10", Cons("20", Cons("30", Nil()))),
      mapWithFlatMap(l)(_ + "")
    )

  @Test def testFilterWithFlatMap() =
    assertEquals(Cons(20, Cons(30, Nil())), filterWithFlatMap(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterWithFlatMap(l)(_ != 20))

  @Test def testMinWithoutFilter() =
    assertEquals(Just(10), minWithoutFilter(l))
    assertEquals(Just(1), minWithoutFilter(Cons(1, Nil())))
    assertEquals(Empty(), minWithoutFilter(Nil()))
 
  @Test def testFoldLeft() =
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    val folded = foldLeft(lst)(0)(_ - _)
    assertEquals(-16, folded)
