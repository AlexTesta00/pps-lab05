package it.unibo.pps.ex5

import it.unibo.pps.util.Sequences.Sequence

import scala.annotation.tailrec

trait Course:
  def name: String
  def category: String

case class CourseImpl(name: String, category: String) extends Course

object sameCategory:

  def unapply(courses: Sequence[Course]): Option[String] =
    courses match
      case Sequence.Cons(first, rest) =>
        val cat = first.category
        if allHaveCategory(rest, cat) then Some(cat) else None
      case _ => None

  @tailrec
  private def allHaveCategory(courses: Sequence[Course], cat: String): Boolean =
    courses match
      case Sequence.Nil() => true
      case Sequence.Cons(head, tail) =>
        head.category == cat && allHaveCategory(tail, cat)

@main def testExtractor(): Unit =
  val course1 = CourseImpl("Scala", "Programming")
  val course2 = CourseImpl("Java", "Programming")
  val course3 = CourseImpl("Python", "Programming")

  val courses = Sequence(course1, course2, course3)

  courses match
    case sameCategory(cat) =>
      println(s"$courses have same category $cat")
    case _ =>
      println(s"$courses have different categories")