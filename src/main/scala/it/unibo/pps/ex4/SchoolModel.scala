package it.unibo.pps.ex4

import it.unibo.pps.util.Sequences

import Sequences.*

import scala.annotation.tailrec

object SchoolModel:

  trait SchoolModule:
    type School
    type Teacher
    type Course

    def teacher(name: String): Teacher
    def course(name: String): Course
    def emptySchool: School

    extension (school: School)
      def courses: Sequence[String]
      def teachers: Sequence[String]
      def setTeacherToCourse(teacher: Teacher, course: Course): School
      def coursesOfATeacher(teacher: Teacher): Sequence[Course]
      def hasTeacher(name: String): Boolean
      def hasCourse(name: String): Boolean

  object BasicSchoolModule extends SchoolModule:

    case class TeacherImpl(name: String)
    case class CourseImpl(name: String)
    case class SchoolImpl(
                           allTeachers: Sequence[TeacherImpl],
                           allCourses: Sequence[CourseImpl],
                           teacherToCourses: Sequence[(TeacherImpl, CourseImpl)]
                         )

    override type Teacher = TeacherImpl
    override type Course = CourseImpl
    override type School = SchoolImpl

    override def teacher(name: String): Teacher =
      TeacherImpl(name)

    override def course(name: String): Course =
      CourseImpl(name)

    override def emptySchool: School =
      SchoolImpl(Sequence.empty, Sequence.empty, Sequence.empty)

    extension (school: School)

      override def courses: Sequence[String] =
        school.allCourses.map(_.name)

      override def teachers: Sequence[String] =
        school.allTeachers.map(_.name)

      override def setTeacherToCourse(teacher: Teacher, course: Course): School =
        val updatedTeachers =
          if containsByName(school.allTeachers, _.name, teacher.name) then school.allTeachers
          else append(school.allTeachers, teacher)

        val updatedCourses =
          if containsByName(school.allCourses, _.name, course.name) then school.allCourses
          else append(school.allCourses, course)

        val updatedAssignments =
          if containsAssignment(school.teacherToCourses, teacher, course) then school.teacherToCourses
          else append(school.teacherToCourses, (teacher, course))

        SchoolImpl(updatedTeachers, updatedCourses, updatedAssignments)

      override def coursesOfATeacher(teacher: Teacher): Sequence[Course] =
        school.teacherToCourses
          .filter(_._1 == teacher)
          .map(_._2)

      override def hasTeacher(name: String): Boolean =
        containsByName(school.allTeachers, _.name, name)

      override def hasCourse(name: String): Boolean =
        containsByName(school.allCourses, _.name, name)

    private def append[A](seq: Sequence[A], elem: A): Sequence[A] = seq match
      case Sequence.Nil()      => Sequence.Cons(elem, Sequence.Nil())
      case Sequence.Cons(h, t) => Sequence.Cons(h, append(t, elem))

    @tailrec
    private def containsByName[A](seq: Sequence[A], extractName: A => String, target: String): Boolean = seq match
      case Sequence.Cons(h, _) if extractName(h) == target => true
      case Sequence.Cons(_, t)                             => containsByName(t, extractName, target)
      case Sequence.Nil()                                  => false

    @tailrec
    private def containsAssignment(
                                    assignments: Sequence[(Teacher, Course)],
                                    teacher: Teacher,
                                    course: Course
                                  ): Boolean = assignments match
      case Sequence.Cons((t, c), _) if t == teacher && c == course => true
      case Sequence.Cons(_, tail)                                  => containsAssignment(tail, teacher, course)
      case Sequence.Nil()                                          => false

@main def examples(): Unit =
  import SchoolModel.BasicSchoolModule.*

  val school = emptySchool
  println(school.teachers)
  println(school.courses)
  println(school.hasTeacher("John"))
  println(school.hasCourse("Math"))

  val john = teacher("John")
  val math = course("Math")
  val italian = course("Italian")

  val school2 = school.setTeacherToCourse(john, math)
  println(school2.teachers)
  println(school2.courses)
  println(school2.hasTeacher("John"))
  println(school2.hasCourse("Math"))
  println(school2.hasCourse("Italian"))

  val school3 = school2.setTeacherToCourse(john, italian)
  println(school3.teachers)
  println(school3.courses)
  println(school3.hasTeacher("John"))
  println(school3.hasCourse("Math"))
  println(school3.hasCourse("Italian"))
  println(school3.coursesOfATeacher(john))