package test

import org.scalatest.mock.MockitoSugar
import org.scalatest.matchers.ShouldMatchers
import model._
import org.scalatest.GivenWhenThen
import org.scalatest.FreeSpec
import org.scalatest.Ignore
import java.util.Date

class SecondPartTest extends FreeSpec with ShouldMatchers with GivenWhenThen with MockitoSugar {

  def mocks = new {
    val subject = mock[Subject]
    val professors = mock[List[Professor]]
    val students = mock[List[Student]]
    val career = mock[Career]
    val courses = mock[List[Course]]
  }

  def fixtureCourse = new {
    val mock = mocks
    val subject = new Subject("Intro", mock.career, mock.courses)
    val students = (new Student("Cosme", "Fulanito", 23, mock.career)) :: (new Student("Killua", "Soldick", 18, mock.career)) ::
      (new Student("Gon", "Freecs", 17, mock.career)) :: (new Student("Betina", "Azul", 23, mock.career)) ::
      (new Student("Carlito", "Alfalfa", 26, mock.career)) :: (new Student("Gaston", "Char", 27, mock.career)) :: List()
  }

  "The Academic record" - {
    "should have the general performance statistics" - {
      "About courses " in {
        given("one subject with two course")
        val f = fixtureCourse
        val students = f.students
        val courseIntro = new Course(f.subject, f.mock.professors, students)
        val courseIntroSecond = new Course(f.subject, f.mock.professors, students, mock[Date])
        students(0).noteTablesAllCourses = (courseIntro, true, 6) :: List()
        students(1).noteTablesAllCourses = (courseIntro, true, 6) :: List()
        students(2).noteTablesAllCourses = (courseIntro, false, 0) :: (courseIntroSecond, true, 6) :: List()
        students(3).noteTablesAllCourses = (courseIntro, true, 2) :: (courseIntroSecond, true, 3) :: List()
        students(4).noteTablesAllCourses = (courseIntro, true, 10) :: List()
        students(5).noteTablesAllCourses = (courseIntro, true, 8) :: List()

        then("the results of approved")
        courseIntro.averageWithoutFails should be(7.5)

        and("dissaproved")
        courseIntro.averageWithFails should be(1.0)

        and("amount approved")
        courseIntro.amountSuccessful should be(4)

        and("amount abandoned")
        courseIntro.amountFailed should be(1)

        and("average approved subjects")
        courseIntro.averageApprovedSubjects should be(1)

        and("have the notes table")
        courseIntro.notesTable should have size (11)
        courseIntro.notesTable should equal((10, 1) :: (9, 0) :: (8, 1) :: (7, 0) :: (6, 2) :: (5, 0) :: (4, 0) :: (3, 0) :: (2, 1) :: (1, 0) :: (0, 1) :: List())

        and("the best note in n times")
        courseIntro.bestNoteNTimes(2) should be(6)

      }

      "About subjects " in {

        given("subject: Data Base ")
        val f = fixtureCourse
        val students = f.students
        val intro = new Subject("Intro", f.mock.career, List())
        val courseIntro = new Course(intro, f.mock.professors, students)
        val courseIntroSecond = new Course(f.subject, f.mock.professors, students, mock[Date])
        students(0).noteTablesAllCourses = (courseIntro, true, 6) :: List()
        students(1).noteTablesAllCourses = (courseIntro, true, 6) :: List()
        students(2).noteTablesAllCourses = (courseIntro, false, 0) :: (courseIntroSecond, true, 6) :: List()
        students(3).noteTablesAllCourses = (courseIntro, false, 0) :: (courseIntroSecond, true, 3) :: List()
        students(4).noteTablesAllCourses = (courseIntro, true, 10) :: List()
        students(5).noteTablesAllCourses = (courseIntro, true, 8) :: List()
        intro.courses = courseIntro :: courseIntroSecond :: List()

        then("the results of approved")
        intro.averageWithoutFails should be(7.2)

        and("dissaproved")
        intro.averageWithFails should be(1)

        and("amount approved")
        intro.amountSuccessful should be(5)

        and("amount abandoned")
        intro.amountFailed should be(2)

        and("average approved subjects")
        intro.averageApprovedSubjects should be(1)

        and("have the notes table")
        intro.notesTable should have size (11)
        intro.notesTable should equal((10, 1) :: (9, 0) :: (8, 1) :: (7, 0) :: (6, 3) :: (5, 0) :: (4, 0) :: (3, 1) :: (2, 0) :: (1, 0) :: (0, 2) :: List())

        and("the best note in n times")
        intro.bestNoteNTimes(3) should be(6)
      }
      "About career" in {

        given("this career Programming")
        val data = AcademicData.getCareer("Programming")
        val tpi = data

        then("the results of approved")
        tpi.averageWithoutFails should be(7.75)

        and("dissaproved")
        tpi.averageWithFails should be(1.58 plusOrMinus (0.1))

        and("amount approved")
        tpi.amountSuccessful should be(16)

        and("amount abandoned")
        tpi.amountFailed should be(5)

        and("average approved subjects")
        tpi.averageApprovedSubjects should be(1)

        and("have the notes table")
        tpi.notesTable should have size (11)
        tpi.notesTable should equal((10, 0) :: (9, 8) :: (8, 2) :: (7, 4) :: (6, 0) :: (5, 0) :: (4, 2) :: (3, 5) :: (2, 2) :: (1, 0) :: (0, 5) :: List())

        and("the best note in n times")
        tpi.bestNoteNTimes(4) should be(9)
      }

      "About professors " in {
        given("Three professors")
        val menganoPython = AcademicData.getProfessor("Mengano", "Python")
        val sultanoScala = AcademicData.getProfessor("Sultano", "Scala")
        val fulaHaskel = AcademicData.getProfessor("Fula", "Haskel")

        then("the results of approved")
        menganoPython.averageWithoutFails should be(8.6 plusOrMinus (0.1))
        sultanoScala.averageWithoutFails should be(8.0)
        fulaHaskel.averageWithoutFails should be(6.6 plusOrMinus (0.1))

        and("dissaproved")
        menganoPython.averageWithFails should be(1.58 plusOrMinus (0.1))
        sultanoScala.averageWithFails should be(2.6)
        fulaHaskel.averageWithFails should be(0.0)

        and("amount approved")
        menganoPython.amountSuccessful should be(6)
        sultanoScala.amountSuccessful should be(4)
        fulaHaskel.amountSuccessful should be(6)

        and("amount abandoned")
        menganoPython.amountFailed should be(5)
        sultanoScala.amountFailed should be(0)
        fulaHaskel.amountFailed should be(0)

        and("average approved subjects")
        menganoPython.averageApprovedSubjects should be(3)
        sultanoScala.averageApprovedSubjects should be(2)
        fulaHaskel.averageApprovedSubjects should be(1)

        and("have the notes table")
        menganoPython.notesTable should equal(List((10, 0), (9, 4), (8, 2), (7, 0), (6, 0), (5, 0), (4, 0), (3, 5), (2, 2), (1, 0), (0, 5)))
        sultanoScala.notesTable should equal(List((10, 0), (9, 2), (8, 0), (7, 2), (6, 0), (5, 0), (4, 0), (3, 3), (2, 2), (1, 0), (0, 0)))
        fulaHaskel.notesTable should equal(List((10, 0), (9, 2), (8, 0), (7, 2), (6, 0), (5, 0), (4, 2), (3, 0), (2, 0), (1, 0), (0, 0)))

        and("the best note in n times")
        menganoPython.bestNoteNTimes(4) should be(9)
        sultanoScala.bestNoteNTimes(3) should be(3)
        fulaHaskel.bestNoteNTimes(2) should be(9)
      }

      "About annaul average of a student" ignore {
        given(" the notes of a student ")
        val mock = mocks
        val student = new Student("Pepito", "Sabe", 26, mock.career)
        val date2012 = new Date
        date2012.setYear(2012)
        val course2012 = new Course(mock.subject, mock.professors, mock.students, date2012)
        val currentCourse = new Course(mock.subject, mock.professors, mock.students)

        student.noteTablesAllCourses = (course2012, true, 8) :: (course2012, true, 7) :: (course2012, true, 5) :: (course2012, true, 6) :: (course2012, false, 0) ::
          (course2012, false, 0) :: (course2012, true, 3) :: (course2012, true, 4) :: List()
        (currentCourse, true, 10) :: (currentCourse, true, 9) ::
          (currentCourse, true, 10) :: (currentCourse, true, 10) :: (currentCourse, false, 0) :: (currentCourse, false, 0) :: List()

        when(" we want to know the average of its notes in 2012 ")
        val dataToStudent2012: Student = student.annualAverage(2012)

        then("your table of notes")
        dataToStudent2012.noteTablesAllCourses should have size (8)

        and("the results of approved")
        dataToStudent2012.averageWithoutFails should be(6.0)

        and("dissaproved")
        dataToStudent2012.averageWithFails should be(1.0)

        and("amount approved")
        dataToStudent2012.amountSuccessful should be(5)

        and("amount abandoned")
        dataToStudent2012.amountFailed should be(2)

        and("average approved subjects")
        dataToStudent2012.averageApprovedSubjects should be(1)

        and("have the notes table")
        dataToStudent2012.notesTable should equal(List((10, 0), (9, 0), (8, 1), (7, 1), (6, 1), (5, 1), (4, 1), (3, 1), (2, 0), (1, 0), (0, 2)))

        and("the best note in n times")
        dataToStudent2012.bestNoteNTimes(1) should be(8)

      }

      "About annual average of a Professor" ignore { // TODO annual average
    	  
      }
    }
  }
}
