package test
import org.scalatest.mock.MockitoSugar
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import model.AcademicRecord
import model.Career
import model.Course
import model.Student

class FirstPartTest extends FlatSpec with ShouldMatchers with MockitoSugar {

  def fixture = new {
    val academicRecord = new AcademicRecord
    val mockCareer = mock[Career]
    val mockCourse = mock[Course]
    val student = new Student("Fulanito", "Cosme", 24, mockCareer)
    student noteTablesAllCourses = (mockCourse, true, 4) ::
      (mockCourse, true, 6) ::
      (mockCourse, true, 3) ::
      (mockCourse, true, 8) ::
      (mockCourse, false, 0) ::
      (mockCourse, true, 1) ::
      (mockCourse, true, 2) ::
      (mockCourse, true, 3) :: List()

  }

  "A Student" should "give the grade point average approved (6) and dissaproved (1.8)" in {
    val f = fixture

    f.academicRecord averageWithFails (f.student) should be(1.8)
    f.academicRecord averageWithoutFails (f.student) should be(6)
  }

  it should "give the amount of material that passed (3) and failed (1)" in {
    val f = fixture

    f.academicRecord amountSuccessful (f.student) should be(3)
    f.academicRecord amountFailed (f.student) should be(1)
  }

  it should "give the average approved subjects " in {
    val f = fixture

    f.academicRecord averageApprovedSubjects (f.student) should be(2)
  }

  it should "have the notes table with (10->2, 9->1, 8->2, 6->1, 5->3, 4->1, 3->1, 2->1, 1->1)" in {
    val f = fixture
    val student = new Student("Fulanito", "Cosme", 24, f.mockCareer)
    student noteTablesAllCourses = (f.mockCourse, true, 10) :: (f.mockCourse, true, 10) :: (f.mockCourse, true, 9) :: (f.mockCourse, true, 8) ::
      (f.mockCourse, true, 8) :: (f.mockCourse, true, 6) :: (f.mockCourse, true, 5) :: (f.mockCourse, true, 5) ::
      (f.mockCourse, true, 5) :: (f.mockCourse, true, 4) :: (f.mockCourse, true, 3) :: (f.mockCourse, true, 2) ::
      (f.mockCourse, true, 1) :: List()

    val notesTable = f.academicRecord notesTable (student)
    notesTable should have size (11)
    notesTable should equal((10,2):: (9,1):: (8,2):: (7,0):: (6,1):: (5,3):: (4,1):: (3,1):: (2,1):: (1,1):: (0,0):: List())

  }

  it should "give 3 times the best note is 9 and 4 times is 3" in {
    val f = fixture
    val student = new Student("Fulanito", "Cosme", 24, f.mockCareer)
    student noteTablesAllCourses = (f.mockCourse, true, 9) :: (f.mockCourse, true, 9) :: (f.mockCourse, true, 9) :: (f.mockCourse, true, 8) ::
      (f.mockCourse, true, 8) :: (f.mockCourse, true, 6) :: (f.mockCourse, true, 5) :: (f.mockCourse, true, 5) ::
      (f.mockCourse, true, 5) :: (f.mockCourse, true, 4) :: (f.mockCourse, true, 3) :: (f.mockCourse, true, 3) ::
      (f.mockCourse, true, 3) ::(f.mockCourse, true, 3) ::(f.mockCourse, true, 3) ::(f.mockCourse, true, 2) ::
      (f.mockCourse, true, 1) :: List()

    f.academicRecord bestNoteNTimes (3, student) should be(9)
    f.academicRecord bestNoteNTimes (2, student) should be(9)
    f.academicRecord bestNoteNTimes (4, student) should be(3)
  }
}
