package test

import org.scalatest.mock.MockitoSugar
import org.scalatest.FunSpec
import org.scalatest.matchers.ShouldMatchers
import java.util.Date
import java.sql.Time
import model._
import org.scalatest.GivenWhenThen

class FourthPartTest extends FunSpec with ShouldMatchers with GivenWhenThen with MockitoSugar {

  def fixture = new {
    val projector = new Resource("Projector")
    val microphone = new Resource("Microphone")
    val airconditioning = new Resource("airconditioning")
    val plugsWithStabilizer = new Resource("plugsWithStabilizer")
    val computers = new Resource("computer")
    val mockDate = mock[Date]
    val mockTime = mock[Time]
  }

  def members = new {
    val careerTpi = AcademicData.getCareer("Programming")
    val members = (new Student("Cosme", "Fulanito", 23, careerTpi)) :: (new Student("Killua", "Zoldyck", 18, careerTpi)) ::
      (new Student("Gon", "Freecs", 17, careerTpi)) :: (new Student("Betina", "Azul", 23, careerTpi)) ::
      (new Student("Carlito", "Alfalfa", 26, careerTpi)) :: (new Student("Gaston", "Char", 27, careerTpi))::
      mock[Student]::mock[Student]::mock[Student]::mock[Student]:: List()
    val session = null //TODO      
  }

  describe("Classroom Management") {
    it("should assign classrooms at 3 activities present") {
      val f = fixture
      val m = members
      
    given("the following activities")
      val course = new Course(AcademicData.getSubject("Intro"), AcademicData.professors, AcademicData.Students)
      course.resources = f.computers :: f.plugsWithStabilizer :: f.projector :: List()

      val talk = new Talk("Lambda", new Professor("Cosme", "Fulanito", 32), m.members, 12, 26, f.mockDate, f.mockTime, f.mockTime) with WithClassRoom
      talk.resources = f.projector :: f.microphone :: List()

      val seminary = new Seminary("Program", AcademicData.getProfessor("Megano", "Python"), m.members, 24, m.session) with WithClassRoom
      seminary.resources = f.airconditioning :: f.microphone :: f.projector :: f.computers :: List()

      val activities: List[_ with WithClassRoom] = course :: talk :: seminary :: List()

    when("the academy has these classrooms")
      val classroomManagement = new ClassroomManagement(activities)
      val classroom: ClassRoom = new ClassRoom(f.projector :: f.microphone :: List(), 30)
      val classroom1: ClassRoom = new ClassRoom(f.computers :: f.plugsWithStabilizer :: f.projector :: List(), 20)
      val classroom2: ClassRoom = new ClassRoom(f.airconditioning :: f.microphone :: f.projector :: f.computers :: List(), 10)
      val classroom3: ClassRoom = new ClassRoom(f.airconditioning :: f.microphone :: f.projector :: f.computers :: List(), 30)
      val classroom4: ClassRoom = new ClassRoom(f.projector :: f.microphone :: List(), 9)
      val classroom5: ClassRoom = new ClassRoom(f.computers :: f.plugsWithStabilizer :: f.projector :: List(), 10)

      val classrooms: List[ClassRoom] = 
        		classroom  :: classroom1 :: classroom2 :: 
        		classroom3 :: classroom4 :: classroom5 :: List()

    then("assign each activity to a classroom")
      classroomManagement.assignClassroom(classrooms)
      val busyClassrooms = classroomManagement.busyClassroom

    and("you get a tuple of classroom activity")
      busyClassrooms should have size (3)
      busyClassrooms should contain((classroom, talk))
      busyClassrooms should contain((classroom1, course))
      busyClassrooms should contain((classroom3, seminary))

    }
  }
}
