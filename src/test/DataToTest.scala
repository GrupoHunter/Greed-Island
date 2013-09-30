package test

import model._

class DataToTest {

  //Data to Career TPI....
  private def loadingProfessor: List[Professor] = {
	new Professor("Mengano", "Python", 36) ::new Professor("Sultano", "Scala", 30):: 
	new Professor("Fula", "Haskel", 42) :: new Professor("Ricardo", "Mate", 40 ):: List()
  }    
  private def loadingSystemCareer: Career = {
    val careerTpi = new Career("Programming", List())

    //subjects load
    val intro = (new Subject("Intro", careerTpi, List()))
    val dataBase = (new Subject("Data Base", careerTpi, List()))
    val math1 = (new Subject("Math1", careerTpi, List()))
    careerTpi.subjects = intro :: dataBase :: math1 :: List()

    //professors load
    val menganoPython = loadingProfessor(0)
    val sultanoScala = loadingProfessor(1)
    val fulaHaskel = loadingProfessor(2)
    val ricardoMate = loadingProfessor(3)

    //students load
    var studentsTPI: List[Student] = (new Student("Cosme", "Fulanito", 23, careerTpi)) :: (new Student("Killua", "Soldick", 18, careerTpi)) ::
      (new Student("Gon", "Freecs", 17, careerTpi)) :: (new Student("Betina", "Azul", 23, careerTpi)) ::
      (new Student("Carlito", "Alfalfa", 26, careerTpi)) :: (new Student("Gaston", "Char", 27, careerTpi)) :: List()

    val studentsIntro1 = (studentsTPI(0)) :: (studentsTPI(1)) :: (studentsTPI(3)) :: List()
    val studentsIntro2 = (studentsTPI(4)) :: (studentsTPI(5)) :: List()
    val studentsDataBase = (studentsTPI(0)) :: (studentsTPI(2)) :: (studentsTPI(4)) :: List()
    val studentsMath = (studentsTPI(1)) :: (studentsTPI(2)) :: (studentsTPI(3)) :: (studentsTPI(5)) :: List()

    //courses load with professors and students
    intro.courses = ((new Course(intro, menganoPython :: List(), studentsIntro1)) :: (new Course(intro, sultanoScala :: List(), studentsIntro2)) :: List())
    dataBase.courses = (new Course(dataBase, menganoPython :: List(), studentsDataBase)) :: (new Course(dataBase, menganoPython :: sultanoScala :: List(), studentsTPI(3) :: List())) :: List()
    math1.courses = (new Course(math1, ricardoMate::fulaHaskel :: List(), studentsMath)) :: List()
    
    //notes load Students
    
    studentsTPI(0).noteTablesAllCourses = (intro.courses(0), true, 8) :: (math1.courses(0), true, 7) :: List()
    studentsTPI(1).noteTablesAllCourses = (intro.courses(0), true, 9) :: (dataBase.courses(0), true, 3) :: List()
    studentsTPI(2).noteTablesAllCourses = (math1.courses(0), true, 4) :: (dataBase.courses(0), false, 0) :: List()
    studentsTPI(3).noteTablesAllCourses = (intro.courses(0), false, 0) :: (dataBase.courses(1), true, 3) :: List()
    studentsTPI(4).noteTablesAllCourses = (intro.courses(1), true, 7) :: (math1.courses(0), true, 9) :: (dataBase.courses(0), true, 9) :: List()
    studentsTPI(5).noteTablesAllCourses = (intro.courses(1), true, 9) :: (dataBase.courses(1), true, 2) :: List()

    careerTpi
  }

  //Access
  var allCareer: List[Career] = this.loadingSystemCareer:: List()
  var allSubject: List[Subject] = allCareer(0).subjects
  var allCourse = allSubject.foldRight(List[Course]())(_.courses ::: _)
  var allProfessors = this.loadingProfessor
  var studentsTPI = allCourse.foldRight(List[Student]())(_.students ::: _ ) 
  
}
