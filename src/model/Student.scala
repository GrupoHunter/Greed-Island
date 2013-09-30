package model

case class Student (aName: String, aLastname: String, anAge: Int, currentCareer: Career)
  extends Person(aName, aLastname, anAge) with Schedule with AcademicQuery {

  type Assist = Boolean
  type Note = Int
  var noteTablesAllCourses: List[(Course, Assist, Note)] = Nil //Should not have repeated
  
  def getAcademicRecord = noteTablesAllCourses.foldRight(List[(Assist, Note)]())((a,b) => (a._2, a._3):: b)
  
  def annualAverage(aYear: Int): Student ={
    new Student(aName, aLastname, anAge, currentCareer) with AcademicQuery{
      val notesWithXYear = this.noteTablesAllCourses.filter(n => n._1.aDate.getYear().equals(aYear))
      override def getAcademicRecord = notesWithXYear.foldRight(List[(Assist, Note)]())((a,b) => (a._2, a._3):: b)
    }
  }
}
