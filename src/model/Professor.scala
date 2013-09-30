package model

case class Professor(aName: String, aLastname: String, anAge: Int)
  extends Person(aName, aLastname, anAge) with Schedule with AcademicQuery {

  private def getAllCourses: List[(Course, Boolean, Int)] = {
    AcademicData.Students.foldRight(List[(Course, Boolean, Int)]())((a, b) => a.noteTablesAllCourses ::: b)
  }

  def getAcademicRecord = {
    this.getAllCourses.filter(p => p._1.professors.contains(this)) //
      .foldRight(List[(Boolean, Int)]())((a, b) => (a._2, a._3) :: b)
  }

  def annualAverage(aYear: Int): Professor = {
    new Professor(aName, aLastname, anAge) with AcademicQuery {
      val notesWithXYear = this.getAllCourses.filter(n => n._1.aDate.getYear().equals(aYear))
      override def getAcademicRecord = notesWithXYear.foldRight(List[(Boolean, Int)]())((a, b) => (a._2, a._3) :: b)
    }
  }
}
