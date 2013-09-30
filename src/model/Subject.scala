package model

class Subject (val aName: String, val aCareer: Career, var courses: List[Course]) extends AcademicQuery{
  
  def getAcademicRecord = courses.foldRight(List[(Boolean, Int)]())(_.getAcademicRecord ::: _)
}
