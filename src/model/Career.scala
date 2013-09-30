package model

class Career (val aName: String,var subjects: List[Subject]) extends AcademicQuery{
	
	def getAcademicRecord = {
	  val allCourses = subjects.foldRight(List[Course]())((a, b)=> a.courses ++ b)
	  val allStudent = allCourses.foldRight(List[Student]())((a,b)=> a.students ++ b)
	  
	  allStudent.foldRight(List[(Course, Boolean, Int)]())((a,b)=> a.noteTablesAllCourses ++ b)
	  			.foldRight(List[(Boolean, Int)]())((a,b)=> (a._2, a._3) ::b)
	}
}
