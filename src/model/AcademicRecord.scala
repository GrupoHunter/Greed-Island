package model

import test.DataToTest

class AcademicRecord {

  def averageWithFails(query: { def averageWithFails: Double }): Double = query.averageWithFails

  def averageWithoutFails(query: { def averageWithoutFails: Double }): Double = query.averageWithoutFails

  def amountSuccessful(query: { def amountSuccessful: Int }): Int = query.amountSuccessful

  def amountFailed(query: { def amountFailed: Int }): Int = query.amountFailed

  def averageApprovedSubjects(query: { def averageApprovedSubjects: Int }): Int = query.averageApprovedSubjects

  def notesTable(query: { def notesTable: List[(Int, Int)] }): List[(Int, Int)] = query notesTable

  def bestNoteNTimes(n: Int, query: { def bestNoteNTimes(n: Int): Int }): Int = query.bestNoteNTimes(n)
}

object AcademicData {
  
  private val data = new DataToTest

  def getCareer(aNameCareer: String): Career =
    data.allCareer.find(c => c.aName.contains(aNameCareer)) match {
      case Some(c) => c
      case None => null
    }

  def getCourse(aNameSubject: String): Course = {
    data.allCourse.find(c => c.subject.aName.contains(aNameSubject)) match {
      case Some(c) => c
      case None => null
    }
  }

  def getSubject(aNameSubject: String): Subject = {
    data.allSubject.find(s => s.aName.contains(aNameSubject)) match {
      case Some(s) => s
      case None => null
    }
  }

  def getProfessor(aName: String, aLastName: String): Professor = {
    data.allProfessors.find(p => p.aName.contains(aName) & p.aLastname.contains(aLastName)) match {
      case Some(p) => p
      case None => null
    }
  }

  var professors: List[Professor] = data.allProfessors
  var Students: List[Student] = data.studentsTPI
}

trait AcademicQuery {

  def getAcademicRecord: List[(Boolean, Int)]

  def averageWithFails: Double = {
    val noteWithFails: List[(Boolean, Int)] = getAcademicRecord filter (n => n._2 < 4)
    if (noteWithFails.nonEmpty)
      noteWithFails.foldLeft(0.0)(_ + _._2) / noteWithFails.length
    else 0.0
  }

  def averageWithoutFails: Double = {
    val noteWithoutFails: List[(Boolean, Int)] = getAcademicRecord filter (n => n._2 >= 4)
    if (noteWithoutFails.nonEmpty)
      noteWithoutFails.foldLeft(0.0)(_ + _._2) / noteWithoutFails.length
    else 0.0
  }

  def amountSuccessful: Int = getAcademicRecord filter (n => n._2 >= 4) length
  def amountFailed: Int = getAcademicRecord filter (n => !n._1) length

  def averageApprovedSubjects: Int = getAcademicRecord.length / this.amountSuccessful

  def notesTable: List[(Int, Int)] = {
    def table(n: Int): List[(Int, Int)] = {
      if (n < 0) List()
      else (n, getAcademicRecord filter (note => note._2 == n) length) :: table(n - 1)
    }
    table(10)
  }

  def bestNoteNTimes(n: Int): Int = {
    notesTable find (e => e._2 >= n) match {
      case Some(value) => value._1
      case None => 0
    }
  }
}
