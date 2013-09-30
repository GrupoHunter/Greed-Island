package model

abstract class Person (aName: String, aLastname: String, anAge: Int)

trait Schedule {
  type Schedule = List[Event]
  var schedule : Schedule = List()
  var belongProjects : List[Activity] = List()
  
  def getSchedule: Schedule = {
    getEvents(belongProjects)
  }
  
  //Functional approach
  def getEvents (xs: List[Activity]): List[Event] = xs match {
    case Nil => Nil
    case (head: Talk) :: tail => head :: getEvents(tail)
    case (head: Project) :: tail => head.log.++(getEvents(tail))
    case (head: Seminary) :: tail => head.sessions.++(getEvents(tail))
    case _ => Nil
  }
}
