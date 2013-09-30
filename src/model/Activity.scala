package model

import java.util.{Date, Calendar}
import java.sql.Time

abstract class Activity () {
  
	var approved: Boolean = false
	implicit val filingDate: Date = Calendar.getInstance().getTime()
	var approvalDate = new Date
	
	def approve(){
	  approved = true
	  approvalDate = Calendar.getInstance().getTime()
	}
	
	def isApproved: Boolean = {
	  approved
	}
	
	def aName: String
	def aResponsible: Person
	def members: List[Person with Schedule]
	def financialAmount: Int
	
	def containsWordsArticle : Boolean = aName contains("Article")
}

case class Seminary (
						aName: String, 
						aResponsible: Person,
						members: List[Person with Schedule],
						financialAmount: Int, 
						sessions: List[Session]
					) 
	extends Activity () {
  
  def neededSpace : Int = members length
  
  /**
   * Adds its name as description on each session.
   */
  def putDescription (sessions: List[Session]) = {
	for(session <- sessions){
	  session.putDescription(aName)
	}
  }
}

case class Project (
						aName: String,
						aResponsible: Person,
						members: List[Person with Schedule],
						financialAmount: Int,
						description : String,
						var results : List[Result],
						var log : List[Experiment]
					) 
	extends Activity () {
	  override def containsWordsArticle = results exists(r => r.description.contains("Article"))
}

case class Talk (
					aName: String,
					aResponsible: Person,
					members: List[Person with Schedule],
					estimatedPublic: Int,
					financialAmount: Int,
					aDate: Date,
					fromHour: Time,
					toHour: Time
				) 
	extends Activity () with Event {
	
  def neededSpace : Int = members.length + estimatedPublic
  
  implicit def scheduleMembers = {
    for (m <- members) {
      this :: m.belongProjects
    }
  }
}

case class Result (aDate: Date, description: String)

case class Session (aDate: Date, fromHour: Time, toHour: Time) extends Event {
  /**
   * This method, puts the seminary's name. (The one where this session was added)
   */
  implicit def putDescription (aName: String) = {
    description = aName
  }
}

case class Experiment (aDate: Date, fromHour: Time, toHour: Time/*, description: String*/) extends Event

trait Event {
  def aDate: Date
  def fromHour: Time
  def toHour: Time
  var description: String = null
}
