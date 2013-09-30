package test

import org.scalatest.mock.MockitoSugar
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import java.util.Date
import java.sql.Time
import model._


class ThirdPartTest extends FlatSpec with ShouldMatchers with MockitoSugar  {

  def fixture = new {
    val mockMembers = mock[List[Person with Schedule]]
    val mockSession = mock[List[Session]]
    val mockExperiments = mock[List[Experiment]]
    val group = new Group(mockMembers)
    val responsible = new Professor("Cosme", "Fulanito", 32)
    val mockDate = mock[Date]
    val mockTime = mock[Time]
  }
	
	"Group" should "have three unapproved projects" in {
	  val f = fixture
	  val mockMembersWithSchedule = mock[List[Person with Schedule]]
	  val proyectApproved = new Project("Non so", f.responsible, f.mockMembers, 23, "non capisco", List(), List())
	  proyectApproved.approve
	  f.group presentedActivities = (new Seminary("Program", f.responsible,f.mockMembers, 24, f.mockSession))::
	  								(new Project("Butterfly", f.responsible, f.mockMembers, 23, "I haven't idea", List(), f.mockExperiments))::
	  								(new Talk("Lambda",f.responsible,mockMembersWithSchedule , 12, 26, f.mockDate, f.mockTime, f.mockTime))::
	  								(proyectApproved)::List()
	  
	  val nonApprovedActivities = f.group.nonApprovedActivities
	  
	  nonApprovedActivities should have length (3)
	  nonApprovedActivities should ( equal ((new Seminary("Program", f.responsible,f.mockMembers, 24, f.mockSession))::
	  										(new Project("Butterfly", f.responsible, f.mockMembers, 23, "I haven't idea", List(), f.mockExperiments))::
	  										(new Talk("Lambda",f.responsible,mockMembersWithSchedule , 12, 26, f.mockDate, f.mockTime, f.mockTime))
	  										::List()))
	}
	
	it should "have total amount of 100" in {
	  val f = fixture
	  val mockMembersWithSchedule = mock[List[Person with Schedule]]
	  f.group presentedActivities = (new Seminary("Program", f.responsible,f.mockMembers, 24, f.mockSession))::
	  								(new Project("Butterfly", f.responsible, f.mockMembers, 12, "I haven't idea", List(),f.mockExperiments))::
	  								(new Talk("Lambda",f.responsible,mockMembersWithSchedule , 12, 64, f.mockDate, f.mockTime, f.mockTime))::List()
	  								
	  f.group.nonApprovedTotalAmount should be (100)
	}
	
	it should "give tree activity" in {
	  val f = fixture 
	  val mockLog = mock[List[Experiment]]
	  val mockMembersWithSchedule = mock[List[Person with Schedule]]
	  f.group presentedActivities = (new Seminary("Article Program", f.responsible,f.mockMembers, 24, f.mockSession))::
	  								(new Project("Butterfly", f.responsible, f.mockMembers, 12, "I haven't idea", List(), f.mockExperiments))::
	  								(new Talk("Article Lambda",f.responsible,mockMembersWithSchedule , 12, 64, f.mockDate, f.mockTime, f.mockTime))::List()
	  
	  f.group.presentedActivities foreach ( p => p.approve)
	  
	  val publishedArticles =  f.group.publishedArticles
	  
	  publishedArticles should have length (2)
	  publishedArticles(0)._1 should have ('aName("Article Program"))
	  publishedArticles(1)._1 should have ('aName("Article Lambda"))
	}
}
