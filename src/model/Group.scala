package model

import java.util.Date

class Group(members: List[Person]) {
  
  var presentedActivities: List[Activity] = List()

  def nonApprovedActivities: List[Activity] = {
    presentedActivities filter (activity => (!activity.isApproved))
  }

  def nonApprovedTotalAmount: Int = {
    this.nonApprovedActivities.foldLeft(0)(_ + _.financialAmount)
  }

  def publishedArticles: List[(Activity, Date)] = {
    val presentedActivities = this.presentedActivities filter (a => (a.containsWordsArticle))
    presentedActivities.foldRight(List[(Activity, Date)]())((a,b)=> (a, a.approvalDate) :: b)
  }
}
