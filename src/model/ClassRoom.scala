package model

import scala.util.control.Breaks._

class ClassroomManagement (val events : List[_ with WithClassRoom]) {
  
  var busyClassroom: List[(ClassRoom, _ with WithClassRoom)] = Nil
    
  /**
   * Assign for each given event a classroom where it can fit (if exists)
   */
  def assignClassroom(classrooms: List[ClassRoom]) = {
	for(event <- events){
	  assignClassroomAux(classrooms, event)
	}
  }
  
  /**
   * This is an aux function - 
   * 	Returns, if exists, a classroom where it fits given event 
   */
  def assignClassroomAux(classrooms: List[ClassRoom], event: Any with WithClassRoom) = {
	var result = getFittingRoom(classrooms, event)
	if(result != None){						//Verify if returned result is an Option with Something
		var eventRoom = result.get				//Get element inside the Option object
		eventRoom :: busyClassroom			//Register linked classroom with event
		classrooms.diff(List(eventRoom._1)) //Remove busy classroom from available ones
		events.diff(List(eventRoom._2))		//Remove already fit event
    }
  }
  
  /**
   *  Searches a classroom (if exists) where can fit given event
   */
  def getFittingRoom (classrooms: List[ClassRoom], event: Any with WithClassRoom): Option[(ClassRoom, _ with WithClassRoom)] = {
    var result: Option[(ClassRoom, _ with WithClassRoom)] = None
    breakable { 
      for(room <- classrooms){
    	  if(event.itFit(room)){				//Verify if room fits event requirements (needed resources)
    	    result = Some((room, event)) 		//Return linked classroom with an event that it fits
    	    //TODO Needs breakable, or get rooms with minimum difference of resources needed.
    	    break
    	  } else {
    	    result = None
    	  }
      }
    }
    result
  }
}

class ClassRoom(var resources: List[Resource], val space: Int)

class Resource(val aName: String)

trait Size {
  def size: Int
}

trait Resources {
  var resources: List[Resource] = Nil
}

trait WithClassRoom extends Resources {
  def neededSpace: Int

  def itFit(room: ClassRoom): Boolean = {
    room.space >= neededSpace && haveEnoughResources(room)
  }

  def haveEnoughResources(room: ClassRoom): Boolean = {
    resources forall (r => room.resources.contains(r))
  }
}
