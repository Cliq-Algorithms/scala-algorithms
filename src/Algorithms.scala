object Algorithms{
val DEGREE_MILE_SCALAR = 68.71;
  /*Index of Largest 
  * Find the index of the largest value in the array
  */
  def indexOfLargest(array: Seq[Int]): Int = {
    val result = array.foldLeft(-1,Int.MinValue,0) {
        case ((maxIndex, maxValue, currentIndex), currentValue) =>
            if(currentValue > maxValue) (currentIndex,currentValue,currentIndex+1)
            else (maxIndex,maxValue,currentIndex+1)
        }
    result._1
  }

  /*Remove
  * Remove array by value 
  */
  def remove(num: Int, array: Array[Int]) = array diff Array(num)
    
  /*Event Search
  * Distance formula between events and location
  */
  def event_search(events: Array[Event], user: User) = {
    var size = events.length
    println("\nEvents nearby")
    for(i <- 0 until size){
      var within_radius = (user.radius/DEGREE_MILE_SCALAR) > Math.pow( (Math.pow( (user.lat - events(i).lat) , 2 ) + Math.pow( (user.lon - events(i).lon) , 2 )), 0.5)
      if (within_radius){
          println(events(i).name)
      }
    }
  }
  /*Tag Search
  * Extends Event Search
  * Runs Tag matching on event search results
  */
    def tag_search(events: Array[Event], user: User, tag: String):Array[Event] = {
      var size = events.length
      println("\n"+ tag +" events nearby")
      for(i <- 0 until size){
        var within_radius = (user.radius/DEGREE_MILE_SCALAR) > Math.pow( (Math.pow( (user.lat - events(i).lat) , 2 ) + Math.pow( (user.lon - events(i).lon) , 2 )), 0.5)
        if (within_radius){
          if(events(i).tags.equals(tag)){
            println(events(i).name)
          }
        }
      }
      return events
    }
    /* Ranking Algorithm 
     * Average of all reviews of a user
     */
    def ranking(user: User,rankings: Array[Double]) = {
      var sum = 0.0
      for (i <- 0 until (rankings.length)){
        sum = sum + rankings(i)
      }
      user.ranking =  sum / rankings.length
      println(user.name + "'s Ranking = "  + user.ranking)
    }
    
    /* Recomendations 
    * Find Favorite Tag and show other events with that tag
    */
    def recomendations(events: Array[Event], user: User)={
      val mainTags = Array("Sports", "Video Games", "Movies", "Food", "Music")
      val mainTagsCount = Array(0,0,0,0,0)
      val attendedEvents = user.events

      for(i <- 0 until attendedEvents.length){
        if(attendedEvents(i).equals("Sports")){
          mainTagsCount(0) = mainTagsCount(0) + 1
        }
        else if(attendedEvents(i).tags == "Video Games"){
          mainTagsCount(1) = mainTagsCount(1) + 1
        }
        else if(attendedEvents(i).tags == "Movies"){
          mainTagsCount(2) = mainTagsCount(2) + 1
        }
        else if(attendedEvents(i).tags == "Food"){
          mainTagsCount(3) = mainTagsCount(3) + 1
        }
        else{
          mainTagsCount(4) = mainTagsCount(4) + 1
        }
      }
      var max = indexOfLargest(mainTagsCount)
      var fav = mainTags(max)
      var event = tag_search(events, user, fav)
    }
  
  //test
  def main(args: Array[String]) {
      // for loop execution with a range
      var naren = new User("Naren", 5, 12.234, 12.342, 0.123)
      var rankings = Array(1.0,0.0,1.0,0.0,0.0,0.0,0.0)
      var a = new Event("The Social Network", 12.234, 12.342, "Movies")
      var b = new Event("The Internship", 12.343, 12.213, "Movies")
      var c = new Event("Soccer", 12.234, 12.342, "Sports")
      var d = new Event("The Matrix", 12.234, 12.342, "Movies")
      val events = Array(a,b,c,d)
      var userEvents = Array(a,b)
      
      naren.events = userEvents
      ranking(naren, rankings)
      println("\nRecommended Events:")
      recomendations(events, naren)
      tag_search(events, naren, "Movies")
      event_search(events, naren)
      
   }
}