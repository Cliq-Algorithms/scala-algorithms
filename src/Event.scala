
class Event (nm: String, lati: Double, long: Double, tag: String, daysAway :Int) {
  var lat = lati
  var lon = long
  var tags = tag
  var name = nm
  var date = daysAway
  var attendees = new Array[User](5)
}