class Event (nm: String, lati: Double, long: Double, tag: String) {
  var lat = lati
  var lon = long
  var tags = tag
  var name = nm
  var attendees = new Array[User](5)
}