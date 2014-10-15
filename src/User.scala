class User (nm: String, rad: Int, lati: Double, long: Double, rank: Double) {
  val name: String = nm
  var radius: Int = rad
  var lat: Double = lati
  var lon: Double = long
  var ranking: Double =  rank
  var events = new Array[Event](5)
}