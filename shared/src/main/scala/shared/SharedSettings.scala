package shared

case class VariantProperty(
    numPlayers: Int = 2
) {
  def lastPlayer: Int = numPlayers - 1
}

case class TimeControlProperty(
    flickspeedfactor: Double = 10.0,
    flickmaxspeed: Double = 10.0
) {
}

case class Color4Property(
    color: String,
    name: String,
    rot: Int
) {
}

object SharedSettings {

  val SUPPORTED_VARIANTS = List(
    "Four Player",
    "Standard",
    "Atomic",
    "Antichess",
    "Flick"
  )

  val SUPPORTED_VARIANT_PROPERTIES = Map(
    "Flick" -> VariantProperty(),
    "Standard" -> VariantProperty(),
    "Atomic" -> VariantProperty(),
    "Antichess" -> VariantProperty(),
    "Four Player" -> VariantProperty(4)
  )

  def DEFAULT_VARIANT = smartchess.board.DEFAULT_VARIANT

  val SUPPORTED_TIME_CONTROLS = List(
    "15 + 10",
    "5 + 8",
    "3 + 2",
    "5 + 0",
    "3 + 0",
    "2 + 0",
    "1 + 0"
  )

  val SUPPORTED_TIME_CONTROL_PROPERTIES = Map[String, TimeControlProperty](
    "15 + 10" -> TimeControlProperty(5.0, 5.0),
    "5 + 8" -> TimeControlProperty(6.0, 6.0),
    "3 + 2" -> TimeControlProperty(8.0, 8.0),
    "5 + 0" -> TimeControlProperty(8.0, 8.0),
    "3 + 0" -> TimeControlProperty(12.0, 12.0),
    "2 + 0" -> TimeControlProperty(15.0, 15.0),
    "1 + 0" -> TimeControlProperty(20.0, 20.0)
  )

  def DEFAULT_TIME_CONTROL = "5 + 8"

  val COLOR4_PROPERTIES = Map[Int, Color4Property](
    0 -> Color4Property("white", "White", 0),
    1 -> Color4Property("yellow", "Yellow", 90),
    2 -> Color4Property("black", "Black", 180),
    3 -> Color4Property("red", "Red", -90)
  )

  //////////////////////////////////////////////////  

  val ANNOTS = List("!!", "!", "!?", "-", "?!", "?", "??")

  case class AnnotProperty(
    score: Int = 0,
    col: String = ""
  )

  val ANNOT_PROPERTIES = Map[String, AnnotProperty](
    "" -> AnnotProperty(-10, "#7f7f7f"),
    "!!" -> AnnotProperty(10, "#00ff00"),
    "!" -> AnnotProperty(8, "#007f00"),
    "!?" -> AnnotProperty(6, "#0000ff"),
    "-" -> AnnotProperty(5, "#000000"),
    "?!" -> AnnotProperty(4, "#00007f"),
    "?" -> AnnotProperty(2, "#7f0000"),
    "??" -> AnnotProperty(0, "#ff0000")
  )

  //////////////////////////////////////////////////

  val AARROW_COLOR = "#00ff00"

  val AARROW_OPACITY = "0.5"

  //////////////////////////////////////////////////

  val TABLE_IDLE_PERIOD = 10
  val MAX_STORED_GAMES_PER_USER = 10

  val ANALYZED_VARIANTS = List("Standard", "Atomic", "Antichess")

  val DEFAULT_ANALYZED_VARIANT = "Standard"

  //////////////////////////////////////////////////

  val SITE_URL = "https://chessapp.cleverapps.io"

  //////////////////////////////////////////////////  
}
