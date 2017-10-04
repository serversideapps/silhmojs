package scalajsclient

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.scalajs.js.Dynamic.global

import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._

import smartchess._
import ScalaJSUtils._
import shared._
import shared.SharedSettings._

object WebBoardGlobals {

  var FLICK_MARGIN = 10.0

  var NUM_PLAYERS = 2 // should be set

  def BOARD_NUM_FILES = if (NUM_PLAYERS == 2) 8 else 14
  def BOARD_NUM_RANKS = if (NUM_PLAYERS == 2) 8 else 14

  def SQUARE_SIZE = (400.0 - 2.0 * FLICK_MARGIN) / BOARD_NUM_FILES
  def HALF_SQUARE_SIZE = SQUARE_SIZE / 2.0

  def BOARD_WIDTH: Double = BOARD_NUM_FILES * SQUARE_SIZE
  def BOARD_HEIGHT: Double = BOARD_NUM_RANKS * SQUARE_SIZE
  def TOTAL_BOARD_WIDTH: Double = BOARD_WIDTH + 2 * MARGIN
  def TOTAL_BOARD_HEIGHT: Double = BOARD_HEIGHT + 2 * MARGIN
  def TOTAL_INFO_WIDTH = TOTAL_BOARD_WIDTH + 2 * FLICK_MARGIN
  def TOTAL_INFO_HEIGHT = TOTAL_BOARD_HEIGHT + 2 * FLICK_MARGIN
  def PLAYER_INFO_HEIGHT: Double = 22.0

  def PADDING: Double = 3.0

  def MARGIN: Double = 10.0

  def SQUARE_OPACITY: Double = 0.15
  def PIECE_OPACITY(c: piece.TColor): Double = if (c == piece.WHITE) 1.0 else 0.9

  def DARK_SQUARE_COLOR = "#8f8f8f"
  def LIGHT_SQUARE_COLOR = "#ffffff"

  def PIECE_SIZE = SQUARE_SIZE - 2 * PADDING

  def PIECE_STYLE: String = {
    val pt = root.attr("piecetype")
    if (pt == "") return "alpha"
    pt
  }

  def PLAYER_INFO_COLOR = "#bfbfbf"

  def CLOCK_STEP = 1000.0

  def CLOCK_WIDTH = if (NUM_PLAYERS == 2) 1.5 * SQUARE_SIZE else 2.4 * SQUARE_SIZE
  def PLAYER_INFO_WIDTH = if (NUM_PLAYERS == 2) TOTAL_INFO_WIDTH - CLOCK_WIDTH else TOTAL_INFO_WIDTH / 2.0

  def PLAYER_PLAY_WIDTH = PLAYER_INFO_WIDTH - 1.2 * CLOCK_WIDTH
  def PLAYER_HANDLE_WIDTH = PLAYER_PLAY_WIDTH

  def PLAYER_AI_WIDTH = PLAYER_HANDLE_WIDTH / 2.0
  def PLAYER_MARGIN = PLAYER_INFO_HEIGHT / 7.0

  def PLAYER_FONT_SIZE = PLAYER_INFO_HEIGHT - 2 * PLAYER_MARGIN
  def PLAYER_HEIGHT = PLAYER_INFO_HEIGHT - 2 * PLAYER_MARGIN

  def GAME_INFO_BCKG_COLOR = "#afaf7f"
  def GAME_INFO_FONT_COLOR = "#000000"

  def GAME_INFO_FONT_SIZE = PLAYER_FONT_SIZE * PLAYER_HEIGHT / PLAYER_INFO_HEIGHT

  def PADDING_FACTOR = 1.5

  def BOARD_CONTROLS_BUTTON_WIDTH = PLAYER_HEIGHT * 2.5
  def BOARD_CONTROLS_BUTTON_HEIGHT = PLAYER_HEIGHT * 0.8
  def BOARD_CONTROLS_FONT_SIZE = PLAYER_FONT_SIZE * 0.8
  def BOARD_CONTROLS_BUTTON_BCKG_COLOR = "#dfffff"
  def BOARD_CONTROLS_BUTTON_FONT_COLOR = "#000000"

  def TERMINATED_MARGIN = TOTAL_BOARD_WIDTH / 8.0
  def TERMINATED_FONT_SIZE = 30.0

  def ALL_ABOVE_BOARD_HEIGHT = PLAYER_INFO_HEIGHT + PLAYER_HEIGHT

  def PLAYER_COLORS = Map[Tuple2[Int, Int], Tuple2[String, String]](
    (2, 0) -> ("white", "black"),
    (2, 1) -> ("black", "white"),
    (4, 0) -> ("white", "black"),
    (4, 1) -> ("yellow", "black"),
    (4, 2) -> ("black", "white"),
    (4, 3) -> ("red", "white")
  )

  var PLAYER_INFO_PLACINGS = Map[Tuple2[Int, Int], Tuple2[Double, Double]]()
  var PLAYER_CLOCK_PLACINGS = Map[Tuple2[Int, Int], Tuple2[Double, Double]]()

  def CLOCK_TOP_SHIFT = MARGIN + FLICK_MARGIN
  def CLOCK_LEFT_SHIFT = MARGIN + FLICK_MARGIN

  def ACONTROLS_COLOR = "#ffffaf"

  def CALC_PLACINGS {

    PLAYER_INFO_PLACINGS = Map[Tuple2[Int, Int], Tuple2[Double, Double]](
      (2, 0) -> (TOTAL_INFO_HEIGHT, 0.0),
      (2, 1) -> (-PLAYER_INFO_HEIGHT, 0.0),
      (4, 0) -> (TOTAL_INFO_HEIGHT, 0.0),
      (4, 1) -> (-PLAYER_INFO_HEIGHT, PLAYER_INFO_WIDTH),
      (4, 2) -> (-PLAYER_INFO_HEIGHT, 0.0),
      (4, 3) -> (TOTAL_INFO_HEIGHT, PLAYER_INFO_WIDTH)
    )

    PLAYER_CLOCK_PLACINGS = Map[Tuple2[Int, Int], Tuple2[Double, Double]](
      (2, 0) -> (TOTAL_INFO_HEIGHT, TOTAL_INFO_WIDTH - CLOCK_WIDTH),
      (2, 1) -> (-PLAYER_INFO_HEIGHT, TOTAL_INFO_WIDTH - CLOCK_WIDTH),
      (4, 0) -> (TOTAL_INFO_HEIGHT - PLAYER_INFO_HEIGHT - CLOCK_TOP_SHIFT, CLOCK_LEFT_SHIFT),
      (4, 1) -> (CLOCK_TOP_SHIFT, TOTAL_INFO_WIDTH - CLOCK_LEFT_SHIFT - CLOCK_WIDTH),
      (4, 2) -> (CLOCK_TOP_SHIFT, CLOCK_LEFT_SHIFT),
      (4, 3) -> (TOTAL_INFO_HEIGHT - PLAYER_INFO_HEIGHT - CLOCK_TOP_SHIFT, TOTAL_INFO_WIDTH - CLOCK_LEFT_SHIFT - CLOCK_WIDTH)
    )

  }

  case class Acontrol(
    function: String,
    text: String,
    bcol: String
  )

  val ACONTROL_BCOL_TERMINAL = "#00007f"
  val ACONTROL_BCOL_INCREMENTAL = "#007f00"
  val ACONTROL_BCOL_DELETE = "#7f0000"
  val ACONTROL_BCOL_STORE = "#7f7f00"
  val ACONTROL_BCOL_SEARCH = "#007f7f"

  def ACONTROLS = List(
    Acontrol("tobegin", "<<", ACONTROL_BCOL_TERMINAL),
    Acontrol("back", "<", ACONTROL_BCOL_INCREMENTAL),
    Acontrol("forward", ">", ACONTROL_BCOL_INCREMENTAL),
    Acontrol("toend", ">>", ACONTROL_BCOL_TERMINAL),
    Acontrol("del", "<", ACONTROL_BCOL_DELETE),
    Acontrol("start", ">", ACONTROL_BCOL_INCREMENTAL),
    Acontrol("stop", "x", ACONTROL_BCOL_DELETE),
    Acontrol("make", "->", ACONTROL_BCOL_TERMINAL),
    Acontrol("store", "o", ACONTROL_BCOL_STORE),
    Acontrol("search", "s", ACONTROL_BCOL_SEARCH)
  )

  def ACONTROL_BORDER = PLAYER_MARGIN / 2.0

  def ACONTROL_WIDTH = 30.0
  def ACONTROL_HEIGHT = 20.0

  def ARROW_ZINDEX = 45

}