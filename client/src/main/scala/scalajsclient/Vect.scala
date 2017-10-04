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

import scala.collection.mutable.ArrayBuffer

object Vect {
  val INFINITE_COORD: Double = 1.0E6
}

case class Vect(
    var x: Double = 0.0,
    var y: Double = 0.0
) {
  // clone
  def cclone = Vect(x, y)
  // invert in place
  def asintpair: String = x.toInt + "," + y.toInt
  def invert {
    x = -x
    y = -y
  }
  // clone inverse
  def inverse = Vect(-x, -y)
  // add in place
  def +=(v: Vect) = {
    x += v.x
    y += v.y
  }
  // clone added
  def +(v: Vect): Vect = Vect(x + v.x, y + v.y)
  // subtract in place
  def -=(v: Vect) = {
    x -= v.x
    y -= v.y
  }
  // clone subtracted
  def -(v: Vect): Vect = Vect(x - v.x, y - v.y)

  // scale in place
  def *=(factor: Double) {
    x *= factor
    y *= factor
  }

  // clone scaled
  def *(factor: Double): Vect = {
    Vect(x * factor, y * factor)
  }

  def d: Double = scala.math.sqrt(x * x + y * y) // length
  var cos: Double = 0.0 // calculated cosine
  var sin: Double = 0.0 // calculated sine

  // calculate trigonometric functions
  def calctrig(r: Double, multrby: Double = scala.math.Pi) {
    cos = scala.math.cos(r * multrby)
    sin = scala.math.sin(r * multrby)
  }

  // rotate in place
  def rotate(r: Double, multrby: Double = scala.math.Pi) {
    calctrig(r, multrby)
    x = cos * x - sin * y
    y = sin * x + cos * y
  }
  // clone rotated
  def rotated(r: Double, multrby: Double = scala.math.Pi): Vect = {
    calctrig(r, multrby)
    Vect(cos * x - sin * y, sin * x + cos * y)
  }
  // normalize to length
  def normalize(tod: Double) {
    val ratio = tod / d
    x = x * ratio
    y = y * ratio
  }
  // clone normaluzed
  def normalized(tod: Double): Vect = {
    val ratio = tod / d
    Vect(x * ratio, y * ratio)
  }
}

case class Polygon(
    vects: ArrayBuffer[Vect] = ArrayBuffer[Vect]()
) {
  import Vect._
  // add vector in place
  def +=(v: Vect) { vects += v }
  // clone with added vector
  def +(v: Vect): Polygon = Polygon(vects :+ v)
  // normalize so that every coordinate is non negative
  var shift = Vect()
  var size = Vect()
  def normalize {
    var minx = INFINITE_COORD
    var miny = INFINITE_COORD
    var maxx = -INFINITE_COORD
    var maxy = -INFINITE_COORD
    for (v <- vects) {
      if (v.x < minx) minx = v.x
      if (v.y < miny) miny = v.y
      if (v.x > maxx) maxx = v.x
      if (v.y > maxy) maxy = v.y
    }
    val min = Vect(minx, miny)
    val max = Vect(maxx, maxy)
    shift = min.inverse
    size = max - min
    for (v <- vects) v += shift
  }
  // clone
  def cclone: Polygon = {
    val c = Polygon()
    for (v <- vects) c += v.cclone
    c
  }
  // clone normalized
  def normalized: Polygon = {
    val pg = cclone
    pg.normalize
    pg
  }
  // report svg
  def reportSVG(
    color: String = "#000000"
  ): String = {
    val points = (for (v <- vects) yield v.asintpair).mkString(" ")
    s"""
    	|<svg width="${size.x.toInt}" height="${size.y.toInt}" style="position:absolute;top:0px;left:0px;">
    	|<polygon points="$points" style="fill:$color;stroke-width:0;">
    	|</svg>
    """.stripMargin
  }
}

case class Arrow(
    from: Vect = Vect(),
    to: Vect = Vect(),
    color: String = "yellow",
    widthfactor: Double = 0.1,
    handlelength: Double = 0.7,
    headfactor: Double = 0.2,
    constantwidth: Double = 0.0
) {
  import Vect._
  val cw = (constantwidth != 0.0)
  val diff = to - from
  val width = if (cw) constantwidth else diff.d * widthfactor
  val bottomright = if (cw) diff.normalized(constantwidth / 2.0).rotated(0.5) else diff.normalized(width / 2.0).rotated(0.5)
  val bottomleft = bottomright.inverse
  val handle = if (cw) diff.normalized(diff.d - 3.0 * constantwidth) else diff.normalized(diff.d * handlelength)
  val headfromright = bottomright + handle
  val headfromleft = bottomleft + handle
  val headtoright = headfromright + (if (cw) bottomright * 2.0 else bottomright.normalized(diff.d * headfactor))
  val headtoleft = headfromleft + (if (cw) bottomleft * 2.0 else bottomleft.normalized(diff.d * headfactor))

  val pg = Polygon(ArrayBuffer(bottomright, headfromright, headtoright, diff, headtoleft, headfromleft, bottomleft))
  pg.normalize

  def svgorig = to - pg.vects(3)

  def svg = pg.reportSVG(color)
}