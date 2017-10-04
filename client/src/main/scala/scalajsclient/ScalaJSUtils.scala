package scalajsclient

import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.timers._
import scala.scalajs.js.Dynamic.global

import org.singlespaced.d3js.d3
import org.singlespaced.d3js.Ops._

import scala.concurrent.duration._

import upickle.default._

import shared._

//////////////////////////////////////////////////////////////////////////

case class EngineMessage(
    action: String = "",
    name: String = "",
    command: String = "",
    buffer: String = "",
    available: List[String] = List[String]()
) {

}

abstract class AbstractEngineSocketActorJS(
    url: String = "ws://localhost:9000/ws"
) extends JSActor {
  val socket = new dom.WebSocket(url)
  socket.onopen = { (e: dom.Event) =>
    self ! SocketOpenedMsg
  }
  socket.onmessage = {
    (e: dom.MessageEvent) =>
      val rawcontent = e.data.toString
      //println("received: " + rawcontent)
      try {
        self ! read[EngineMessage](rawcontent)
      } catch { case e: Throwable => println("decoding message failed fatally " + rawcontent + " stacktrace : "); e.printStackTrace(); self ! MalformedMessage(rawcontent) }
  }
  socket.onclose = (_: dom.raw.CloseEvent) => {}
  dom.window.onbeforeunload = (_: dom.raw.BeforeUnloadEvent) => {
    socket.onclose = (_: dom.raw.CloseEvent) => {}
    socket.close()
  }
  def sendStr(str: String) {
    socket.send(str)
  }
  def sendMsg(msg: Any) {
    msg match {
      case x: EngineMessage => {
        val upicklestr = write[EngineMessage](x)
        socket.send(upicklestr)
        //println("sent: " + upicklestr)
      }
      case _ => println("error: unknown message")
    }
  }
}

//////////////////////////////////////////////////////////////////////////

case object SocketOpenedMsg

object SocketActorJS {
  import ScalaJSUtils._
  var domain = "localhost:9000"
  var prot = "ws"
  def setupfromurl(url: String) {
    val pd = getprotanddomain(url)
    if (pd._1 == "https") prot = "wss"
    domain = pd._2
    //println(s"setting up socket actor from $url , pd: $pd , prot: $prot, domain: $domain, socket url: " + getUrl)
  }
  def getUrl: String = {
    s"$prot://$domain/ws"
  }
}

abstract class SocketActorJS(
    url: String = SocketActorJS.getUrl
) extends JSActor {
  val socket = new dom.WebSocket(url)
  socket.onopen = { (e: dom.Event) =>
    self ! SocketOpenedMsg
  }
  socket.onmessage = {
    (e: dom.MessageEvent) =>
      val rawcontent = e.data.toString
      //println("received: " + rawcontent)
      try {
        val firstspace = rawcontent.indexOf(" ")
        val kind = rawcontent.substring(0, firstspace)
        val content = rawcontent.substring(firstspace + 1)
        kind match {
          case "HelloMessage" => self ! read[HelloMessage](content)
          case "TableCreationResultMessage" => self ! read[TableCreationResultMessage](content)
          case "SendTablesResultMessage" => self ! read[SendTablesResultMessage](content)
          case "SitPlayerResultMessage" => self ! read[SitPlayerResultMessage](content)
          case "SendTableResultMessage" => self ! read[SendTableResultMessage](content)
          case "StorePresentationResultMessage" => self ! read[StorePresentationResultMessage](content)
          case _ => println("unknown message kind " + kind); self ! MalformedMessage(rawcontent)
        }
      } catch { case e: Throwable => println("decoding message failed fatally " + rawcontent + " stacktrace : "); e.printStackTrace(); self ! MalformedMessage(rawcontent) }
  }
  socket.onclose = (_: dom.raw.CloseEvent) => {}
  dom.window.onbeforeunload = (_: dom.raw.BeforeUnloadEvent) => {
    socket.onclose = (_: dom.raw.CloseEvent) => {}
    socket.close()
  }
  def sendMsg(msg: Any) {
    val upicklestr = msg match {
      case x: HelloMessage => write(x)
      case x: CreateTableMessage => write(x)
      case x: StoreViewMessage => write(x)
      case x: SendTablesMessage => write(x)
      case x: DeleteTableMessage => write(x)
      case x: SitPlayerMessage => write(x)
      case x: SendTableMessage => write(x)
      case x: SendMoveMessage => write(x)
      case x: RegisterWebBoardMessage => write(x)
      case x: StorePresentationMessage => write(x)
      case _ => println("error: unknown message")
    }
    val content = msg.getClass.getSimpleName + " " + upicklestr
    socket.send(content)
    //println("sent: " + content)
  }
}

case class LoggerStatusProperty(
  backgroundcolor: String = "#ffffff"
)

object Logger {
  val MAX_ITEMS = 50
  val STATUS_PROPERTIES = Map(
    "ok" -> LoggerStatusProperty("#afffaf"),
    "failed" -> LoggerStatusProperty("#ffafaf"),
    "warn" -> LoggerStatusProperty("#ffffaf")
  )
}

case class Logitem(
    content: String = "",
    status: String = ""
) {
  import Logger._
  def statusproperty = if (STATUS_PROPERTIES.contains(status)) STATUS_PROPERTIES(status) else LoggerStatusProperty()
  def bcol = statusproperty.backgroundcolor
}

case class Logger() extends scala.collection.mutable.ArrayBuffer[Logitem] {
  import Logger._
  def log(li: Logitem) {
    this += li
    if (this.length > MAX_ITEMS) this.remove(0)
  }

  def reportHTML: String = {
    val td = """td class="devtd""""
    val logcontent = (for (li <- this.reverse) yield {
      s"""
        |<tr>
        |<$td><span style="background-color: ${li.bcol};">${li.content}</span></td>
        |</tr>
      """.stripMargin
    }).mkString("\n")

    s"""
      |<table>      
      |$logcontent
      |</table>
    """.stripMargin
  }
}

object ScalaJSUtils {
  var implccfg: ChessConfig = ChessConfig()
  def t(phrase: String) = implccfg.translate(phrase)

  def log(what: String) = global.console.log(what)

  def viewportWidth = dom.window.innerWidth
  def viewportHeight = dom.window.innerHeight

  def dgebid(id: String) = global.document.getElementById(id)
  def getpx(pxstr: String) = pxstr.replaceAll("px", "").toDouble
  def gbcrleft(e: dom.raw.HTMLElement) = e.getBoundingClientRect().left.asInstanceOf[Double]
  def gbcrleftd(e: scala.scalajs.js.Dynamic) = gbcrleft(e.asInstanceOf[dom.raw.HTMLElement])
  def gbcrleftbyid(id: String) = gbcrleftd(dgebid(id))
  def gbcrtop(e: dom.raw.HTMLElement) = e.getBoundingClientRect().top.asInstanceOf[Double]
  def gbcrtopd(e: scala.scalajs.js.Dynamic) = gbcrtop(e.asInstanceOf[dom.raw.HTMLElement])
  def gbcrtopbyid(id: String) = gbcrtopd(dgebid(id))
  def gbcrwidth(e: dom.raw.HTMLElement) = e.getBoundingClientRect().width.asInstanceOf[Double]
  def gbcrwidthd(e: scala.scalajs.js.Dynamic) = gbcrwidth(e.asInstanceOf[dom.raw.HTMLElement])
  def gbcrwidthbyid(id: String) = gbcrwidthd(dgebid(id))
  def gbcrheight(e: dom.raw.HTMLElement) = e.getBoundingClientRect().height.asInstanceOf[Double]
  def gbcrheightd(e: scala.scalajs.js.Dynamic) = gbcrheight(e.asInstanceOf[dom.raw.HTMLElement])
  def gbcrheightbyid(id: String) = gbcrheightd(dgebid(id))
  def isincrid(x: Double, y: Double, id: String): Boolean =
    {
      val cy = gbcrtopbyid(id)
      val cx = gbcrleftbyid(id)
      val w = gbcrwidthbyid(id)
      val h = gbcrheightbyid(id)
      if (y < cy) return false
      if (y > cy + h) return false
      if (x < cx) return false
      x < cx + w
    }

  def s(id: String) = d3.select("#" + id)
  def root = s("root")
  def info = s("info")
  def roote = dgebid("root")
  def roottop = gbcrtopd(roote)
  def rootleft = gbcrleftd(roote)

  def user = root.attr("user")
  def admin: Boolean = (root.attr("admin") == "true")

  case class MyFactor(factor: Double = 1.0) {
    def *(f: Double): MyFactor = MyFactor(this.factor * f)
  }

  def scaled(d: Double)(implicit f: MyFactor = MyFactor()): Double = d * f.factor
  def px(d: Double)(implicit f: MyFactor = MyFactor()): String = scaled(d) + "px"

  type MouseHandler = js.Function1[dom.MouseEvent, Unit]
  type FullMouseHandler = js.ThisFunction1[dom.raw.HTMLElement, dom.MouseEvent, Unit]
  type DragHandler = js.ThisFunction1[dom.raw.HTMLElement, dom.DragEvent, Unit]

  def svgspinner = s"""
  <svg version="1.1" 
    id="svg-spinner" 
    xmlns="http://www.w3.org/2000/svg" 
    xmlns:xlink="http://www.w3.org/1999/xlink" 
    x="0px"
    y="0px"
    viewBox="0 0 80 80" 
    xml:space="preserve">

    <path
        id="spinner" 
        fill="#D43B11" 
        d="M40,72C22.4,72,8,57.6,8,40C8,22.4,
        22.4,8,40,8c17.6,0,32,14.4,32,32c0,1.1-0.9,2-2,2
        s-2-0.9-2-2c0-15.4-12.6-28-28-28S12,24.6,12,40s12.6,
        28,28,28c1.1,0,2,0.9,2,2S41.1,72,40,72z"


        <!-- ANIMATION START -->

        <animateTransform
            attributeType="xml"
            attributeName="transform"
            type="rotate"
            from="0 40 40"
            to="360 40 40"
            dur="0.8s"
            repeatCount="indefinite"
        />
    </path>
</svg>
  """

  def clearInfo {
    info.html("")
  }

  def coverInfo(msg: String) {
    info.html("").append("div").
      style("position", "fixed").
      style("width", viewportWidth + "px").
      style("height", viewportHeight + "px").
      style("top", "0px").
      style("left", "0px").
      style("background-color", "#afafaf").
      style("border-style", "solid").
      style("border-width", "0px").
      style("opacity", "0.7").
      style("font-size", "40px").
      style("text-align", "center").
      style("padding-top", "50px").
      style("z-index", "500").
      html(msg)
  }

  def getprotanddomain(url: String): Tuple2[String, String] = {
    val parts0 = url.split(":\\/\\/")
    val parts1 = parts0(1).split("\\/")
    (parts0(0), parts1(0))
  }

  def playsound(which: String) {
    val id = which + "sound"

    val el = dgebid(id)

    el.play()
  }
}