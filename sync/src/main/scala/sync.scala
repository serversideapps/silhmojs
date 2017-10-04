package sync

import java.io._
import scala.collection.mutable.ArrayBuffer

object Constants {
  val scalaVersion = "2.11"
  val clientoptjs = "client-opt.js"
  val clientjsdepsminjs = "client-jsdeps.min.js"
  val targetname = "chessapp"
  def scaladir = s"scala-$scalaVersion"
}

case class MyPath(parts: String*) {
  import Utils._
  def toname: String = {
    if (parts.length <= 0) return "."
    parts.mkString(sep)
  }
  def topath: String = toname + sep
  def getname: String = {
    if (parts.length <= 0) return ""
    parts.reverse.head
  }
  def length = parts.length

  def -(mp: MyPath): MyPath = {
    for (i <- 0 to mp.length - 1) {
      if (parts(i) != mp.parts(i)) return MyPath(parts: _*)
    }
    MyPath(parts.slice(mp.length, parts.length): _*)
  }

  def +(mp: MyPath): MyPath = {
    val allparts = mp.parts.toList ::: parts.toList
    MyPath(allparts.filter(!specialdir(_)): _*)
  }

  def normalized: MyPath = {
    val newparts = ArrayBuffer[String]()
    for (p <- parts) {
      if ((p == parentdir) && (newparts.length > 0)) newparts.remove(newparts.length - 1) else newparts += p
    }
    MyPath(newparts: _*)
  }
}

case class MyDirEntry(
    mypath: MyPath = MyPath(),
    isdir: Boolean = false,
    lastmod: Long = 0,
    exists: Boolean = false
) {
  import Utils._
  def isfile = !isdir

  def toname = mypath.toname
  def topath = mypath.topath

  def getname: String = {
    if (!exists) return ""
    mypath.getname
  }

  def tofile = new File(toname)

  def listentries: MyDirEntries = {
    val md = MyDirEntries()
    if ((!exists) || (!isdir)) return md
    val files = tofile.listFiles
    for (f <- files) md += MyDirEntryFromPath(f.getAbsolutePath)
    md
  }

  def reportPrintable: String = s"""$toname $isdir $lastmod"""

  def -(mp: MyPath): MyDirEntry = this.copy(mypath = mypath - mp)

  def +(mp: MyPath): MyDirEntry = MyDirEntryFromMyPath(mypath + mp)

  def plus(mp: MyPath): MyDirEntry = this.copy(mypath = mypath + mp)
}

case class SyncItem(
  from: MyDirEntry = MyDirEntry(),
  to: MyDirEntry = MyDirEntry()
)

case class SyncItems() extends ArrayBuffer[SyncItem] {
  import Utils._
  def addDir(setfrom: MyPath, setto: MyPath, filterrecdirsfunc: FDEF = Some((e: MyDirEntry) => (e.getname != "target"))) {
    val from = MyDirEntryFromMyPath(setfrom).mypath
    val to = MyDirEntryFromMyPath(setto).mypath.normalized

    val entries = Utils.collect(from, filterrecdirsfunc = filterrecdirsfunc)

    val dirs = entries.getdirs.sortbycomplexity

    for (r <- dirs) {
      val d = (r - from) + to
      if (!d.exists) {
        this += SyncItem(r, (r - from).plus(to))
      }
    }

    val files = entries.getfiles.sortbycomplexity

    for (r <- files) {
      val d = (r - from) + to
      if ((!d.exists) || (r.lastmod > d.lastmod)) {
        this += SyncItem(r, (r - from).plus(to))
      }
    }
  }

  def addFile(setfrom: MyPath, setto: MyPath, unconditional: Boolean = false): Boolean = {
    val from = MyDirEntryFromMyPath(setfrom).mypath
    val to = MyDirEntryFromMyPath(setto).mypath.normalized

    val fromde = MyDirEntryFromMyPath(from)
    val tode = MyDirEntryFromMyPath(to)

    if (!fromde.exists) {
      println("error, from file does not exist " + fromde.mypath.toname)
      return false
    } else if ((!tode.exists) || (fromde.lastmod > tode.lastmod) || unconditional) {
      this += SyncItem(fromde, tode)
      return true
    }
    false
  }

  def reportPrintable: String = (for (item <- this) yield item.from.reportPrintable + "\n -> " + item.to.reportPrintable).mkString("\n")

  def reportWindowsBatch: String = (for (item <- this) yield {
    if (item.from.isdir) quotedcommand("mkdir", item.to.mypath.toname) else
      quotedcommand("copy", item.from.mypath.toname, item.to.mypath.toname)
  }).mkString("\n")
}

case class MyDirEntries() extends ArrayBuffer[MyDirEntry] {
  def myfilter(ff: (MyDirEntry) => Boolean): MyDirEntries = {
    val md = MyDirEntries()
    for (e <- this) {
      if (ff(e)) md += e
    }
    md
  }

  def getdirs = myfilter((e: MyDirEntry) => e.isdir)
  def getfiles = myfilter((e: MyDirEntry) => e.isfile)

  def append(md: MyDirEntries) {
    for (e <- md) this += e
  }

  def reportPrintable: String = (for (e <- this) yield e.reportPrintable).mkString("\n")

  def frombuffer(buff: ArrayBuffer[MyDirEntry]): MyDirEntries = {
    val md = MyDirEntries()
    for (e <- buff) md += e
    md
  }

  def sortbycomplexity: MyDirEntries = frombuffer(this.sortWith((a, b) => a.mypath.length < b.mypath.length))

  def -(mp: MyPath): MyDirEntries = {
    val md = MyDirEntries()
    for (e <- this) md += (e - mp)
    md
  }

  def +(mp: MyPath): MyDirEntries = {
    val md = MyDirEntries()
    for (e <- this) md += (e + mp)
    md
  }
}

object Utils {
  val sep = File.separator
  val regexsep = "\\" + sep
  val regexpesc = "\\"

  val thisdir = "."
  val parentdir = ".."

  def specialdir(d: String) = ((d == thisdir))

  val quote = """""""

  def quotedcommand(command: String, args: String*): String = command + " " + (for (arg <- args) yield quote + arg + quote).mkString(" ")

  def WriteStringToFile(path: String, content: String) {
    org.apache.commons.io.FileUtils.writeStringToFile(
      new java.io.File(path),
      content,
      null.asInstanceOf[String]
    )
  }

  def ReadFileToString(path: String): String = {
    val f = new java.io.File(path)
    if (!f.exists()) return null
    org.apache.commons.io.FileUtils.readFileToString(
      f,
      null.asInstanceOf[String]
    )
  }

  def MyDirEntryFromMyPath(mypath: MyPath): MyDirEntry = {
    val f = new File(mypath.toname)
    if (!f.exists) return MyDirEntry()
    MyDirEntry(
      mypath = MyPathFromPath(f.getAbsolutePath),
      isdir = f.isDirectory,
      lastmod = f.lastModified,
      exists = true
    )
  }

  def MyPathFromPath(path: String): MyPath = {
    val parts = path.split(regexsep).filter(!specialdir(_))
    if (parts.length <= 1) return MyPath(parts: _*)
    val partsrev = parts.reverse
    if (partsrev.head == "") return MyPath(partsrev.tail.reverse: _*)
    MyPath(parts: _*)
  }

  def MyDirEntryFromPath(path: String) = MyDirEntryFromMyPath(MyPathFromPath(path))

  type FDEF = Option[((MyDirEntry) => Boolean)]

  def collect(
    mypath: MyPath,
    entries: MyDirEntries = MyDirEntries(),
    recursive: Boolean = true,
    filterrecdirsfunc: FDEF = None
  ): MyDirEntries = {
    val de = MyDirEntryFromMyPath(mypath)

    if (!(de.exists && de.isdir)) return entries

    entries += de

    val le = de.listentries

    entries.append(le.getfiles)

    if (recursive) for (d <- le.getdirs) {
      var ok = true
      if (!filterrecdirsfunc.isEmpty) ok = filterrecdirsfunc.get(d)
      if (ok) collect(mypath = d.mypath, entries = entries, recursive = recursive, filterrecdirsfunc = filterrecdirsfunc)
    }

    entries
  }

  def change_version(
    local: MyPath,
    remote: MyPath,
    jsnames: String*
  ) {
    val localde = MyDirEntryFromMyPath(local)
    val remotede = MyDirEntryFromMyPath(remote)
    var content = ReadFileToString(if (remotede.exists) remotede.toname else localde.toname)
    for (jsname <- jsnames) {
      val parts = content.split(s"$jsname$regexpesc?v")
      val quote = """""""
      val parts2 = parts(1).split(quote)
      val version = parts2(0).toInt
      val newversion = version + 1
      content = parts(0) + s"$jsname?v" + newversion + quote + parts2.tail.mkString(quote)
    }
    WriteStringToFile(localde.toname, content)
  }
}

object Sync extends App {
  import Utils._
  import Constants._

  val commitname = if (args.length > 0) args(0) else "Test"

  val syncitems = SyncItems()

  syncitems.addDir(MyPath("server", "app"), MyPath("..", targetname, "app"))
  syncitems.addDir(MyPath("server", "public"), MyPath("..", targetname, "public"))
  syncitems.addDir(MyPath("server", "conf"), MyPath("..", targetname, "conf"))
  syncitems.addDir(MyPath("shared", "src", "main", "scala", "shared"), MyPath("..", targetname, "app", "shared"))
  val addopt = syncitems.addFile(
    MyPath("client", "target", scaladir, clientoptjs),
    MyPath("..", targetname, "public", "javascripts", clientoptjs)
  )
  val adddeps = syncitems.addFile(
    MyPath("client", "target", scaladir, clientjsdepsminjs),
    MyPath("..", targetname, "public", "javascripts", clientjsdepsminjs)
  )

  if (addopt || adddeps) {
    change_version(
      MyPath("chess.scala.html"),
      MyPath("..", "chessapp", "app", "views", "chess.scala.html"),
      clientjsdepsminjs,
      clientoptjs
    )
  }

  syncitems.addFile(MyPath("chess.scala.html"), MyPath("..", targetname, "app", "views"))

  val sbat = syncitems.reportWindowsBatch

  val bat = s"""
  	|$sbat
    |pause
    |call pre.bat
    |git add -A .
    |git commit -m "$commitname"
    |git push origin master
    |cd ..\\$targetname
    |call pre.bat
    |git add -A .
    |git commit -m "$commitname"
    |git push origin master
    |pause
  """.stripMargin

  println(bat)

  WriteStringToFile("c.bat", bat)

}