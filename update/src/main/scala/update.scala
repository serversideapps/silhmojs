package update

case class CopyTask(
    srcPath: String,
    destPath: String
) {
  var uptodate = 0
  var changed = 0
  var direxists = 0
  var createdir = 0
}

object Update extends App {
  val sep = java.io.File.separator
  val del = "----------------------------------------"
  def fullPath(root: String, path: String) = root + path
  def pathFromParts(parts: List[String]) = parts.mkString(sep) + sep
  def fullPathFromParts(parts: List[String]) = fullPath(pathFromParts(parts.reverse.tail.reverse), parts.reverse.head)

  def pathExists(path: String): Boolean = new java.io.File(path).exists

  val srcRootParts = List(".")
  val srcRoot = pathFromParts(srcRootParts)
  val destRootParts = List("..", "chessapp")
  val destRoot = pathFromParts(destRootParts)
  val destProjectRoot = pathFromParts(destRootParts ::: List("project"))

  val scalaVersion = "2.11"
  val clientoptjs = "client-opt.js"

  val tasks = List(
    CopyTask(
      srcRoot + pathFromParts(List("server", "app")),
      destRoot + pathFromParts(List("app"))
    ),
    CopyTask(
      srcRoot + pathFromParts(List("server", "public")),
      destRoot + pathFromParts(List("public"))
    ),
    CopyTask(
      srcRoot + pathFromParts(List("server", "conf")),
      destRoot + pathFromParts(List("conf"))
    ),
    CopyTask(
      srcRoot + pathFromParts(List("shared", "src", "main", "scala", "shared")),
      destRoot + pathFromParts(List("app", "shared"))
    )
  )

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

  case class CollectFilesResult(
    allfiles: List[String],
    alldirs: List[String]
  )

  def CollectFiles(
    path: String,
    dirnames: List[String] = List[String](),
    recursive: Boolean = false,
    setfiles: List[String] = List[String](),
    setdirs: List[String] = List[String]()
  ): CollectFilesResult = {
    val d = new java.io.File(path)

    if (!(d.exists && d.isDirectory)) return CollectFilesResult(setfiles, setdirs)

    val root = (setdirs.length == 0)

    var allfiles = setfiles
    var alldirs = setdirs

    val all = d.listFiles
    val dirs = all.filter(_.isDirectory).filter(_.getName != "target")
    val files = all.filter(_.isFile)

    if (recursive) for (dir <- dirs) {
      val cfsr = CollectFiles(dir.getAbsolutePath, dirnames :+ dir.getName, recursive, allfiles, alldirs)
      allfiles = cfsr.allfiles
      alldirs = cfsr.alldirs
    }

    allfiles = allfiles ::: ((for (f <- files) yield (dirnames :+ f.getName).mkString(sep)).toList)

    alldirs = ((for (d <- dirs) yield (dirnames :+ d.getName).mkString(sep)).toList) ::: alldirs

    CollectFilesResult(allfiles, alldirs)
  }

  var copies = scala.collection.mutable.ArrayBuffer[String]()
  var createdirs = scala.collection.mutable.ArrayBuffer[String]()

  def collect() {
    for (task <- tasks) {
      val cfsr = CollectFiles(task.srcPath, recursive = true)
      val srcs = cfsr.allfiles
      for (src <- srcs) {
        val srcpath = task.srcPath + src
        val destpath = task.destPath + src
        val srcfile = new java.io.File(srcpath)
        val destfile = new java.io.File(destpath)
        val destexists = destfile.exists
        val srclastmod = srcfile.lastModified
        val destlastmod = destfile.lastModified
        val uptodate = destexists && (destlastmod >= srclastmod)
        if (uptodate) task.uptodate += 1 else task.changed += 1
        if (!uptodate) {
          copies += s"""copy "$srcpath" "$destpath""""
        }
      }
      val dirs = cfsr.alldirs
      if (!pathExists(task.destPath)) createdirs += s"""mkdir "${task.destPath}""""
      for (dir <- dirs) {
        val destpath = task.destPath + dir
        val destfile = new java.io.File(destpath)
        val destexists = destfile.exists
        if (destexists) task.direxists += 1 else task.createdir += 1
        if (!destexists) {
          createdirs += s"""mkdir "$destpath""""
        }
      }
    }
  }

  def change_version {
    var chesshtmlpath = fullPathFromParts(destRootParts ::: List("app", "views", "chess.scala.html"))
    if (!pathExists(chesshtmlpath)) {
      chesshtmlpath = fullPath(srcRoot, "chess.scala.html")
    }
    val content = ReadFileToString(chesshtmlpath)
    val parts = content.split(s"$clientoptjs$sep?v")
    val quote = """""""
    val parts2 = parts(1).split(quote)
    val version = parts2(0).toInt
    val newversion = version + 1
    val newcontent = parts(0) + s"$clientoptjs?v" + newversion + quote + parts2(1)
    println("new chess.scala.html version : " + newversion)
    WriteStringToFile("chess.scala.html", newcontent)
  }

  def mkdirorrem(path: String): String = {
    if (pathExists(path)) return s"rem $path exists"
    s"""mkdir "$path""""
  }

  def update {

    collect()

    change_version

    println(s"$del\nSummary\n$del")

    for (task <- tasks) {
      println(task)
      println(s"--> up to date: ${task.uptodate}, changed:   ${task.changed}")
      println(s"--> dir exists: ${task.direxists}, createdir: ${task.createdir}")
    }

    val bat = s"""
  	|${mkdirorrem(destRoot)}
  	|${mkdirorrem(destProjectRoot)}
  	|${createdirs.mkString("\n")}
    |${copies.mkString("\n")}
    |copy "${fullPathFromParts(srcRootParts ::: List("client", "target", s"scala-$scalaVersion", clientoptjs))}" "${pathFromParts(destRootParts ::: List("public", "javascripts"))}" /Y
    |copy "${fullPathFromParts(srcRootParts ::: List("client", "target", s"scala-$scalaVersion", "client-jsdeps.min.js"))}" "${pathFromParts(destRootParts ::: List("public", "javascripts"))}" /Y    
    |copy "chess.scala.html" "${pathFromParts(destRootParts ::: List("app", "views"))}" /Y
    |pause
    |cd $srcRoot
    |call pre.bat
    |git add -A .
    |git commit -m "$commitname"
    |git push origin master
    |cd $destRoot
    |call pre.bat
    |git add -A .
    |git commit -m "$commitname"
    |git push origin master
    |pause
  """.stripMargin

    WriteStringToFile("c.bat", bat)

  }

  def correctcreds(orig: String): String = orig.replaceAll("serversideapps", "scalasbtguiapps").replaceAll("silhmojs", "chessapp")

  def build {
    val prebat = ReadFileToString(fullPath(srcRoot, "pre.bat"))
    val prebatnew = correctcreds(prebat)

    val gitconfig = ReadFileToString(fullPath(srcRoot, "gitconfig.txt"))
    val gitconfignew = correctcreds(gitconfig)

    val gitignorenew = s"""
/gitconfig.txt
/pre.bat
"""

    val buildsbtnew = s"""
name := "chessapp"

organization := "chessappmaker"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.11"

resolvers := Resolver.jcenterRepo +: resolvers.value

resolvers := ("Atlassian Releases" at "https://maven.atlassian.com/public/") +: resolvers.value

libraryDependencies ++= Seq(
  "com.mohiva" %% "play-silhouette" % "4.0.0",
  "com.mohiva" %% "play-silhouette-password-bcrypt" % "4.0.0",
  "com.mohiva" %% "play-silhouette-persistence" % "4.0.0",
  "com.mohiva" %% "play-silhouette-crypto-jca" % "4.0.0",  
  "net.codingwell" %% "scala-guice" % "4.0.1",
  "com.iheart" %% "ficus" % "1.2.6",
  "com.typesafe.play" %% "play-mailer" % "5.0.0",
  "com.enragedginger" %% "akka-quartz-scheduler" % "1.5.0-akka-2.4.x",  
  "com.mohiva" %% "play-silhouette-testkit" % "4.0.0" % "test",
  "org.reactivemongo" %% "play2-reactivemongo" % "0.12.3" excludeAll(ExclusionRule("com.typesafe.play", "play-iteratees_2.11")),
  "com.mohiva" %% "play-silhouette-persistence-reactivemongo" % "4.0.0",
  "com.lihaoyi" %% "upickle" % "0.4.3",
  "commons-lang" % "commons-lang" % "2.6",
  "org.scalatestplus.play" %% "scalatestplus-play" % "2.0.0" % Test,
  specs2 % Test,
  cache,
  filters
)

routesGenerator := InjectedRoutesGenerator

routesImport += "utils.route.Binders._"

enablePlugins(PlayScala)
"""

    val buildpropertiesnew = s"""
sbt.version=0.13.15
"""

    val pluginssbtnew = s"""
logLevel := Level.Warn

resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.url("heroku-sbt-plugin-releases",url("https://dl.bintray.com/heroku/sbt-plugins/"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.5.4")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")
"""

    WriteStringToFile(fullPath(destRoot, "pre.bat"), prebatnew)
    WriteStringToFile(fullPath(destRoot, "gitconfig.txt"), gitconfignew)
    WriteStringToFile(fullPath(destRoot, ".gitignore"), gitignorenew)
    WriteStringToFile(fullPath(destRoot, "build.sbt"), buildsbtnew)

    WriteStringToFile(fullPath(destProjectRoot, "build.properties"), buildpropertiesnew)
    WriteStringToFile(fullPath(destProjectRoot, "plugins.sbt"), pluginssbtnew)
  }

  val commitname = args.head

  if (commitname != "build") update else build
}

