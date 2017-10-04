import sbt._

object Dependencies {  

  ////////////////////////////////////////////////////////////////////////////////

  lazy val commonsIo = 
  	"commons-io" % "commons-io" % "2.5"  

  lazy val commonsLang = 
  	"commons-lang" % "commons-lang" % "2.6"

  lazy val scalajsScripts = 
  	"com.vmunier" %% "scalajs-scripts" % "1.0.0"

  lazy val playSilhouette = 
  	"com.mohiva" %% "play-silhouette" % "4.0.0"

  lazy val playSilhouettePasswordBcrypt = 
  	"com.mohiva" %% "play-silhouette-password-bcrypt" % "4.0.0"

  lazy val playSilhouettePersistence = 
  	"com.mohiva" %% "play-silhouette-persistence" % "4.0.0"

  lazy val playSilhouetteCryptoJca = 
  	"com.mohiva" %% "play-silhouette-crypto-jca" % "4.0.0"

  lazy val playSilhouetteTestkit = 
  	"com.mohiva" %% "play-silhouette-testkit" % "4.0.0" % "test"  

  lazy val playMailer = 
  	"com.typesafe.play" %% "play-mailer" % "5.0.0"

  lazy val playAkkaQuartzScheduler = 
  	"com.enragedginger" %% "akka-quartz-scheduler" % "1.5.0-akka-2.4.x"

  lazy val play2Reactivemongo = 
  	"org.reactivemongo" %% "play2-reactivemongo" % "0.12.3" excludeAll(ExclusionRule("com.typesafe.play", "play-iteratees_2.11"))  

  lazy val playSilhouettePersistenceReactivemongo = 
  	"com.mohiva" %% "play-silhouette-persistence-reactivemongo" % "4.0.0"

  lazy val scalaGuice = 
  	"net.codingwell" %% "scala-guice" % "4.0.1"

  lazy val ficus = 
  	"com.iheart" %% "ficus" % "1.2.6"  

  lazy val upickleServer = 
  	"com.lihaoyi" %% "upickle" % "0.4.3"  

  ////////////////////////////////////////////////////////////////////////////////
  
  lazy val updateDeps = Seq(
  	commonsIo
  )

  lazy val serverDeps = Seq(
  	commonsLang,
  	scalajsScripts,
  	playSilhouette,
  	playSilhouettePasswordBcrypt,
  	playSilhouettePersistence,
  	playSilhouetteCryptoJca,
  	playSilhouetteTestkit,
  	playMailer,
  	playAkkaQuartzScheduler,
  	play2Reactivemongo,
  	playSilhouettePersistenceReactivemongo,
  	scalaGuice,
  	ficus,  	
  	upickleServer
  )

  ////////////////////////////////////////////////////////////////////////////////

}
