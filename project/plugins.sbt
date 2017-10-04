
logLevel := Level.Warn

resolvers += "Atlassian Releases" at "https://maven.atlassian.com/public/"

resolvers += "Typesafe repository" at "https://repo.typesafe.com/typesafe/releases/"

resolvers += Resolver.url("heroku-sbt-plugin-releases",url("https://dl.bintray.com/heroku/sbt-plugins/"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.2.0")

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.5.4")

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.6.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.3")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.15")

addSbtPlugin("com.vmunier" % "sbt-web-scalajs" % "1.0.4")

addSbtPlugin("com.typesafe.sbt" % "sbt-gzip" % "1.0.0")

addSbtPlugin("com.typesafe.sbt" % "sbt-digest" % "1.1.1")

addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-plugin" % "4.0.0")
