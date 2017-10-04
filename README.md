**Chessapp** is an online chess application which allows you to play real time rated games at various time controls against *human* and *AI* opponents. Beyond standard chess it supports variants *Four Player*, *Atomic*, *Antichess* and *Flick*.

# Language and build tool

The application is implemented as a single [Scala](https://www.scala-lang.org/) / [Sbt](http://www.scala-sbt.org/) project.

# Server

The server is based on [Play Framework 2.5](https://www.playframework.com/documentation/2.5.x/ScalaHome).

# Client

The client is based on [Scala.js](https://www.scala-js.org/) which allows Scala code to be compiled to Javascript. It makes use of the Scala.js facade for the D3.js library [scala-js-d3](https://github.com/spaced/scala-js-d3).

# Server - client integration

The server and the client are part of the same project and both the server and the client are built when you issue the usual "compile" command in Sbt. The integration is based on the template [play-with-scalajs-example](https://github.com/vmunier/play-with-scalajs-example).

The project has three building blocks: the **server**, the **client** and **shared code**. Since both the server and the client are written in Scala, this allows to have shared code between them. This is particularly useful in a chess application because you don't have to duplicate the complicated move generation logic for the client.

# Authentication

The project uses the [Silhoutte](https://www.silhouette.rocks/docs) module for authentication. In particular it is based on the [play-silhouette-seed](https://github.com/mohiva/play-silhouette-seed) example.

To avoid robots registration uses [Google reCAPTCHA](https://www.google.com/recaptcha/intro/). As an implementation ideas from the [play-recapthca](https://github.com/chrisnappin/play-recaptcha) module were used ( the module itself is not imported due to it having non trivial dependencies that Sbt refuses to cache ).

# Persistence

For persistence [MongoDB](https://www.mongodb.com/) is used with the [ReactiveMongo](http://reactivemongo.org/) driver.

# Serialization

For serialization of messages between the server and the client through sockets [uPickle](http://www.lihaoyi.com/upickle-pprint/upickle/) is used which is a lightweigth serialization library without complicated dependency chain. It basically serializes simple data types and case classes, but this is more than enough for the purposes of the application.

# Concurrency

For concurrency handling and timing project makes use of [Akka Actors](http://doc.akka.io/docs/akka/current/scala/actors.html) and [akka-quartz-scheduler](https://github.com/enragedginger/akka-quartz-scheduler).

# Additional resources

The project uses the [Apache Commons Lang](https://commons.apache.org/proper/commons-lang/) module for date formatting. For formatting the source code itself [Scalariform](https://github.com/scala-ide/scalariform) is used to give a unified and organized appearance of the code.