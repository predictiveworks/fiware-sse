name              := "fiware-sse"

version           := "1.0.0"

organization      := "de.kp.fiware"

scalaVersion := "2.12.13"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

resolvers += "Local Maven Repository" at "file:///"+Path.userHome+"/.m2/repository"

libraryDependencies ++= Seq(
   "com.typesafe.akka" %% "akka-http" % "10.2.4",
   "com.typesafe.akka" %% "akka-stream" % "2.6.13",
   "com.github.scopt" %% "scopt" % "4.0.0",
   "com.google.code.gson" % "gson" % "2.8.6",
   "org.bouncycastle" % "bcpkix-jdk15on" % "1.68"
)
