name := "better-swagger"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "io.swagger.parser.v3" % "swagger-parser" % "2.0.23",
  "org.slf4j" % "slf4j-nop" % "1.7.30" % Optional
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xsource:3")
