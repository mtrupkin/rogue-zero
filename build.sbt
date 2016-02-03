name := """rogue-zero"""

version := "1.0-SNAPSHOT"

organization := "org.mtrupkin"

scalaVersion := "2.11.7"

resolvers ++= Seq(
	Resolver.url("me.mtrupkin ivy repo", url("http://dl.bintray.com/mtrupkin/ivy/"))(Resolver.ivyStylePatterns)
)

libraryDependencies ++= Seq(
  "org.mtrupkin.console" %% "console-core" % "0.8"
)