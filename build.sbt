name := """rogue-zero"""

version := "1.0-SNAPSHOT"

organization := "org.mtrupkin"

scalaVersion := "2.11.7"

licenses += ("MIT", url("http://www.opensource.org/licenses/mit-license.html"))

fork in run := true

resolvers ++= Seq(
	Resolver.url("me.mtrupkin ivy repo", url("http://dl.bintray.com/mtrupkin/ivy/"))(Resolver.ivyStylePatterns)
)

libraryDependencies ++= Seq(
  "org.mtrupkin.console" %% "console-core" % "0.8",
  "org.scalafx" %% "scalafx" % "8.0.60-R9"
)