resolvers ++= Seq(
	Resolver.url("org.trupkin sbt plugins", url("http://dl.bintray.com/mtrupkin/sbt-plugins/"))(Resolver.ivyStylePatterns),
	"dsol-xml" at "http://simulation.tudelft.nl/maven/",
	"SpringSource" at "http://repository.springsource.com/maven/bundles/external"
)

addSbtPlugin("org.trupkin" % "sbt-launch4j" % "0.0.8")
