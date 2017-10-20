addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.1.5")
addSbtPlugin("me.lessis" % "bintray-sbt" % "0.3.0")
addSbtPlugin("io.get-coursier" % "sbt-coursier" % coursier.util.Properties.version)
addSbtPlugin("com.eed3si9n" % "sbt-doge" % "0.1.5")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.7.0")
addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.1")
addSbtPlugin("com.dwijnand" % "sbt-dynver" % "2.0.0")
// exclude is a workaround for https://github.com/sbt/sbt-assembly/issues/236#issuecomment-294452474
addSbtPlugin(
  "com.eed3si9n" % "sbt-assembly" % "0.14.5" exclude ("org.apache.maven", "maven-plugin-api")
)
resolvers += Resolver.bintrayRepo("scalamacros", "maven")
