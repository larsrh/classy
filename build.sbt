organization := "info.hupel"
name := "classy"
crossPaths := false
autoScalaLibrary := false
homepage := Some(url("http://lars.hupel.info/libisabelle/"))
licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

enablePlugins(LibisabellePlugin)
moduleName := name.value
isabelleVersions := List("2016", "2016-1")
isabelleSessions in Compile := List("Classy", "HOL-Classy")

resolvers += Resolver.sonatypeRepo("releases")
libraryDependencies += "info.hupel" % "multi-isabelle" % "0.1.1"

pomExtra := (
  <developers>
    <developer>
      <id>larsrh</id>
      <name>Lars Hupel</name>
      <url>http://lars.hupel.info</url>
    </developer>
  </developers>
  <scm>
    <connection>scm:git:github.com/larsrh/classy.git</connection>
    <developerConnection>scm:git:git@github.com:larsrh/classy.git</developerConnection>
    <url>https://github.com/larsrh/multi-isabelle</url>
  </scm>
)

credentials += Credentials(
  Option(System.getProperty("build.publish.credentials")) map (new File(_)) getOrElse (Path.userHome / ".ivy2" / ".credentials")
)


// Release stuff

import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  ReleaseStep(action = Command.process("publishSigned", _)),
  setNextVersion,
  commitNextVersion,
  ReleaseStep(action = Command.process("sonatypeReleaseAll", _))
)

