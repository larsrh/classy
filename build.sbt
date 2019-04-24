organization := "info.hupel"
name := "classy"
crossPaths := false
autoScalaLibrary := false
homepage := Some(url("http://lars.hupel.info/libisabelle/"))
licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT"))

enablePlugins(LibisabellePlugin)
moduleName := name.value
isabelleVersions ~= {
  case Seq() => List(Version.Stable("2017"), Version.Stable("2018"), Version.Stable("2019-RC0"))
  case ver => ver
}
isabelleSessions in Compile := List("Classy")

libraryDependencies += "info.hupel" % "multi-isabelle" % "0.1.2"

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

releaseVcsSign := true

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommand("publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeRelease")
)

publishTo := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)
