import sbtcrossproject.CrossPlugin.autoImport.crossProject

name := "hookup"
organization in ThisBuild := "com.outr"
version in ThisBuild := "1.0.0"
scalaVersion in ThisBuild := "2.12.8"
crossScalaVersions in ThisBuild := List("2.12.8", "2.11.12")
resolvers in ThisBuild += Resolver.sonatypeRepo("releases")
resolvers in ThisBuild += Resolver.sonatypeRepo("snapshots")
scalacOptions in ThisBuild ++= Seq("-unchecked", "-deprecation", "-feature")

publishTo in ThisBuild := sonatypePublishTo.value
sonatypeProfileName in ThisBuild := "com.outr"
publishMavenStyle in ThisBuild := true
licenses in ThisBuild := Seq("MIT" -> url("https://github.com/outr/hookup/blob/master/LICENSE"))
sonatypeProjectHosting in ThisBuild := Some(xerial.sbt.Sonatype.GitHubHosting("outr", "hookup", "matt@outr.com"))
homepage in ThisBuild := Some(url("https://github.com/outr/hookup"))
scmInfo in ThisBuild := Some(
  ScmInfo(
    url("https://github.com/outr/hookup"),
    "scm:git@github.com:outr/hookup.git"
  )
)
developers in ThisBuild := List(
  Developer(id="darkfrog", name="Matt Hicks", email="matt@matthicks.com", url=url("http://matthicks.com"))
)

val profigVersion = "2.3.4"
val scribeVersion = "2.7.1"
val reactifyVersion = "3.0.3"
val scalacticVersion = "3.0.5"
val scalaTestVersion = "3.0.5"

lazy val root = crossProject(JSPlatform, JVMPlatform).in(file("."))
  .settings(
    name := "hookup",
    resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases",
    libraryDependencies ++= Seq(
      "com.outr" %%% "profig" % profigVersion,
      "com.outr" %%% "scribe" % scribeVersion,
      "com.outr" %%% "reactify" % reactifyVersion,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scalactic" %%% "scalactic" % scalacticVersion,
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test"
    )
  )
  .jsSettings(
    test := {}
  )

lazy val js = root.js
lazy val jvm = root.jvm
