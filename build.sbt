import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

name := "hookup"
organization in ThisBuild := "com.outr"
version in ThisBuild := "2.0.3-SNAPSHOT"
scalaVersion in ThisBuild := "2.13.0"
crossScalaVersions in ThisBuild := List("2.13.0", "2.12.8", "2.11.12")
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

val profigVersion = "2.3.6"
val scribeVersion = "2.7.8"
val reactifyVersion = "3.0.4"
val scalaTestVersion = "3.1.0-SNAP13"

lazy val hookup = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(
    name := "hookup",
    resolvers += "Artima Maven Repository" at "http://repo.artima.com/releases",
    libraryDependencies ++= Seq(
      "com.outr" %%% "profig" % profigVersion,
      "com.outr" %%% "scribe" % scribeVersion,
      "com.outr" %%% "reactify" % reactifyVersion,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value,
      "org.scalatest" %%% "scalatest" % scalaTestVersion % "test"
    )
  )
  .jsSettings(
    test := {}
  )