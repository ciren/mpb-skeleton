import sbt._

scalaVersion := "2.12.8"

lazy val cilib = RootProject(file("cilib"))

lazy val root =
  project.in(file("."))
    .dependsOn(cilib)
    .settings(Seq(
      resolvers ++= Seq(
        Resolver.sonatypeRepo("releases"),
        "bintray/non".at("http://dl.bintray.com/non/maven"),
        "bmjames Bintray Repo".at("https://dl.bintray.com/bmjames/maven")
      )
    ))
