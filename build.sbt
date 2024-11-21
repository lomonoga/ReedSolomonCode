import sbt._

ThisBuild / version := "1.0.0"

ThisBuild / scalaVersion := "2.13.15"

lazy val domain = project

lazy val boot = project
  .dependsOn(domain)

lazy val root = Project("ReedSolomonCode", file("."))
  .settings(
    organization := "com.lomonoga",
    name := "ReedSolomonCode"
  )
  .aggregate(
    domain,
    boot
  )
  .dependsOn(
    boot
  )
