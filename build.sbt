name := "sequel"

version := "1.0"

scalaVersion := "2.12.6"


libraryDependencies ++= Seq(
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "com.lihaoyi" %% "pprint" % "0.5.3",
  "com.lihaoyi" %% "utest" % "0.5.3" % "test"
)

testFrameworks += new TestFramework("utest.runner.Framework")