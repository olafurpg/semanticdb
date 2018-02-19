lazy val core = project.settings(
  scalaVersion := "2.12.4",
  libraryDependencies ++= List(
    "org.scala-lang.modules" % "scala-asm" % "5.1.0-scala-2",
    "org.scalameta" %% "metap" % "3.3.0",
    "org.scalameta" %% "semanticdb" % "3.3.0"
  )
)
