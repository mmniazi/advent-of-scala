name := "advent-of-scala"

version := "0.1"

scalaVersion := "2.13.1"

resolvers += Resolver.typesafeIvyRepo("releases")

libraryDependencies += "org.scalatest" % "scalatest_2.13" % "3.1.0" % "test"
libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0"

// Add dependency on ScalaFX library
libraryDependencies += "org.scalafx" %% "scalafx" % "12.0.2-R18"

// Determine OS version of JavaFX binaries
lazy val osName = System.getProperty("os.name") match {
  case n if n.startsWith("Linux")   => "linux"
  case n if n.startsWith("Mac")     => "mac"
  case n if n.startsWith("Windows") => "win"
  case _ => throw new Exception("Unknown platform!")
}

// Add dependency on JavaFX libraries, OS dependent
lazy val javaFXModules = Seq("base", "controls", "fxml", "graphics", "media", "swing", "web")
libraryDependencies ++= javaFXModules.map( m =>
  "org.openjfx" % s"javafx-$m" % "12.0.2" classifier osName
)

mainClass in (Compile, run) := Some("day13.Day13")
