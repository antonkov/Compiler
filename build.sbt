name := "Compiler"

version := "1.0"

scalaVersion := "2.11.7"

javaCppPresetLibs ++= Seq("llvm" -> "3.7.0")
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "1.0.1"


