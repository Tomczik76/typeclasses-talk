name := "Typeclass"

version := "0.1"

scalaVersion := "2.12.8"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.14.0"