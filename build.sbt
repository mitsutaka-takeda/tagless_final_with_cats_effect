name := "tagless_final_with_cats_effect"

version := "0.1"

scalaVersion := "2.12.8"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4")

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "1.2.0",
  "org.postgresql" % "postgresql" % "9.4.1208",
  "io.getquill" %% "quill-async-postgres" % "3.0.1",
//  "io.getquill" %% "quill-jdbc" % "3.0.1"
)
