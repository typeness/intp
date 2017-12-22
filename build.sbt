import AssemblyKeys._


name := "intp"

version := "0.3.1"
assemblySettings
jarName in assembly := name.value + "-" + version.value+ ".jar"

scalaVersion := "2.12.2"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
scalacOptions ++= Seq(
  "-target:jvm-1.8",
  "-encoding", "UTF-8",
  "-deprecation",
  "-Xfuture",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard",
  "-Xlint:-unused,_"
)

initialCommands in console :=  """import io.github.typeness.intp._"""