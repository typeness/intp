
name := "intp"

version := "0.5.0"
assemblyJarName in assembly := name.value + "-" + version.value+ ".jar"

scalaVersion := "2.12.4"
libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
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