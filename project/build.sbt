name := "project"

version := "1.0-SNAPSHOT"

organization := "edu.arizona.cs"

scalaVersion := "2.12.6"

scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation")

libraryDependencies ++= {
  val procVer = "7.4.2"

  Seq(
    "org.clulab" %% "processors-main" % procVer,
    "org.clulab" %% "processors-corenlp" % procVer,
    "org.clulab" %% "processors-odin" % procVer,
    "org.clulab" %% "processors-modelsmain" % procVer,
    "org.clulab" %% "processors-modelscorenlp" % procVer,
  )
}
