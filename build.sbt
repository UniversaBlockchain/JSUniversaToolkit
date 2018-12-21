import org.scalajs.sbtplugin.ScalaJSPlugin.AutoImport.jsDependencies

enablePlugins(ScalaJSPlugin)

scalaVersion := "2.12.7"


lazy val commonSettings = Seq(
  version := "0.1",
  libraryDependencies ++= Seq(
    "org.scala-js" %%% "scalajs-dom" % "0.9.6",
    "org.scala-lang" % "scala-reflect" % "2.12.7",
    "org.specs2" %%% "specs2-core" % "4.2.0" % "test",
  )
)

lazy val boss = project
  .enablePlugins(ScalaJSPlugin)
  .settings(commonSettings)

lazy val crypto_cloud = project
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(boss)
  .settings(
    commonSettings,
    scalacOptions in Test ++= Seq("-Yrangepos"),
    scalacOptions ++= {
      if (isSnapshot.value)
        Seq.empty
      else {
        val a = baseDirectory.value.toURI
        val g = "https://raw.githubusercontent.com/scala-js/scala-js-dom"
        Seq(s"-P:scalajs:mapSourceURI:$a->$g/v${version.value}/", "-P:scalajs:sjsDefinedByDefault")
      }
    },
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-async" % "0.9.7",
      "ru.pavkin" %%% "scala-js-momentjs" % "0.9.2",
      "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-M13",
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.0.0-M13_2018c"
    )
  )
  // .settings(commonSettings)

lazy val contracts_api = project.in(file("contracts"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(crypto_cloud)
  .settings(
    commonSettings,
    scalacOptions in Test ++= Seq("-Yrangepos"),
    scalacOptions ++= {
      if (isSnapshot.value)
        Seq.empty
      else {
        val a = baseDirectory.value.toURI
        val g = "https://raw.githubusercontent.com/scala-js/scala-js-dom"
        Seq(s"-P:scalajs:mapSourceURI:$a->$g/v${version.value}/", "-P:scalajs:sjsDefinedByDefault")
      }
    },
    jsEnv in Test := new org.scalajs.jsenv.selenium.SeleniumJSEnv(
      org.openqa.selenium.remote.DesiredCapabilities.chrome()
    ),
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-async" % "0.9.7",
      "ru.pavkin" %%% "scala-js-momentjs" % "0.9.2",
      "io.github.cquiroz" %%% "scala-java-time" % "2.0.0-M13",
      "io.github.cquiroz" %%% "scala-java-time-tzdb" % "2.0.0-M13_2018c"
    ),
    jsDependencies += ProvidedJS / "universajs.js"
  )

val debugJs = taskKey[Unit]("Build and copy the fastOptJs files to rails folders")

debugJs := {
  (fastOptJS in Compile).value
  val src = baseDirectory.value + "/contracts/target/scala-2.12/contracts_api-fastopt.js"
  val dst = baseDirectory.value + "/dist/universa.js"
  IO.copyFile(
    new File(src),
    new File(dst)
  )
  IO.copyFile(
    new File(src+".map"),
    new File(baseDirectory.value + "/dist/universa-fastopt.js.map")
  )
  IO.copyFile(
    new File(baseDirectory.value + "/contracts/target/scala-2.12/contracts_api-jsdeps.js"),
    new File(baseDirectory.value + "/dist/universa-jsdeps.js")
  )
}
