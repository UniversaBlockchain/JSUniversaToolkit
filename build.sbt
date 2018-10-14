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


lazy val crypto_cloud_api = project.in(file("crypto_cloud"))
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
    jsEnv in Test := new org.scalajs.jsenv.selenium.SeleniumJSEnv(
      org.openqa.selenium.remote.DesiredCapabilities.chrome()
    ),
    scalaJSUseMainModuleInitializer := true,
    jsDependencies += ProvidedJS / "universajs.js"
  )
