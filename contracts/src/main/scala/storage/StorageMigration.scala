package storage

import boss.Boss
import boss.jsany._
import org.scalajs.dom.window.localStorage
import storage.migrations.Migration
import tools.universa.UniversaTools.{decode64, encode64}

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

@JSExportTopLevel("LocalStorage.migrations")
object StorageMigration {

    //add new storage.migrations here
    private val allMigrations: List[Migration] = List()

    private val versionKey = "Universa-version"

    private def encode(version: String): String = encode64(Boss.dump(version))
    private def decode(encoded: String): String =
      Boss.load(decode64(encoded)).asInstanceOf[String]

    @JSExport("run")
    def runNeededMigrations: Int = {
      runNeededMigrations(allMigrations)
    }

    //for tests only
    private[storage] def runNeededMigrations(migrations: List[Migration]): Int = {
      if (migrations.isEmpty) return 0

      val latestVersion = migrations.map(_.version).max
      def getCurrentVersion: String = {
        Option(localStorage.getItem(versionKey))
          .map(encoded => decode(encoded))
          .getOrElse("000")
      }

      def setCurrentVersion: Unit = {
        localStorage.setItem(versionKey, encode(latestVersion))
      }

      val currentVersion = getCurrentVersion
      if (currentVersion != latestVersion) {
        //1. find all with index > currentVersion
        val requiredMigrations = migrations.filter(_.version > currentVersion)
        //2. run required storage.migrations
        //what if some migration fail? let's stop the process
        requiredMigrations.foreach(_.runMigration)
        //3. set new universa currentVersion
        setCurrentVersion
        //return amount of storage.migrations that were run
        requiredMigrations.size
      } else 0
    }
  }