package storage.migrations

trait Migration {
  def version: String
  def runMigration: Unit
}
