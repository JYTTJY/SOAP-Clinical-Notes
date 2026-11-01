# db_helpers.R - 本地数据库存储
library(DBI)
library(RSQLite)

init_db <- function(db_path = "clinical_notes.sqlite") {
  con <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(con))
  dbExecute(con, "
    CREATE TABLE IF NOT EXISTS notes_history (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      patient_id TEXT,
      visit_date TEXT,
      pmh TEXT,
      meds TEXT,
      scr REAL,
      full_note TEXT
    )
  ")
}