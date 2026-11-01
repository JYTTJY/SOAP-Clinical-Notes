# app.R - Musculoskeletal Physiotherapy Evaluation & SOAPER Matrix Documentation System (All-in-One)
library(shiny)
library(shinyjs)
library(jsonlite)
library(DBI)
library(RSQLite)

# Allow larger MRI / CT report uploads (Shiny's default limit is only 5MB)
options(shiny.maxRequestSize = 50 * 1024^2)

# ==================== 1. DATABASE & LOGIC SETUP ====================

# Permanent storage for uploaded files (not publicly served): patient_files/<IC>/<date>/file
UPLOAD_ROOT <- "patient_files"
if (!dir.exists(UPLOAD_ROOT)) dir.create(UPLOAD_ROOT, recursive = TRUE, showWarnings = FALSE)

# Temporary folder for previews: one random sub-folder per session, deleted when the session ends
VIEW_ROOT <- file.path(tempdir(), "doc_view")
if (!dir.exists(VIEW_ROOT)) dir.create(VIEW_ROOT, recursive = TRUE, showWarnings = FALSE)
addResourcePath("doc_view", VIEW_ROOT)

init_sqlite_db <- function(db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS patient_visits_v6 (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      patient_ic TEXT,
      patient_id TEXT,
      patient_name TEXT,
      visit_date TEXT,
      chief_complaints TEXT,
      form_data_json TEXT,
      soaper_note TEXT
    )
  ")
  
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS templates_v1 (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      type TEXT,
      title TEXT,
      content TEXT
    )
  ")
  
  # Older versions stored the line breaks of the default templates as a literal backslash + n; fix them once into real line breaks
  dbExecute(conn, "UPDATE templates_v1 SET content = REPLACE(content, '\\n', char(10)) WHERE content LIKE '%\\n%'")
  
  template_count <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM templates_v1")$count[1]
  if (template_count == 0) {
    defaults <- list(
      list(type = "treatment", title = "Lumbar Radiculopathy Standard Care",
           content = paste(
             "- Manual Therapy: L4-L5 Joint Mobilization Grade II-III (3 sets x 60s)",
             "- Electrotherapy: TENS on lumbar paraspinals 20 mins",
             "- Traction: Mechanical Lumbar Traction (1/3 BW, 15 mins)",
             "- Exercise: Lumbar extension in prone (3 sets x 10 reps)", sep = "\n")),
      list(type = "treatment", title = "Shoulder Impingement Protocol",
           content = paste(
             "- Manual Therapy: Glenohumeral posterior glide Grade II (3 sets x 60s)",
             "- Exercise Therapy: Rotator cuff isometric + Scapular setting (3 sets x 10 reps)",
             "- Modality: Ice pack 15 mins post-treatment", sep = "\n")),
      list(type = "hep", title = "Lumbar Spine Extension & Core Control",
           content = paste(
             "1. Prone Press-ups: 10 reps, hold 5s, 3 sets daily.",
             "2. Bird-Dog exercise: 10 reps/side, hold 3s.",
             "3. Avoid prolonged sitting (>30 mins) and forward bending.", sep = "\n")),
      list(type = "hep", title = "Shoulder Rotator Cuff & Scapular Rehab",
           content = paste(
             "1. Pendulum exercises: 2 mins clock-wise and counter-clockwise.",
             "2. Isometric External Rotation against wall: 10s hold x 10 reps, 2 sets.",
             "3. Scapular Retraction (Squeeze shoulder blades): 5s hold x 15 reps.", sep = "\n"))
    )
    for (t in defaults) {
      dbExecute(conn, "INSERT INTO templates_v1 (type, title, content) VALUES (?, ?, ?)",
                params = list(t$type, t$title, t$content))
    }
  }
}

init_sqlite_db()

# ---------- Patient IC standard format ----------
# 12 digits -> 880512-01-5431; passport number -> spaces removed and upper-cased
normalize_ic <- function(x) {
  x <- if (is.null(x) || length(x) == 0 || is.na(x[1])) "" else as.character(x[1])
  x <- toupper(gsub("\\s+", "", trimws(x)))
  digits <- gsub("-", "", x, fixed = TRUE)
  if (grepl("^[0-9]{12}$", digits)) {
    return(paste0(substr(digits, 1, 6), "-", substr(digits, 7, 8), "-", substr(digits, 9, 12)))
  }
  x
}

is_valid_ic <- function(x) grepl("^[A-Z0-9-]{5,20}$", x)

# Safe string for folder / file names
safe_name <- function(x) {
  x <- gsub("[^[:alnum:]._-]+", "_", as.character(x))
  x <- gsub("^_+|_+$", "", x)
  if (!nzchar(x)) "file" else x
}

# ---------- Visit records ----------
save_patient_visit <- function(patient_ic, patient_id, patient_name, chief_complaints, form_data, note, visit_date = Sys.Date(), db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  json_data <- as.character(jsonlite::toJSON(form_data, auto_unbox = TRUE, null = "null"))
  visit_date <- as.character(visit_date)   # visit date (can be a past date)
  
  # A NULL in any field makes the database write fail, so convert everything to empty strings
  nz <- function(x) if (is.null(x) || length(x) == 0 || is.na(x[1])) "" else as.character(x[1])
  patient_ic <- nz(patient_ic); patient_id <- nz(patient_id); patient_name <- nz(patient_name)
  chief_complaints <- nz(chief_complaints); note <- nz(note)
  
  # Same patient (IC) on the same day: update that day's record instead of adding a duplicate. If the IC is empty we cannot tell whether it is the same person, so always insert.
  existing <- data.frame()
  if (!is.null(patient_ic) && nchar(trimws(patient_ic)) > 0) {
    existing <- dbGetQuery(conn,
                           "SELECT id FROM patient_visits_v6 WHERE patient_ic = ? AND visit_date = ? ORDER BY id DESC LIMIT 1",
                           params = list(patient_ic, visit_date))
  }
  
  if (nrow(existing) > 0) {
    dbExecute(conn,
              "UPDATE patient_visits_v6 SET patient_id = ?, patient_name = ?, chief_complaints = ?, form_data_json = ?, soaper_note = ? WHERE id = ?",
              params = list(patient_id, patient_name, chief_complaints, json_data, note, existing$id[1]))
    return(invisible("updated"))
  }
  
  dbExecute(conn,
            "INSERT INTO patient_visits_v6 (patient_ic, patient_id, patient_name, visit_date, chief_complaints, form_data_json, soaper_note) VALUES (?, ?, ?, ?, ?, ?, ?)",
            params = list(patient_ic, patient_id, patient_name, visit_date, chief_complaints, json_data, note))
  invisible("inserted")
}

get_latest_patient_visit_by_ic <- function(patient_ic, db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  query <- "SELECT * FROM patient_visits_v6 WHERE patient_ic = ? ORDER BY visit_date DESC, id DESC LIMIT 1"
  dbGetQuery(conn, query, params = list(patient_ic))
}

# ---------- Templates ----------
get_templates_by_type <- function(template_type, db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  query <- "SELECT * FROM templates_v1 WHERE type = ? ORDER BY id DESC"
  dbGetQuery(conn, query, params = list(template_type))
}

get_template_by_id <- function(id, db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  dbGetQuery(conn, "SELECT * FROM templates_v1 WHERE id = ?", params = list(as.integer(id)))
}

# Does a template with the same title already exist in this category? (case-insensitive; exclude_id skips the template being edited)
template_title_exists <- function(type, title, exclude_id = 0, db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  res <- dbGetQuery(conn,
                    "SELECT COUNT(*) AS n FROM templates_v1 WHERE type = ? AND LOWER(title) = LOWER(?) AND id != ?",
                    params = list(type, title, as.integer(exclude_id)))
  res$n[1] > 0
}

add_template <- function(type, title, content, db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  query <- "INSERT INTO templates_v1 (type, title, content) VALUES (?, ?, ?)"
  dbExecute(conn, query, params = list(type, title, content))
}

update_template_by_id <- function(id, type, title, content, db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  query <- "UPDATE templates_v1 SET type = ?, title = ?, content = ? WHERE id = ?"
  dbExecute(conn, query, params = list(type, title, content, as.integer(id)))
}

delete_template_by_id <- function(id, db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  on.exit(dbDisconnect(conn), add = TRUE)
  query <- "DELETE FROM templates_v1 WHERE id = ?"
  dbExecute(conn, query, params = list(as.integer(id)))
}


# Safe output into the report: escape HTML special characters, turn line breaks into <br>, show a placeholder for empty values
esc_html_raw <- function(x, empty = "—") {
  if (is.null(x) || length(x) == 0) return(empty)
  x <- x[!is.na(x)]
  if (length(x) == 0) return(empty)
  x <- paste(x, collapse = ", ")
  if (nchar(trimws(x)) == 0) return(empty)
  x <- htmltools::htmlEscape(x)
  gsub("\r?\n", "<br>", x)
}

synthesize_full_evaluation_form <- function(doc, f) {
  # Trial mode: anything that was not filled in stays truly blank (no dash, no "Not Recorded"),
  # so the printed page can be filled in by hand.
  is_trial <- isTRUE(doc$trial)
  esc_html <- function(x, empty = "—") esc_html_raw(x, if (is_trial) "" else empty)
  
  rom_html <- ""
  fmt_deg <- function(x) {
    x <- trimws(as.character(x))
    if (length(x) == 0 || is.na(x) || !nzchar(x) || x == "-") (if (is_trial) "" else "—") else paste0(htmltools::htmlEscape(x), "°")
  }
  if (!is.null(f$rom_df) && nrow(f$rom_df) > 0) {
    rom_rows <- apply(f$rom_df, 1, function(row) {
      sprintf("<tr><td style='padding: 6px 10px; border: 1px solid #cbd5e1; font-weight: 600;'>%s</td><td style='padding: 6px 10px; border: 1px solid #cbd5e1; text-align: center;'>%s</td><td style='padding: 6px 10px; border: 1px solid #cbd5e1; text-align: center;'>%s</td><td style='padding: 6px 10px; border: 1px solid #cbd5e1; color: #475569;'>%s</td></tr>", 
              row["Movement"], fmt_deg(row["AROM"]), fmt_deg(row["PROM"]), esc_html(row["Remarks"], ""))
    })
    if ("Region" %in% names(f$rom_df) && length(unique(f$rom_df$Region)) > 1) {
      parts <- character(0)
      for (rg in unique(f$rom_df$Region)) {
        parts <- c(parts,
                   sprintf("<tr style='page-break-after: avoid; break-after: avoid;'><td colspan='4' style='padding: 5px 10px; background: #fef3c7; color: #92400e; font-weight: 700; border: 1px solid #cbd5e1;'>📍 %s</td></tr>", htmltools::htmlEscape(rg)),
                   rom_rows[f$rom_df$Region == rg])
      }
      rom_html <- paste(parts, collapse = "")
    } else {
      rom_html <- paste(rom_rows, collapse = "")
    }
  } else {
    rom_html <- "<tr><td colspan='4' style='padding: 8px; text-align: center; color: #94a3b8;'>No ROM data recorded.</td></tr>"
  }
  
  pre_vas <- if(!is.null(f$sym_vas)) as.numeric(f$sym_vas) else 0
  post_vas <- if(!is.null(f$vas_post)) as.numeric(f$vas_post) else 0
  
  pt_impression_text <- esc_html(f$pt_impression, "Not Recorded")
  treatment_plan_text <- esc_html(f$treatment_plan, "Not Specified")
  hep_text <- esc_html(f$hep, "None")
  next_sched_text <- esc_html(f$next_schedule, "As needed")
  next_plan_text <- esc_html(f$next_plan, "Review progress next session.")
  s_hpi_text <- esc_html(f$s_hpi, "Not Recorded")
  
  # When MMT / special tests have no data (e.g. no region selected yet), show a placeholder row instead of an empty table
  empty_row <- "<tr><td colspan='2' style='padding: 8px; text-align: center; color: #94a3b8; border: 1px solid #cbd5e1;'>No data recorded.</td></tr>"
  if (is.null(f$mmt_table_html) || !nzchar(f$mmt_table_html)) f$mmt_table_html <- empty_row
  if (is.null(f$special_tests_table_html) || !nzchar(f$special_tests_table_html)) f$special_tests_table_html <- empty_row
  
  # Visit date (Date of Assessment on the report)
  visit_date_txt <- tryCatch(as.character(as.Date(doc$visit_date)), error = function(e) as.character(Sys.Date()))
  if (length(visit_date_txt) == 0 || is.na(visit_date_txt)) visit_date_txt <- as.character(Sys.Date())
  
  # Patient name for the file name / page footer (computed before escaping; characters unsuitable for file names are removed)
  raw_name <- if (is.null(doc$patient_name) || length(doc$patient_name) == 0 || is.na(doc$patient_name[1])) "" else as.character(doc$patient_name[1])
  safe_name <- raw_name
  for (ch in c("/", "\\", ":", "*", "?", "\"", "<", ">", "|", "&", "'", ";", "{", "}")) safe_name <- gsub(ch, "", safe_name, fixed = TRUE)
  footer_name <- trimws(gsub("\\s+", " ", safe_name))
  file_stem <- if (nchar(footer_name) > 0) paste0(gsub(" ", "_", footer_name), "_", visit_date_txt) else paste0("Evaluation_Report_", visit_date_txt)
  
  
  # Escape all other user input (except table fields that are already HTML)
  html_keys <- c("rom_df", "mmt_table_html", "special_tests_table_html")
  for (nm in setdiff(names(f), html_keys)) f[[nm]] <- esc_html(f[[nm]])
  for (nm in names(doc)) doc[[nm]] <- esc_html(doc[[nm]])
  
  # ---------- Print layout helpers: every block is a card that must not be split across pages ----------
  v <- function(x) if (is.null(x) || length(x) == 0 || all(is.na(x))) "" else paste(x, collapse = ", ")
  
  card_open <- "<div class='avoid-break' style='border: 1.5px solid #cbd5e1; border-radius: 8px; margin-bottom: 14px; overflow: hidden; background: #ffffff; page-break-inside: avoid; break-inside: avoid;'>"
  
  sec_bar <- function(title, color, bg) {
    paste0("<div class='sec-title' style='font-size: 13px; font-weight: 700; color: ", color, "; background: ", bg, "; padding: 8px 12px; border-bottom: 1px solid #cbd5e1; page-break-after: avoid; break-after: avoid;'>", title, "</div>")
  }
  
  sub_bar <- function(title) {
    paste0("<div class='sec-title' style='font-size: 12.5px; font-weight: 700; color: #0f172a; background: #f1f5f9; padding: 7px 12px; border-bottom: 1px solid #cbd5e1; page-break-after: avoid; break-after: avoid;'>", title, "</div>")
  }
  
  th_l <- "padding: 6px 10px; border: 1px solid #cbd5e1; text-align: left;"
  th_c <- "padding: 6px 10px; border: 1px solid #cbd5e1; text-align: center;"
  td_l <- "padding: 6px 10px; border: 1px solid #cbd5e1; font-weight: bold;"
  td_c <- "padding: 6px 10px; border: 1px solid #cbd5e1; text-align: center;"
  
  # A data-table card with a title (never split across pages; if it does not fit, the whole card moves to the next page)
  data_block <- function(title, col1, col2, rows_html, col2_width = "35%") {
    paste0(
      card_open, sub_bar(title),
      "<div style='padding: 10px 12px;'>",
      "<table style='width: 100%; border-collapse: collapse; background: #fff;'>",
      "<tr style='background: #f1f5f9; font-size: 11px; font-weight: bold; color: #334155; page-break-inside: avoid; break-inside: avoid;'>",
      "<th style='", th_l, "'>", col1, "</th><th style='", th_c, " width: ", col2_width, ";'>", col2, "</th></tr>",
      rows_html,
      "</table></div></div>"
    )
  }
  
  kv_rows <- function(pairs) {
    paste0(vapply(pairs, function(p) {
      paste0("<tr style='page-break-inside: avoid; break-inside: avoid;'><td style='", td_l, "'>", p[1], "</td><td style='", td_c, "'>", v(p[2]), "</td></tr>")
    }, character(1)), collapse = "")
  }
  
  balance_card <- data_block("Balance Assessment", "Component", "Performance Level",
                             kv_rows(list(c("Sitting", f$bal_sitting), c("Standing", f$bal_standing),
                                          c("Single Leg", f$bal_sls), c("Tandem", f$bal_tandem))), "45%")
  functional_card <- data_block("Functional Assessment", "Component", "Performance Level",
                                kv_rows(list(c("Bed Mobility", f$func_bed), c("Transfers", f$func_transfer),
                                             c("Sit-to-Stand", f$func_sts), c("Gait", f$func_gait))), "45%")
  # Remove the card's own bottom margin; the outer wrapper controls spacing
  balance_card <- sub("margin-bottom: 14px;", "margin-bottom: 0;", balance_card, fixed = TRUE)
  functional_card <- sub("margin-bottom: 14px;", "margin-bottom: 0;", functional_card, fixed = TRUE)
  
  balance_functional_row <- paste0(
    "<table class='avoid-break' style='width: 100%; border-collapse: separate; border-spacing: 0; margin-bottom: 14px; page-break-inside: avoid; break-inside: avoid;'><tr>",
    "<td style='width: 50%; vertical-align: top; padding: 0 7px 0 0;'>", balance_card, "</td>",
    "<td style='width: 50%; vertical-align: top; padding: 0 0 0 7px;'>", functional_card, "</td>",
    "</tr></table>"
  )
  
  paste0(
    "<div class='report-root' data-filename='", file_stem, "' data-patient='", footer_name, "' style='font-family: -apple-system, BlinkMacSystemFont, \"Segoe UI\", Roboto, sans-serif; font-size: 13px; color: #1e293b; background: #ffffff; padding: 10px; line-height: 1.5;'>
      
      <!-- Report header -->
      <div style='text-align: center; margin-bottom: 16px; page-break-inside: avoid; break-inside: avoid;'>
        <div style='font-size: 18px; font-weight: 800; color: #0f172a; letter-spacing: 0.8px; text-transform: uppercase;'>GREEN CARE PHYSIOTHERAPY EVALUATION REPORT</div>
        <div style='font-size: 11px; color: #64748b; margin-top: 2px;'>Comprehensive Clinical Documentation System</div>
      </div>

      <!-- Patient information -->
      <div class='avoid-break' style='border: 2px solid #0f172a; border-radius: 8px; padding: 12px; margin-bottom: 14px; background: #f8fafc; page-break-inside: avoid; break-inside: avoid;'>
        <div style='font-weight: 700; color: #0f172a; font-size: 13px; margin-bottom: 8px; border-bottom: 1px solid #cbd5e1; padding-bottom: 4px;'>PATIENT INFORMATION & DIAGNOSIS</div>
        <table style='width: 100%; border-collapse: collapse;'>
          <tr>
            <td colspan='2' style='padding: 3px 0;'><strong>Patient Name:</strong> ", doc$patient_name, "</td>
          </tr>
          <tr>
            <td style='padding: 3px 0; width: 50%;'><strong>Patient ID:</strong> ", doc$patient_id, "</td>
            <td style='padding: 3px 0; width: 50%;'><strong>Patient IC:</strong> ", doc$patient_ic, "</td>
          </tr>
          <tr>
            <td colspan='2' style='padding: 3px 0;'><strong>Date of Assessment:</strong> ", visit_date_txt, "</td>
          </tr>
          <tr>
            <td colspan='2' style='padding: 3px 0;'><strong>Primary Diagnosis:</strong> <span style='color: #0284c7; font-weight: 600;'>", doc$primary_dx, "</span></td>
          </tr>
          <tr>
            <td colspan='2' style='padding: 3px 0;'><strong>Diagnostic Imaging / MRI:</strong> ", doc$radiology_findings, "</td>
          </tr>
        </table>
      </div>

      <!-- [I] SUBJECTIVE HISTORY -->
      ", card_open, sec_bar("[I] SUBJECTIVE HISTORY (S)", "#0369a1", "#e0f2fe"), "
        <div style='padding: 12px;'>
          <table style='width: 100%; border-collapse: collapse;'>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; width: 25%; vertical-align: top;'>Chief Complaints</td>
              <td style='padding: 6px 0; vertical-align: top;'>", f$chief_complaints, "</td>
            </tr>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; vertical-align: top; border-top: 1px solid #f1f5f9;'>Past Medical Hx</td>
              <td style='padding: 6px 0; vertical-align: top; border-top: 1px solid #f1f5f9;'>", f$past_med, "</td>
            </tr>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; vertical-align: top; border-top: 1px solid #f1f5f9;'>Personal Hx</td>
              <td style='padding: 6px 0; vertical-align: top; border-top: 1px solid #f1f5f9;'>", f$personal_hx, "</td>
            </tr>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; vertical-align: top; border-top: 1px solid #f1f5f9;'>Family Hx</td>
              <td style='padding: 6px 0; vertical-align: top; border-top: 1px solid #f1f5f9;'>", f$family_hx, "</td>
            </tr>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; vertical-align: top; border-top: 1px solid #f1f5f9;'>History of Present Illness</td>
              <td style='padding: 6px 0; vertical-align: top; border-top: 1px solid #f1f5f9;'>", s_hpi_text, "</td>
            </tr>
            <tr>
              <td style='padding: 8px 0; font-weight: bold; vertical-align: top; border-top: 1px solid #cbd5e1;'>Symptoms Profile</td>
              <td style='padding: 8px 0; vertical-align: top; border-top: 1px solid #cbd5e1;'>
                <table style='width: 100%; border-collapse: collapse; background: #f8fafc; border: 1px solid #e2e8f0; font-size: 12px;'>
                  <tr>
                    <td style='padding: 5px 8px; width: 18%; font-weight: 700; color: #334155;'>Side / Site:</td>
                    <td style='padding: 5px 8px; width: 32%; color: #0f172a;'>", f$sym_side, " / ", f$sym_site, "</td>
                    <td style='padding: 5px 8px; width: 20%; font-weight: 700; color: #334155;'>Onset & Duration:</td>
                    <td style='padding: 5px 8px; width: 30%; color: #0f172a;'>", f$sym_onset, " (", f$sym_duration, ")</td>
                  </tr>
                  <tr style='border-top: 1px solid #f1f5f9;'>
                    <td style='padding: 5px 8px; font-weight: 700; color: #334155;'>Symptom Type:</td>
                    <td style='padding: 5px 8px; color: #0f172a;'>", f$sym_type, "</td>
                    <td style='padding: 5px 8px; font-weight: 700; color: #334155;'>Baseline VAS:</td>
                    <td style='padding: 5px 8px; color: #dc2626; font-weight: bold;'>", pre_vas, " / 10</td>
                  </tr>
                  <tr style='border-top: 1px solid #f1f5f9;'>
                    <td style='padding: 5px 8px; font-weight: 700; color: #334155;'>Aggravating:</td>
                    <td style='padding: 5px 8px; color: #0f172a;'>", f$sym_agg, "</td>
                    <td style='padding: 5px 8px; font-weight: 700; color: #334155;'>Relieving:</td>
                    <td style='padding: 5px 8px; color: #0f172a;'>", f$sym_rel, "</td>
                  </tr>
                  <tr style='border-top: 1px solid #f1f5f9;'>
                    <td style='padding: 5px 8px; font-weight: 700; color: #334155;'>24h Behavior:</td>
                    <td style='padding: 5px 8px; color: #0f172a;'>", f$sym_24h, "</td>
                    <td style='padding: 5px 8px; font-weight: 700; color: #334155;'>Prev. Treatments:</td>
                    <td style='padding: 5px 8px; color: #0f172a;'>", f$sym_prev_tx, "</td>
                  </tr>
                  <tr style='border-top: 1.5px dashed #cbd5e1;'>
                    <td style='padding: 6px 8px; font-weight: 700; color: #334155;'>Functional Impact:</td>
                    <td colspan='3' style='padding: 6px 8px; color: #0f172a;'>", f$sym_func_impact, "</td>
                  </tr>
                </table>
              </td>
            </tr>
          </table>
        </div>
      </div>

      <!-- [II] OBJECTIVE EXAMINATION: the title + Observation form one card; every table after that is its own card and moves to the next page as a whole if it does not fit -->
      ", card_open, sec_bar("[II] OBJECTIVE EXAMINATION (O)", "#92400e", "#fef3c7"), "
        <div style='padding: 12px;'>
          <table style='width: 100%; border-collapse: collapse;'>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; width: 25%; vertical-align: top;'>Observation</td>
              <td style='padding: 6px 0; vertical-align: top;'>General: ", f$obs_general, "<br>Local: ", f$obs_local, "</td>
            </tr>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; vertical-align: top; border-top: 1px solid #f1f5f9;'>Palpation Findings</td>
              <td style='padding: 6px 0; vertical-align: top; border-top: 1px solid #f1f5f9;'>", f$obs_palpation, "</td>
            </tr>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; vertical-align: top; border-top: 1px solid #f1f5f9;'>Target Region</td>
              <td style='padding: 6px 0; vertical-align: top; border-top: 1px solid #f1f5f9; color: #b45309; font-weight: 700;'>", f$joint, "</td>
            </tr>
          </table>
        </div>
      </div>
      
      ", card_open, sub_bar("Range of Motion (ROM)"), "
        <div style='padding: 10px 12px;'>
          <table style='width: 100%; border-collapse: collapse; background: #fff;'>
            <tr style='background: #f1f5f9; font-size: 11px; font-weight: bold; color: #334155;'>
              <th style='", th_l, "'>Direction</th>
              <th style='", th_c, " width: 22%;'>AROM</th>
              <th style='", th_c, " width: 22%;'>PROM</th>
              <th style='", th_l, " width: 35%;'>Remarks</th>
            </tr>
            ", rom_html, "
          </table>
        </div>
      </div>
      
      ", data_block("Muscle Strength Testing (MMT)", "Muscle Group / Myotome", "Grade (0-5)", f$mmt_table_html), "
      
      ", data_block("Special Clinical Tests", "Special Test Name", "Result", f$special_tests_table_html), "
      
      ", balance_functional_row, "
      
      ", card_open, sub_bar("Joint Stability & Measurements"), "
        <div style='padding: 10px 12px;'>
          <table style='width: 100%; border-collapse: collapse;'>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; width: 25%; vertical-align: top;'>Joint Stability</td>
              <td style='padding: 6px 0; vertical-align: top;'>", f$joint_integrity, "</td>
            </tr>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; vertical-align: top; border-top: 1px solid #f1f5f9;'>Special Measurements</td>
              <td style='padding: 6px 0; vertical-align: top; border-top: 1px solid #f1f5f9;'>", f$measurement, "</td>
            </tr>
          </table>
        </div>
      </div>

      <!-- [III] CLINICAL ASSESSMENT -->
      ", card_open, sec_bar("[III] CLINICAL ASSESSMENT (A)", "#991b1b", "#fee2e2"), "
        <div style='padding: 12px;'>
          <table style='width: 100%; border-collapse: collapse;'>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; width: 25%; vertical-align: top;'>PT Impression</td>
              <td style='padding: 6px 0; vertical-align: top;'>", pt_impression_text, "</td>
            </tr>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; vertical-align: top; border-top: 1px solid #f1f5f9;'>Status & Safety</td>
              <td style='padding: 6px 0; vertical-align: top; border-top: 1px solid #f1f5f9;'>
                Status: ", f$impression, "<br>
                Red Flags: <span style='color: #dc2626;'>", f$flags, "</span><br>
                Precautions: ", f$precautions, "
              </td>
            </tr>
          </table>
        </div>
      </div>

      <!-- [IV] PLAN OF CARE -->
      ", card_open, sec_bar("[IV] PLAN OF CARE & INTERVENTIONS (P)", "#166534", "#dcfce7"), "
        <div style='padding: 12px; color: #1e293b;'>", treatment_plan_text, "</div>
      </div>

      <!-- [V] POST-TREATMENT & HOME PLAN + signature block: kept on the same page -->
      <div class='avoid-break' style='page-break-inside: avoid; break-inside: avoid;'>
      ", card_open, sec_bar("[V] POST-TREATMENT & HOME PLAN (E & R)", "#3730a3", "#e0e7ff"), "
        <div style='padding: 12px;'>
          <table style='width: 100%; border-collapse: collapse;'>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; width: 25%; vertical-align: top;'>Post-Tx VAS Pain</td>
              <td style='padding: 6px 0; vertical-align: top;'><strong>", post_vas, " / 10</strong></td>
            </tr>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; vertical-align: top; border-top: 1px solid #f1f5f9;'>Home Exercise (HEP)</td>
              <td style='padding: 6px 0; vertical-align: top; border-top: 1px solid #f1f5f9;'>", hep_text, "</td>
            </tr>
            <tr>
              <td style='padding: 6px 0; font-weight: bold; vertical-align: top; border-top: 1px solid #f1f5f9;'>Follow-Up & Plan</td>
              <td style='padding: 6px 0; vertical-align: top; border-top: 1px solid #f1f5f9;'>
                Schedule: ", next_sched_text, "<br>
                Next Session Focus: ", next_plan_text, "
              </td>
            </tr>
          </table>
        </div>
      </div>
      
      <table style='width: 100%; border-collapse: separate; border-spacing: 0; margin-top: 10px; page-break-inside: avoid; break-inside: avoid;'>
        <tr>
          <td style='width: 38%; padding: 0 14px 0 0; vertical-align: bottom;'>
            <div style='height: 44px; '>&nbsp;</div>
            <div style='border-top: 1.5px solid #0f172a; padding-top: 4px; font-size: 11px; color: #475569; text-align: center;'>Physiotherapist Name</div>
          </td>
          <td style='width: 36%; padding: 0 14px 0 0; vertical-align: bottom;'>
            <div style='height: 44px;'>&nbsp;</div>
            <div style='border-top: 1.5px solid #0f172a; padding-top: 4px; font-size: 11px; color: #475569; text-align: center;'>Signature</div>
          </td>
          <td style='width: 26%; padding: 0; vertical-align: bottom;'>
            <div style='height: 44px; '>&nbsp;</div>
            <div style='border-top: 1.5px solid #0f172a; padding-top: 4px; font-size: 11px; color: #475569; text-align: center;'>Date</div>
          </td>
        </tr>
      </table>
      
      <div style='text-align: right; margin-top: 14px; font-size: 11px; color: #94a3b8; font-style: italic; padding-top: 6px; page-break-inside: avoid; break-inside: avoid;'>Authorized Clinical Documentation • Green Care Physiotherapy</div>
      </div>
    </div>"
  )
}

# ==================== 2. MAPPINGS ====================

get_movements_for_joint <- function(joint) {
  switch(joint,
         "Lumbar Spine" = c("Flexion", "Extension", "L Side Bending", "R Side Bending", "L Rotation", "R Rotation"),
         "Cervical Spine" = c("Flexion", "Extension", "L Side Bending", "R Side Bending", "L Rotation", "R Rotation"),
         "Thoracic Spine / Scoliosis" = c("Flexion", "Extension", "L Side Bending", "R Side Bending", "L Rotation", "R Rotation"),
         "Shoulder Joint" = c("Flexion", "Extension", "Abduction", "Internal Rot", "External Rot"),
         "Knee Joint" = c("Flexion", "Extension"),
         "Hip Joint" = c("Flexion", "Extension", "Abduction", "Adduction", "Internal Rot", "External Rot"),
         "Ankle Joint" = c("Dorsiflexion", "Plantarflexion", "Inversion", "Eversion"),
         c("Flexion", "Extension", "Rotation")
  )
}

get_special_tests_for_joint <- function(joint) {
  switch(joint,
         "Shoulder Joint" = c("Hawkins-Kennedy Test", "Neer Impingement Test", "Empty Can (Jobe) Test", "Drop Arm Test", "Apprehension Test"),
         "Lumbar Spine" = c("Straight Leg Raise (SLR) Test", "Crossed SLR Test", "Slump Test", "Adam's Forward Bend Test", "FABER / Patrick's Test"),
         "Cervical Spine" = c("Spurling's Test", "Cervical Distraction Test", "Upper Limb Tension Test (ULTT)", "Jackson's Compression Test"),
         "Knee Joint" = c("Lachman Test", "Anterior Drawer Test", "McMurray Test", "Patellar Apprehension Test", "Valgus/Varus Stress Test"),
         "Hip Joint" = c("FADIR Test", "FABER Test", "Thomas Test", "Trendelenburg Test"),
         "Ankle Joint" = c("Anterior Drawer Test (Ankle)", "Talar Tilt Test", "Thompson Test (Squeeze Test)", "Kleiger's Test (External Rotation)"),
         "Thoracic Spine / Scoliosis" = c("Adam's Forward Bend Test", "Scapular Assistance Test", "Rib Compression Test"),
         c("Special Test 1", "Special Test 2")
  )
}

get_mmt_groups_for_joint <- function(joint) {
  switch(joint,
         "Shoulder Joint" = c("Shoulder Flexors", "Shoulder Extensors", "Shoulder Abductors", "Shoulder Adductors", "Internal Rotators", "External Rotators"),
         "Lumbar Spine" = c("Trunk Flexors (Abs)", "Trunk Extensors", "L4 Myotome (Dorsiflexion)", "L5 Myotome (EHL)", "S1 Myotome (Plantarflexion)"),
         "Cervical Spine" = c("Neck Flexors", "Neck Extensors", "C5 Myotome (Abduction)", "C6 Myotome (Elbow Flexion)", "C7 Myotome (Elbow Ext)"),
         "Knee Joint" = c("Knee Extensors (Quads)", "Knee Flexors (Hamstrings)"),
         "Hip Joint" = c("Hip Flexors", "Hip Extensors", "Hip Abductors", "Hip Adductors", "Hip Int Rotators", "Hip Ext Rotators"),
         "Ankle Joint" = c("Ankle Dorsiflexors", "Ankle Plantarflexors", "Ankle Inverters", "Ankle Everters"),
         c("Group 1 Flexors", "Group 2 Extensors")
  )
}

# ---- Form constants: dropdown choices, blank defaults, demo data ----

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

joint_slug <- function(j) {
  j <- if (is.null(j) || length(j) == 0 || is.na(j[1])) "" else as.character(j[1])
  if (!nzchar(j)) return("none")
  gsub("^_+|_+$", "", gsub("[^a-z0-9]+", "_", tolower(j)))
}

# Storage keys for ROM / MMT / special tests, e.g. rom_arom|lumbar_spine|1
store_key <- function(kind, joint, i) paste(kind, joint_slug(joint), i, sep = "|")

SELECT_CHOICES <- list(
  s_past_med      = c("None", "Hypertension", "Diabetes", "Heart Disease", "Osteoporosis"),
  s_personal_hx   = c("None", "Smoker", "Social Drinker", "Sedentary / Desk Job", "Regular Exercise"),
  s_family_hx     = c("None", "Stroke", "Cancer", "Heart Disease", "Diabetes"),
  sym_side        = c("Left", "Right", "Bilateral", "Central"),
  sym_site        = c("Lumbar Spine", "Cervical Spine", "Shoulder", "Knee", "Hip", "Ankle"),
  sym_onset       = c("Insidious", "Acute", "Subacute", "Traumatic"),
  sym_duration    = c("< 2 weeks", "3 weeks", "1 month", "3 months", "> 6 months"),
  sym_type        = c("Aching & Numbness", "Sharp / Stabbing", "Dull Pain", "Burning", "Throbbing"),
  sym_agg         = c("Forward Bending", "Prolonged Sitting", "Prolonged Standing", "Overhead Movement", "Weight Bearing", "Other"),
  sym_rel         = c("Rest / Lying Flat", "Extension Stretch", "Heat / Ice Pack", "Medication", "Other"),
  sym_24h         = c("Worse at end of day", "Worse in morning / stiffness", "Eases with movement", "Constant / No pattern", "Night pain disturbs sleep", "Other"),
  sym_prev_tx     = c("None", "NSAIDs / Painkillers", "Physiotherapy", "TCM / Acupuncture", "Chiropractic Care", "Steroid Injection", "Other"),
  r_next_schedule = c("1x per week", "2x per week", "3x per week", "As needed (PRN)")
)

# Dropdowns that start empty and only show placeholder text (pick a value or type your own)
blank_selectize <- function(id, label, width = "100%") {
  selectizeInput(id, label, choices = SELECT_CHOICES[[id]], selected = "", width = width,
                 options = list(create = TRUE, placeholder = "Select or type...",
                                onInitialize = I('function() { this.setValue(""); }')))
}

# Ids of each kind of input (used to clear / load the whole form at once)
FORM_TEXT_IDS   <- c("ext_name", "ext_ic", "ext_id_num", "sym_func_impact", "meas_assess", "a_precautions")
FORM_AREA_IDS   <- c("ext_dx", "ext_findings", "s_chief_complaints", "s_hpi", "obs_general", "obs_local", "obs_palpation",
                     "a_pt_impression", "p_treatment_plan", "r_hep", "r_next_plan")
FORM_SLIDER_IDS <- c("sym_vas", "e_vas_post")
FORM_SELECT_IDS <- c("o_joint_select", "joint_integrity", "a_impression",
                     "bal_sitting", "bal_standing", "bal_sls", "bal_tandem",
                     "func_bed", "func_transfer", "func_sts", "func_gait")

BLANK_VALUES <- c(
  setNames(rep(list(""), length(FORM_TEXT_IDS)), FORM_TEXT_IDS),
  setNames(rep(list(""), length(FORM_AREA_IDS)), FORM_AREA_IDS),
  setNames(rep(list(0), length(FORM_SLIDER_IDS)), FORM_SLIDER_IDS),
  setNames(rep(list(""), length(names(SELECT_CHOICES))), names(SELECT_CHOICES))
)
for (id in FORM_SELECT_IDS) BLANK_VALUES[[id]] <- if (grepl("^(bal|func)_", id)) "N/A" else ""
BLANK_VALUES[["o_joint_select"]] <- character(0)   # Multi-select: no region is selected by default

# Demo data: only loaded when 'Demo Data' is clicked (for training / testing)
DEMO_VALUES <- list(
  ext_name = "Tan Ah Kow", ext_ic = "880512-01-5431", ext_id_num = "2026",
  ext_dx = "L4-L5 Lumbar Disc Herniation with L5 Radiculopathy",
  ext_findings = "[MRI] L4/L5 left posterior protrusion compressing L5 root.\n[X-Ray] Reduced L4-L5 disc space height.",
  s_chief_complaints = "Lower back pain radiating down to left leg.",
  s_past_med = "Hypertension", s_personal_hx = "None", s_family_hx = "None",
  s_hpi = "Patient reported gradual onset of LBP 3 weeks ago after heavy lifting. Pain worsened 3 days ago with left leg numbness down to L5 dermatome.",
  sym_side = "Left", sym_site = "Lumbar Spine", sym_onset = "Insidious", sym_duration = "3 weeks", sym_type = "Aching & Numbness",
  sym_vas = 6, sym_agg = "Prolonged Sitting", sym_rel = "Rest / Lying Flat", sym_24h = "Worse at end of day", sym_prev_tx = "None",
  sym_func_impact = "Unable to sit >20 mins; Difficulty with prolonged driving.",
  o_joint_select = "Lumbar Spine",
  obs_general = "Antalgic posture, slight right trunk shift.",
  obs_local = "Mild spasm in left lumbar paraspinal muscles.",
  obs_palpation = "Tenderness at L4/L5 spinous process.",
  bal_sitting = "Good", bal_standing = "Fair", bal_sls = "Poor", bal_tandem = "Fair",
  func_bed = "Good", func_transfer = "Fair", func_sts = "Fair", func_gait = "Good",
  joint_integrity = "Normal Joint Play & Stability", meas_assess = "Girth: Equal Bilaterally",
  a_pt_impression = "L4-L5 Lumbar radiculopathy secondary to disc herniation, presenting with L5 sensory deficit and core muscle imbalance.",
  a_impression = "Progressing as expected", a_precautions = "Avoid overhead loading at end-range.",
  p_treatment_plan = "- Manual Therapy: Joint Mobilization Grade II Glenohumeral (3 sets x 60s)\n- Exercise Therapy: Rotator Cuff Isometric + Scapular Setting (3 sets x 10 reps)",
  e_vas_post = 3,
  r_hep = "Pendulum exercises + Isometric rotator cuff 10s x 10 reps.",
  r_next_plan = "Progress to isotonic strengthening; re-check shoulder active abduction ROM and pain level.",
  r_next_schedule = "1x per week"
)


# Is a form blank? (used by draft autosave: a blank form is neither saved nor offered for restore)
form_is_blank <- function(fd) {
  norm <- function(x) {
    x <- as.character(unlist(x))
    x <- x[!is.na(x)]
    x[nzchar(trimws(x))]
  }
  for (nm in names(BLANK_VALUES)) {
    if (!identical(norm(fd[[nm]]), norm(BLANK_VALUES[[nm]]))) return(FALSE)
  }
  if (length(norm(fd$a_flags)) > 0) return(FALSE)
  ts <- fd$table_store
  if (is.list(ts)) {
    for (k in names(ts)) {
      kind <- sub("\\|.*$", "", k)
      default <- switch(kind, mmt = "N/T", st = "ND", "")
      v <- as.character(unlist(ts[[k]]))
      if (length(v) > 0 && !is.na(v[1]) && trimws(v[1]) != default) return(FALSE)
    }
  }
  TRUE
}

# ==================== 3. UI LAYOUT ====================

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      body { background-color: #f8fafc; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }
      .custom-card { background: #ffffff; border: 1px solid #e2e8f0; border-radius: 12px; padding: 24px; margin-bottom: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.03); }
      
      .soap-report-view { 
        background: #ffffff !important; 
        color: #0f172a !important; 
        border: 1px solid #cbd5e1 !important; 
        border-radius: 8px !important; 
        padding: 20px !important; 
        overflow: visible !important;
      }
      
      .table-rom .form-group, .table-mmt .form-group, table .form-group { margin-bottom: 0 !important; }
      .table-rom td, .table-mmt td, table td { vertical-align: middle !important; padding: 4px 8px !important; }
      
      .st-radio-group { display: flex !important; justify-content: center !important; align-items: center !important; width: 100% !important; }
      .st-radio-group .shiny-options-group { display: flex !important; flex-direction: row !important; flex-wrap: nowrap !important; gap: 16px !important; align-items: center !important; justify-content: center !important; margin: 0 !important; white-space: nowrap !important; }
      .st-radio-group .radio-inline { margin: 0 !important; padding-left: 0 !important; display: inline-flex !important; align-items: center !important; font-size: 11px !important; white-space: nowrap !important; cursor: pointer !important; }
      .st-radio-group .radio-inline input[type='radio'] { position: relative !important; margin-top: 0 !important; margin-left: 0 !important; margin-right: 5px !important; cursor: pointer !important; }

      .soaper-card { border-radius: 8px; padding: 16px; margin-bottom: 18px; border: 1px solid #cbd5e1; background-color: #ffffff; }
      .soaper-card-s { border-left: 5px solid #0284c7; background-color: #f0f9ff; }
      .soaper-card-o { border-left: 5px solid #d97706; background-color: #fffbeb; }
      .soaper-card-a { border-left: 5px solid #dc2626; background-color: #fef2f2; }
      .soaper-card-p { border-left: 5px solid #16a34a; background-color: #f0fdf4; }
      .soaper-card-e { border-left: 5px solid #4f46e5; background-color: #eef2ff; }
      .soaper-card-r { border-left: 5px solid #9333ea; background-color: #faf5ff; }

      .section-badge { font-size: 12px; padding: 4px 10px; border-radius: 4px; font-weight: bold; display: inline-block; margin-bottom: 12px; }
      .bg-s { background-color: #e0f2fe; color: #0369a1; }
      .bg-o { background-color: #fef3c7; color: #92400e; }
      .bg-a { background-color: #fee2e2; color: #991b1b; }
      .bg-p { background-color: #dcfce7; color: #166534; }
      .bg-e { background-color: #e0e7ff; color: #3730a3; }
      .bg-r { background-color: #f3e8ff; color: #6b21a8; }
      
      .flag-box { background-color: #fff5f5; border: 1px solid #feb2b2; padding: 12px; border-radius: 6px; margin: 10px 0; }
      .table-rom input, .table-mmt input, .table-mmt select, table select { border: 1px solid #cbd5e1; border-radius: 4px; padding: 2px 5px; width: 100%; }
      .header-bar { background: #198754; color: white; padding: 14px 24px; border-radius: 10px; margin-bottom: 20px; display: flex; justify-content: space-between; align-items: center; }
      .form-group { margin-bottom: 10px !important; }
      .form-control { border-radius: 8px; border: 1px solid #cbd5e1; font-size: 12px; padding: 4px 8px; }
      .sub-box { background: #ffffff; border: 1px solid #e2e8f0; border-radius: 8px; padding: 12px; margin-bottom: 12px; }
      .sub-box pre { white-space: pre-wrap !important; word-break: break-word !important; font-family: 'Consolas', 'Courier New', monospace; font-size: 13px !important; line-height: 1.5; }
      
      .input-wrapper-inner { position: relative !important; width: 100%; }
      .tmpl-btn-bar { display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px; }
      
      .input-group-text-prefix {
        background-color: #e2e8f0;
        border: 1px solid #cbd5e1;
        border-right: none;
        border-top-left-radius: 8px;
        border-bottom-left-radius: 8px;
        font-weight: bold;
        color: #334155;
        font-size: 13px;
        padding: 6px 12px;
      }
      .input-group-prefixed input {
        border-top-left-radius: 0 !important;
        border-bottom-left-radius: 0 !important;
      }
      
      .tmpl-select-item {
        padding: 10px 14px;
        margin-bottom: 8px;
        background: #f8fafc;
        border: 1px solid #e2e8f0;
        border-radius: 6px;
        cursor: pointer;
        transition: all 0.2s ease;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      .tmpl-select-item:hover { background: #e2e8f0; border-color: #cbd5e1; }
      .tmpl-select-title { font-size: 13px; font-weight: 600; color: #1e293b; }
      
      /* ---- Upload slots: the uploaded file name sits inside the grey box to the right of Browse ---- */
      .file-slot .input-group input[type='text'] { display: none !important; }
      .file-slot .file-chips { height: auto !important; min-height: 38px; background: #eeeeee; border: 1px solid #cfd8e3; padding: 4px 8px; vertical-align: middle; }
      .file-slot .chip-row { display: flex; flex-wrap: wrap; align-items: center; gap: 6px; min-height: 28px; }
      .file-slot .chip { display: inline-flex; align-items: center; max-width: 100%; background: #ffffff; border: 1px solid #cbd5e1; border-radius: 14px; padding: 1px 2px 1px 10px; font-size: 13px; color: #1e293b; }
      .file-slot .chip.active { border-color: #198754; background: #f0fdf4; }
      .file-slot .chip .chip-name { cursor: pointer; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; max-width: 300px; }
      .file-slot .chip .chip-x { border: none; background: transparent; color: #dc2626; font-size: 18px; line-height: 1; cursor: pointer; padding: 0 6px; }
      .file-slot .file-ph { color: #94a3b8; font-size: 14px; line-height: 28px; }
      
      /* ---- Trial switch (far right of the tab bar): sliding button, the word TRIAL is inside it ---- */
      .trial-switch { position: absolute; top: 9px; right: 14px; z-index: 20; display: flex; align-items: center; }
      .trial-switch .form-group { margin: 0; width: auto; }
      .trial-switch .checkbox { margin: 0; padding: 0; min-height: 0; }
      .trial-switch .checkbox label { position: relative; display: block; width: 88px; height: 30px; margin: 0; padding: 0; cursor: pointer; }
      .trial-switch .checkbox label input[type='checkbox'] { position: absolute; left: 0; top: 0; width: 100%; height: 100%; margin: 0; opacity: 0; cursor: pointer; z-index: 2; }
      .trial-switch .checkbox label > span { display: block; position: relative; width: 88px; height: 30px; padding: 0; border-radius: 15px; background: #3f4650; box-shadow: inset 0 1px 4px rgba(0,0,0,0.45); transition: background 0.2s; }
      .trial-switch .checkbox label > span::before { content: 'TRIAL'; position: absolute; right: 12px; top: 0; line-height: 30px; font-size: 12px; font-weight: 700; letter-spacing: 0.8px; color: #8d96a3; transition: color 0.2s; }
      .trial-switch .checkbox label > span::after { content: ''; position: absolute; top: 3px; left: 3px; width: 24px; height: 24px; border-radius: 50%; background: linear-gradient(#4a525c, #2b3036); box-shadow: 0 1px 3px rgba(0,0,0,0.5); transition: left 0.2s; }
      .trial-switch .checkbox label input:checked + span { background: #198754; }
      .trial-switch .checkbox label input:checked + span::before { left: 12px; right: auto; color: #ffffff; }
      .trial-switch .checkbox label input:checked + span::after { left: 61px; }
      @media (max-width: 800px) { .trial-switch { position: static; justify-content: flex-end; margin: 0 0 6px 0; } }
      
      /* ---- dropdowns: lighter arrow, consistent placeholder colour ---- */
      .selectize-control.single .selectize-input { min-height: 38px; border: 1px solid #cbd5e1; border-radius: 6px; box-shadow: none; }
      .selectize-control.single .selectize-input:after { border-color: #94a3b8 transparent transparent transparent !important; border-width: 5px 4px 0 4px !important; right: 12px !important; }
      .selectize-control.single .selectize-input.dropdown-active:after { border-color: transparent transparent #94a3b8 transparent !important; border-width: 0 4px 5px 4px !important; }
      .selectize-input > input::placeholder { color: #94a3b8 !important; }
      
      /* ---- Collapsible S / O / A / P / E / R sections ---- */
      details.soaper-card > summary { cursor: pointer; list-style: none; display: flex; align-items: center; justify-content: space-between; user-select: none; }
      details.soaper-card > summary::-webkit-details-marker { display: none; }
      details.soaper-card > summary::after { content: '▾'; color: #64748b; font-size: 18px; margin-bottom: 12px; }
      details.soaper-card:not([open]) > summary::after { content: '▸'; margin-bottom: 0; }
      details.soaper-card:not([open]) > summary .section-badge { margin-bottom: 0; }
      
      /* ---- Generate button pinned to the bottom ---- */
      .generate-bar { position: sticky; bottom: 0; z-index: 30; padding: 12px 0 6px 0; background: linear-gradient(to top, #ffffff 75%, rgba(255,255,255,0)); }
      .req-star { color: #dc2626; }
    ")),
    tags$script(HTML("
      // Print: put the report into a separate hidden iframe and print that, so the layout of the whole app cannot interfere with page breaks
      function printReport() {
        var el = document.querySelector('.soap-report-view');
        var root = el ? el.querySelector('.report-root') : null;
        if (!root) {
          alert('Please click Generate / Update SOAPER Note first.');
          return;
        }
        var fname = root.getAttribute('data-filename') || 'Physiotherapy_Evaluation_Report';
        var patient = root.getAttribute('data-patient') || '';
        var q = String.fromCharCode(34);
        var footerLeft = patient ? ('Patient: ' + patient) : 'Physiotherapy Evaluation Report';
        
        var old = document.getElementById('print-frame');
        if (old) { old.parentNode.removeChild(old); }
        var iframe = document.createElement('iframe');
        iframe.id = 'print-frame';
        iframe.style.cssText = 'position:fixed;right:0;bottom:0;width:0;height:0;border:0;visibility:hidden;';
        document.body.appendChild(iframe);
        
        // Footer (patient name + Page X of Y): supported by Chrome / Edge 131+; other browsers simply ignore it
        var css = '@page { size: A4; margin: 12mm 12mm 16mm 12mm; ' +
          '@bottom-left { content: ' + q + footerLeft + q + '; font-size: 9px; color: #64748b; } ' +
          '@bottom-right { content: ' + q + 'Page ' + q + ' counter(page) ' + q + ' of ' + q + ' counter(pages); font-size: 9px; color: #64748b; } } ' +
          'html, body { margin: 0; padding: 0; background: #ffffff; }' +
          '* { -webkit-print-color-adjust: exact; print-color-adjust: exact; box-sizing: border-box; }' +
          '.report-root { padding: 0 !important; }' +
          '.avoid-break, tr, th, td, .sec-title { page-break-inside: avoid; break-inside: avoid; }' +
          '.sec-title { page-break-after: avoid; break-after: avoid; }' +
          'table { page-break-inside: auto; }';
        
        // The tags of the print document are assembled from pieces on purpose: literal head / body
        // tags inside this inline script can be mistaken for the real page structure by HTML
        // post-processing on some hosts (the rest of the script then shows up as text on the page).
        var lt = String.fromCharCode(60);
        var tg = function(name, closing) { return lt + (closing ? '/' : '') + name + '>'; };
        var d = iframe.contentWindow.document;
        d.open();
        d.write(lt + '!DOCTYPE html>' + tg('html') + tg('head') + lt + 'meta charset=utf-8>' +
                tg('title') + fname + tg('title', true) + tg('style') + css + tg('style', true) + tg('head', true) +
                tg('body') + el.innerHTML + tg('body', true) + tg('html', true));
        d.close();
        
        // Browsers use the page title as the default PDF file name, so temporarily set it to 'PatientName_Date' while printing
        var oldTitle = document.title;
        document.title = fname;
        setTimeout(function() {
          iframe.contentWindow.focus();
          iframe.contentWindow.print();
          setTimeout(function() { document.title = oldTitle; }, 1500);
        }, 400);
      }
      
      // Ctrl+P / Cmd+P: once a report has been generated, print the report itself
      document.addEventListener('keydown', function(e) {
        if ((e.ctrlKey || e.metaKey) && (e.key === 'p' || e.key === 'P') && document.querySelector('.soap-report-view .report-root')) {
          e.preventDefault();
          printReport();
        }
      });
      
      function fallbackCopy(text) {
        var ta = document.createElement('textarea');
        ta.value = text;
        ta.style.cssText = 'position:fixed;left:-9999px;top:0;';
        document.body.appendChild(ta);
        ta.select();
        try { document.execCommand('copy'); } catch (e) {}
        document.body.removeChild(ta);
      }
      // Copy: turn the report HTML into plain text before putting it on the clipboard (so no HTML code is pasted)
      Shiny.addCustomMessageHandler('copy_to_clipboard', function(html) {
        var box = document.createElement('div');
        box.style.cssText = 'position:absolute;left:-9999px;top:0;width:800px;';
        box.innerHTML = html;
        document.body.appendChild(box);
        var text = box.innerText.replace(/[ \\t]+\\n/g, '\\n').replace(/\\n{3,}/g, '\\n\\n').trim();
        document.body.removeChild(box);
        var done = function() { alert('📋 Clinical Evaluation Form & SOAPER note copied to clipboard (plain text)!'); };
        if (navigator.clipboard && window.isSecureContext) {
          navigator.clipboard.writeText(text).then(done, function() { fallbackCopy(text); done(); });
        } else {
          fallbackCopy(text);
          done();
        }
      });
      
      // ---- Draft autosave (stored only in this browser) ----
      Shiny.addCustomMessageHandler('draft_save', function(json) {
        try { localStorage.setItem('soap_draft_v1', json); } catch (e) {}
      });
      Shiny.addCustomMessageHandler('draft_clear', function(x) {
        try { localStorage.removeItem('soap_draft_v1'); } catch (e) {}
      });
      // Every time the page connects to the server (open / refresh / reconnect), hand the local draft to the server, which asks whether to restore it
      $(document).on('shiny:connected', function() {
        var d = null;
        try { d = localStorage.getItem('soap_draft_v1'); } catch (e) {}
        Shiny.setInputValue('draft_probe', {found: !!d, data: d || '', n: Math.random()}, {priority: 'event'});
      });
    "))
  ),
  
  div(class = "header-bar no-print",
      div(h3("Green Care Physiotherapy - Musculoskeletal Evaluation System", style = "margin:0; font-weight:bold; font-size: 20px;"))
  ),
  
  
  div(style = "position: relative;",
      
      # Trial switch: sits at the far right of the tab bar (slide it ON to try the form without saving anything)
      div(class = "trial-switch no-print", title = "Trial mode: slide to green to try the form without saving anything",
          checkboxInput("trial_mode", NULL, value = FALSE)
      ),
      
      tabsetPanel(id = "main_tabs", type = "tabs",
                  
                  tabPanel("📁 Tab 1: Document Upload & Intake", class = "p-3 bg-white border-top-0 rounded-bottom no-print",
                           div(style = "padding-top: 15px;",
                               fluidRow(
                                 column(6,
                                        div(class = "custom-card",
                                            div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 18px; border-bottom: 2px solid #f1f5f9; padding-bottom: 8px;",
                                                h5("📄 1. Document Intake", style = "color: #198754; font-weight: bold; margin: 0;"),
                                                div(style = "display: flex; gap: 6px;",
                                                    actionButton("btn_new_patient_t1", "🆕 New Patient", class = "btn btn-outline-secondary btn-sm"),
                                                    actionButton("btn_load_demo", "🧪 Demo Data", class = "btn btn-outline-secondary btn-sm")
                                                )
                                            ),
                                            uiOutput("ui_file_referral"),
                                            uiOutput("ui_rows_referral"),
                                            uiOutput("ui_file_imaging"),
                                            uiOutput("ui_rows_imaging"),
                                            h5("👤 2. Patient Demographics & Dx", style = "color: #198754; font-weight: bold; margin-top: 25px; margin-bottom: 18px; border-bottom: 2px solid #f1f5f9; padding-bottom: 8px;"),
                                            fluidRow(
                                              column(6, textInput("ext_name", HTML("Patient Name: <span class='req-star'>*</span>"), value = "", placeholder = "Full name", width = "100%")),
                                              column(6, textInput("ext_ic", HTML("IC / Passport: <span class='req-star'>*</span>"), value = "", placeholder = "e.g. 900101-01-1234", width = "100%"))
                                            ),
                                            div(class = "form-group", style = "margin-bottom: 10px;",
                                                tags$label("Patient ID:", style = "font-size: 12px; font-weight: 600; color: #334155; margin-bottom: 4px; display: block;"),
                                                div(class = "input-group input-group-prefixed", style = "display: flex; width: 100%;",
                                                    span(class = "input-group-text-prefix", "GC"),
                                                    tags$input(id = "ext_id_num", type = "text", class = "form-control", value = "", maxlength = "4", placeholder = "e.g. 2026", style = "flex: 1;")
                                                )
                                            ),
                                            dateInput("visit_date", HTML("Date of Assessment: <span class='req-star'>*</span>"), value = Sys.Date(), max = Sys.Date(), format = "dd/mm/yyyy", width = "100%"),
                                            textAreaInput("ext_dx", "Primary Clinical Diagnosis:", value = "", placeholder = "e.g. L4-L5 lumbar disc herniation with L5 radiculopathy", rows = 2, width = "100%"),
                                            textAreaInput("ext_findings", "Key Diagnostic Findings:", value = "", placeholder = "e.g. [MRI] ...  [X-Ray] ...", rows = 3, width = "100%"),
                                            actionButton("btn_go_to_eval", "➡️ Next: Evaluation Form", class = "btn btn-success btn-block w-100 fw-bold", style = "width: 100%; margin-top: 15px; padding: 12px; font-size: 15px; border-radius: 8px;")
                                        )
                                 ),
                                 column(6,
                                        div(class = "custom-card", style = "min-height: 680px;",
                                            h5("🖼 Interactive Document Viewer", style = "color: #198754; font-weight: bold; margin-top: 0; margin-bottom: 18px; border-bottom: 2px solid #f1f5f9; padding-bottom: 8px;"),
                                            uiOutput("ui_doc_preview")
                                        )
                                 )
                               )
                           )
                  ),
                  
                  tabPanel("✍ Tab 2: Evaluation Form & SOAPER Matrix", class = "p-3 bg-white border-top-0 rounded-bottom",
                           div(style = "padding-top: 15px;",
                               fluidRow(
                                 column(6, class = "no-print",
                                        div(class = "custom-card",
                                            div(class = "d-flex justify-content-between align-items-center mb-3",
                                                h5("📋 MUSCULOSKELETAL EVALUATION FORM", style = "color: #198754; font-weight: bold; margin:0;"),
                                                div(style = "display: flex; gap: 6px; flex-wrap: wrap; justify-content: flex-end;",
                                                    actionButton("btn_new_patient", "🆕 New Patient", class = "btn btn-outline-secondary btn-sm fw-bold", style = "border-radius: 6px;"),
                                                    actionButton("btn_load_last_visit", "🔄 Import Last Record by IC", class = "btn btn-outline-success btn-sm fw-bold", style = "border-radius: 6px;")
                                                )
                                            ),
                                            
                                            div(style = "text-align: right; margin: -4px 0 10px 0;",
                                                tags$a(href = "#", onclick = "document.querySelectorAll('details.soaper-card').forEach(function(d){d.open = true;}); return false;", style = "font-size: 12px; margin-right: 12px;", "Expand all"),
                                                tags$a(href = "#", onclick = "document.querySelectorAll('details.soaper-card').forEach(function(d){d.open = false;}); return false;", style = "font-size: 12px;", "Collapse all")
                                            ),
                                            
                                            tags$details(class = "soaper-card soaper-card-s", open = NA,
                                                         tags$summary(span(class = "section-badge bg-s", "S - Subjective History")),
                                                         tags$label("1. Chief Complaints & Medical History", style = "font-weight: bold; font-size: 13px; display:block; color: #0369a1; margin-bottom: 6px;"),
                                                         textAreaInput("s_chief_complaints", "Chief Complaints:", value = "", placeholder = "e.g. Lower back pain radiating down to left leg", rows = 2, width = "100%"),
                                                         blank_selectize("s_past_med", "Past Medical Hx:"),
                                                         fluidRow(
                                                           column(6, blank_selectize("s_personal_hx", "Personal Hx:")),
                                                           column(6, blank_selectize("s_family_hx", "Family Hx:"))
                                                         ),
                                                         tags$label("2. Symptoms History", style = "font-weight: bold; font-size: 13px; display:block; color: #0369a1; margin-top: 10px; margin-bottom: 6px;"),
                                                         textAreaInput("s_hpi", "History of Present Illness (HPI) / Current Condition:", value = "", placeholder = "Onset, mechanism of injury, progression, current condition...", rows = 2, width = "100%"),
                                                         fluidRow(
                                                           column(4, blank_selectize("sym_side", "Side:")),
                                                           column(4, blank_selectize("sym_site", "Site:")),
                                                           column(4, blank_selectize("sym_onset", "Onset:"))
                                                         ),
                                                         fluidRow(
                                                           column(4, blank_selectize("sym_duration", "Duration:")),
                                                           column(4, blank_selectize("sym_type", "Type:")),
                                                           column(4, sliderInput("sym_vas", "VAS Pain (0-10):", min=0, max=10, value=0))
                                                         ),
                                                         fluidRow(
                                                           column(6, blank_selectize("sym_agg", "Aggravating Factors:")),
                                                           column(6, blank_selectize("sym_rel", "Relieving Factors:"))
                                                         ),
                                                         fluidRow(
                                                           column(6, uiOutput("ui_sym_agg_other")),
                                                           column(6, uiOutput("ui_sym_rel_other"))
                                                         ),
                                                         
                                                         fluidRow(
                                                           column(6, blank_selectize("sym_24h", "24-Hour Behavior:")),
                                                           column(6, blank_selectize("sym_prev_tx", "Previous Treatments:"))
                                                         ),
                                                         fluidRow(
                                                           column(6, uiOutput("ui_sym_24h_other")),
                                                           column(6, uiOutput("ui_sym_prev_tx_other"))
                                                         ),
                                                         textInput("sym_func_impact", "Functional Impairment:", value = "", placeholder = "e.g. Unable to sit > 20 mins", width = "100%")
                                            ),
                                            
                                            tags$details(class = "soaper-card soaper-card-o", open = NA,
                                                         tags$summary(span(class = "section-badge bg-o", "O - Objective Examination")),
                                                         selectizeInput("o_joint_select", "📍 Target Joint(s) / Region(s):", choices = c("Shoulder Joint", "Lumbar Spine", "Cervical Spine", "Thoracic Spine / Scoliosis", "Knee Joint", "Hip Joint", "Ankle Joint"), selected = NULL, multiple = TRUE, width = "100%", options = list(placeholder = "Select one or more regions...", plugins = list("remove_button"))),
                                                         tags$label("1. Observation & Palpation", style = "font-weight: bold; font-size: 13px; display:block; color: #92400e; margin-top: 10px; margin-bottom: 6px;"),
                                                         fluidRow(
                                                           column(4, textAreaInput("obs_general", "General Observation:", value = "", placeholder = "e.g. Posture, gait, swelling", rows = 2)),
                                                           column(4, textAreaInput("obs_local", "Local Observation:", value = "", placeholder = "e.g. Muscle spasm, atrophy, skin", rows = 2)),
                                                           column(4, textAreaInput("obs_palpation", "Palpation Findings:", value = "", placeholder = "e.g. Tenderness at ...", rows = 2))
                                                         ),
                                                         tags$label("2. Range of Motion (ROM)", style = "font-weight: bold; font-size: 13px; display:block; color: #92400e; margin-top: 12px; margin-bottom: 4px;"),
                                                         uiOutput("ui_rom_table"),
                                                         tags$label("3. Muscle Strength Testing (MMT Grade 0-5)", style = "font-weight: bold; font-size: 13px; display:block; color: #92400e; margin-top: 12px; margin-bottom: 4px;"),
                                                         uiOutput("ui_mmt_group_table"),
                                                         uiOutput("ui_special_tests"),
                                                         tags$label("5. Balance Assessment", style = "font-weight: bold; font-size: 13px; display:block; color: #92400e; margin-top: 12px; margin-bottom: 4px;"),
                                                         tags$table(class = "table table-sm table-bordered table-rom", style = "margin-bottom: 10px; background: #fff;",
                                                                    tags$thead(style = "background: #f8f9fa; font-size: 11px;", tags$tr(tags$th("Component"), tags$th("Performance Level"))),
                                                                    tags$tbody(
                                                                      tags$tr(tags$td(style = "font-size: 12px; font-weight: bold; width: 45%; vertical-align: middle;", "Sitting"), tags$td(style = "width: 55%; vertical-align: middle;", selectInput("bal_sitting", NULL, choices = c("Good", "Fair", "Poor", "N/A"), selected = "N/A", width = "100%"))),
                                                                      tags$tr(tags$td(style = "font-size: 12px; font-weight: bold; width: 45%; vertical-align: middle;", "Standing"), tags$td(style = "width: 55%; vertical-align: middle;", selectInput("bal_standing", NULL, choices = c("Good", "Fair", "Poor", "N/A"), selected = "N/A", width = "100%"))),
                                                                      tags$tr(tags$td(style = "font-size: 12px; font-weight: bold; width: 45%; vertical-align: middle;", "Single Leg"), tags$td(style = "width: 55%; vertical-align: middle;", selectInput("bal_sls", NULL, choices = c("Good", "Fair", "Poor", "N/A"), selected = "N/A", width = "100%"))),
                                                                      tags$tr(tags$td(style = "font-size: 12px; font-weight: bold; width: 45%; vertical-align: middle;", "Tandem"), tags$td(style = "width: 55%; vertical-align: middle;", selectInput("bal_tandem", NULL, choices = c("Good", "Fair", "Poor", "N/A"), selected = "N/A", width = "100%")))
                                                                    )
                                                         ),
                                                         tags$label("6. Functional Assessment", style = "font-weight: bold; font-size: 13px; display:block; color: #92400e; margin-top: 12px; margin-bottom: 4px;"),
                                                         tags$table(class = "table table-sm table-bordered table-rom", style = "margin-bottom: 10px; background: #fff;",
                                                                    tags$thead(style = "background: #f8f9fa; font-size: 11px;", tags$tr(tags$th("Component"), tags$th("Performance Level"))),
                                                                    tags$tbody(
                                                                      tags$tr(tags$td(style = "font-size: 12px; font-weight: bold; width: 45%; vertical-align: middle;", "Bed Mobility"), tags$td(style = "width: 55%; vertical-align: middle;", selectInput("func_bed", NULL, choices = c("Good", "Fair", "Poor", "N/A"), selected = "N/A", width = "100%"))),
                                                                      tags$tr(tags$td(style = "font-size: 12px; font-weight: bold; width: 45%; vertical-align: middle;", "Transfers"), tags$td(style = "width: 55%; vertical-align: middle;", selectInput("func_transfer", NULL, choices = c("Good", "Fair", "Poor", "N/A"), selected = "N/A", width = "100%"))),
                                                                      tags$tr(tags$td(style = "font-size: 12px; font-weight: bold; width: 45%; vertical-align: middle;", "Sit to Stand"), tags$td(style = "width: 55%; vertical-align: middle;", selectInput("func_sts", NULL, choices = c("Good", "Fair", "Poor", "N/A"), selected = "N/A", width = "100%"))),
                                                                      tags$tr(tags$td(style = "font-size: 12px; font-weight: bold; width: 45%; vertical-align: middle;", "Gait"), tags$td(style = "width: 55%; vertical-align: middle;", selectInput("func_gait", NULL, choices = c("Good", "Fair", "Poor", "N/A"), selected = "N/A", width = "100%")))
                                                                    )
                                                         ),
                                                         tags$label("7. Joint Stability & Measurements", style = "font-weight: bold; font-size: 13px; display:block; color: #92400e; margin-top: 12px; margin-bottom: 4px;"),
                                                         fluidRow(
                                                           column(6, selectInput("joint_integrity", "Joint Stability:", choices = c("— Select —" = "", "Normal Play & Stability" = "Normal Joint Play & Stability", "Hypermobile / Laxity" = "Joint Hypermobility / Laxity", "Hypomobile / Restricted" = "Joint Hypomobility / Restricted Play", "Instability Positive" = "Apparent Joint Instability"), selected = "", width = "100%")),
                                                           column(6, textInput("meas_assess", "Measurements (Girth/Length):", value = "", placeholder = "e.g. Girth: equal bilaterally", width = "100%"))
                                                         )
                                            ),
                                            
                                            tags$details(class = "soaper-card soaper-card-a", open = NA,
                                                         tags$summary(span(class = "section-badge bg-a", "A - Assessment & Safety Red Flags")),
                                                         textAreaInput("a_pt_impression", "Physical Therapy Clinical Impression:", value = "", placeholder = "Clinical impression / working diagnosis", rows = 3, width = "100%"),
                                                         selectInput("a_impression", "Clinical Status Impression:", choices = c("— Select —" = "", "🟢 Progressing as expected" = "Progressing as expected", "🟡 Plateauing (requires adjustment)" = "Plateauing (requires adjustment)", "🔴 Exacerbation (requires safety re-eval)" = "Exacerbation (requires safety re-eval)"), selected = "", width = "100%"),
                                                         div(class = "flag-box",
                                                             uiOutput("ui_dynamic_flags"),
                                                             textInput("a_precautions", "Precautions / Contraindications:", value = "", placeholder = "e.g. Avoid end-range loading", width = "100%")
                                                         )
                                            ),
                                            
                                            tags$details(class = "soaper-card soaper-card-p", open = NA,
                                                         tags$summary(span(class = "section-badge bg-p", "P - Treatment Plan Matrix")),
                                                         div(class = "tmpl-btn-bar",
                                                             tags$label("Treatment Interventions & Dosages:", style = "font-size: 13px; font-weight: 600; margin: 0; color: #166534;"),
                                                             actionButton("btn_apply_treatment_tmpl", "📋 Apply Treatment Template", class = "btn btn-outline-success btn-xs fw-bold", style = "font-size: 11px; padding: 2px 8px;")
                                                         ),
                                                         textAreaInput("p_treatment_plan", label = NULL, value = "", placeholder = "Manual therapy, exercise, modalities and dosages...", rows = 4, width = "100%")
                                            ),
                                            
                                            tags$details(class = "soaper-card soaper-card-e", open = NA,
                                                         tags$summary(span(class = "section-badge bg-e", "E - Post-Tx Evaluation")),
                                                         sliderInput("e_vas_post", "Post-Tx VAS Pain (0-10):", min = 0, max = 10, value = 0, width = "100%")
                                            ),
                                            
                                            tags$details(class = "soaper-card soaper-card-r", open = NA,
                                                         tags$summary(span(class = "section-badge bg-r", "R - Home Plan & Next Review")),
                                                         div(class = "tmpl-btn-bar",
                                                             tags$label("Home Exercise Program (HEP):", style = "font-size: 13px; font-weight: 600; margin: 0; color: #6b21a8;"),
                                                             actionButton("btn_apply_hep_tmpl", "🏠 Apply HEP Template", class = "btn btn-outline-success btn-xs fw-bold", style = "font-size: 11px; padding: 2px 8px;")
                                                         ),
                                                         textAreaInput("r_hep", label = NULL, value = "", placeholder = "Home exercises, reps/sets, precautions...", rows = 3, width = "100%"),
                                                         textAreaInput("r_next_plan", "Next Session Plan & Clinical Review Focus:", value = "", placeholder = "Next session focus and review points...", rows = 2, width = "100%"),
                                                         blank_selectize("r_next_schedule", "Follow-up Schedule:", width = "100%")
                                            ),
                                            
                                            div(class = "generate-bar",
                                                actionButton("btn_generate_note", "⚡ Generate / Update SOAPER Note", class = "btn-success w-100 fw-bold", style = "padding: 12px; font-size: 15px; border-radius: 8px; width: 100%;")
                                            )
                                        )
                                 ),
                                 
                                 column(12, class = "col-md-6",
                                        div(class = "custom-card printable-soap",
                                            h5("✨ Generated Clinical Evaluation Note & Form", style = "color: #198754; font-weight: bold; margin-top: 0;"),
                                            p(style = "font-size: 12px; color: #64748b;", "Fill out the evaluation form on the left and click '⚡ Generate / Update SOAPER Note' to generate the report."),
                                            
                                            div(style = "margin-bottom: 15px;",
                                                htmlOutput("soaper_report_display")
                                            ),
                                            
                                            div(class = "no-print", style = "display: flex; gap: 10px;",
                                                actionButton("btn_copy", "📋 Copy Full Evaluation Form Note", class = "btn btn-primary", style = "flex: 1;"),
                                                actionButton("btn_print", "🖨️ Print / Export PDF", class = "btn btn-default", onclick = "printReport();", style = "flex: 1;")
                                            )
                                        )
                                 )
                               )
                           )
                  ),
                  tabPanel("⚙️ Tab 3: Templates Manager", class = "p-3 bg-white border-top-0 rounded-bottom no-print",
                           div(style = "padding-top: 15px;",
                               fluidRow(
                                 column(4,
                                        div(class = "custom-card",
                                            h5("➕ Add / Edit Template", style = "color: #198754; font-weight: bold; margin-top: 0; margin-bottom: 8px; border-bottom: 2px solid #f1f5f9; padding-bottom: 8px;"),
                                            uiOutput("ui_tmpl_mode"),
                                            hidden(numericInput("tmpl_edit_id", "ID", value = 0)),
                                            selectInput("tmpl_type", "Template Category:", choices = c("Treatment Plan" = "treatment", "Home Exercise Program (HEP)" = "hep"), width = "100%"),
                                            textInput("tmpl_title", "Template Title:", placeholder = "e.g., Lumbar Disc Herniation Standard Care", width = "100%"),
                                            textAreaInput("tmpl_content", "Template Content:", placeholder = "Enter specific treatment items, dosages, or home exercise instructions...", rows = 8, width = "100%"),
                                            div(style = "display: flex; gap: 10px; margin-top: 15px;",
                                                actionButton("btn_save_template", "💾 Save Template", class = "btn btn-success fw-bold", style = "flex: 1;"),
                                                actionButton("btn_clear_template_form", "🧹 Clear Form", class = "btn btn-outline-secondary", style = "flex: 1;")
                                            )
                                        )
                                 ),
                                 column(8,
                                        div(class = "custom-card",
                                            textInput("tmpl_search", NULL, placeholder = "🔍 Search templates by title or content...", width = "100%"),
                                            tabsetPanel(id = "tmpl_sub_tabs", type = "pills",
                                                        tabPanel("📋 Treatment Plan Templates", div(style = "padding-top: 15px;", uiOutput("ui_treatment_templates_list"))),
                                                        tabPanel("🏠 HEP Templates", div(style = "padding-top: 15px;", uiOutput("ui_hep_templates_list")))
                                            )
                                        )
                                 )
                               )
                           )
                  )
      )
  )
)

# ==================== 4. SERVER LOGIC ====================

server <- function(input, output, session) {
  
  # ---------- State ----------
  tmpl_trigger <- reactiveVal(0)
  current_report_text <- reactiveVal("")
  
  form_store <- reactiveValues()      # Temporary store of ROM / MMT / special-test inputs: switching regions and back does not lose them
  form_epoch <- reactiveVal(0)        # Increment = re-render the dynamic tables and the red-flag list
  pending_flags <- reactiveVal(NULL)  # Red-flag ticks to apply once the region switch has finished (after load / clear / draft restore)
  nonce_ref <- reactiveVal(0)         # Increment = re-create the Referral Letter upload box (empties it so it is ready for the next file)
  nonce_img <- reactiveVal(0)         # Increment = re-create the Diagnostic Reports upload box
  staged_docs <- reactiveVal(list())          # newly uploaded files, copied into a private staging folder
  removed_saved <- reactiveVal(character(0))  # Saved attachments removed with the x button (tracked by key)
  doc_sel <- reactiveVal(1L)          # Which file the preview is showing
  loaded_docs <- reactiveVal(list())  # Attachments that came with a saved record
  pending_record <- reactiveVal(NULL)
  pending_template <- reactiveVal(NULL)
  pending_delete_id <- reactiveVal(NULL)
  draft_armed <- reactiveVal(FALSE)   # Until the draft check has finished, never write or clear the local draft
  pending_draft <- reactiveVal(NULL)
  last_generated <- reactiveVal(NULL) # What was just generated and saved (so saved content is not stored again as a draft)
  
  # Temporary folder for previews (one per session, deleted when the session ends)
  view_dir <- file.path(VIEW_ROOT, session$token)
  dir.create(view_dir, recursive = TRUE, showWarnings = FALSE)
  stage_dir <- file.path(tempdir(), "doc_stage", session$token)
  dir.create(stage_dir, recursive = TRUE, showWarnings = FALSE)
  session$onSessionEnded(function() {
    unlink(view_dir, recursive = TRUE)
    unlink(stage_dir, recursive = TRUE)
  })
  
  # The default of a date input is frozen when the app starts, so refresh it to 'today' at the start of every session
  session$onFlushed(function() {
    updateDateInput(session, "visit_date", value = Sys.Date(), max = Sys.Date())
  }, once = TRUE)
  
  get_full_patient_id <- reactive({
    num <- if (!is.null(input$ext_id_num)) trimws(input$ext_id_num) else ""
    if (nzchar(num)) paste0("GC", num) else ""
  })
  
  output$ui_sym_agg_other <- renderUI({
    if (!is.null(input$sym_agg) && input$sym_agg == "Other") {
      textInput("sym_agg_other_text", "Other Aggravating Factor:", placeholder = "Please specify...", width = "100%")
    }
  })
  
  output$ui_sym_rel_other <- renderUI({
    if (!is.null(input$sym_rel) && input$sym_rel == "Other") {
      textInput("sym_rel_other_text", "Other Relieving Factor:", placeholder = "Please specify...", width = "100%")
    }
  })
  
  output$ui_sym_24h_other <- renderUI({
    if (!is.null(input$sym_24h) && input$sym_24h == "Other") {
      textInput("sym_24h_other_text", "Other 24-Hour Behavior:", placeholder = "Please specify...", width = "100%")
    }
  })
  
  output$ui_sym_prev_tx_other <- renderUI({
    if (!is.null(input$sym_prev_tx) && input$sym_prev_tx == "Other") {
      textInput("sym_prev_tx_other_text", "Other Previous Treatments:", placeholder = "Please specify...", width = "100%")
    }
  })
  
  # ==================== Form: load / clear ====================
  
  set_form_values <- function(vals) {
    for (id in names(vals)) {
      v <- vals[[id]]
      if (id == "o_joint_select") {            # multi-select
        v <- as.character(unlist(v))
        v <- v[!is.na(v) & nzchar(v)]
        updateSelectInput(session, id, selected = if (length(v) > 0) v else character(0))
        next
      }
      if (id %in% FORM_SLIDER_IDS) {
        v <- suppressWarnings(as.numeric(v[1]))
        if (length(v) == 0 || is.na(v)) v <- 0
        updateSliderInput(session, id, value = v)
        next
      }
      v <- if (is.null(v) || length(v) == 0 || is.na(v[1])) "" else as.character(v[1])
      if (id %in% FORM_TEXT_IDS) {
        updateTextInput(session, id, value = v)
      } else if (id %in% FORM_AREA_IDS) {
        updateTextAreaInput(session, id, value = v)
      } else if (id %in% FORM_SELECT_IDS) {
        updateSelectInput(session, id, selected = v)
      } else if (id %in% names(SELECT_CHOICES)) {
        ch <- unique(c(SELECT_CHOICES[[id]], if (nzchar(v)) v))
        updateSelectizeInput(session, id, choices = ch, selected = v)
      }
    }
  }
  
  reset_store <- function() {
    for (k in names(reactiveValuesToList(form_store))) form_store[[k]] <- NULL
  }
  
  set_pending_flags <- function(joints_now, flags) {
    pending_flags(list(joints = sort(as.character(unlist(joints_now))), flags = as.character(unlist(flags))))
  }
  
  clear_form <- function() {
    reset_store()
    set_form_values(BLANK_VALUES)
    set_pending_flags(character(0), character(0))
    loaded_docs(list())
    clear_staged()
    removed_saved(character(0)); doc_sel(1L)
    nonce_ref(nonce_ref() + 1); nonce_img(nonce_img() + 1)
    updateDateInput(session, "visit_date", value = Sys.Date())
    current_report_text("")
    pending_record(NULL)
    last_generated(NULL)
    form_epoch(form_epoch() + 1)
  }
  
  load_demo <- function() {
    clear_form()
    set_form_values(DEMO_VALUES)
    joint <- DEMO_VALUES$o_joint_select
    for (i in seq_along(get_movements_for_joint(joint))) {
      form_store[[store_key("rom_arom", joint, i)]] <- if (i == 1) "120" else "45"
      form_store[[store_key("rom_prom", joint, i)]] <- if (i == 1) "130" else "50"
      form_store[[store_key("rom_remark", joint, i)]] <- if (i == 1) "Pain at end-range" else "Mild stiffness"
    }
    for (i in seq_along(get_mmt_groups_for_joint(joint))) {
      form_store[[store_key("mmt", joint, i)]] <- if (i == 1) "4/5" else "5/5"
    }
    for (i in seq_along(get_special_tests_for_joint(joint))) {
      form_store[[store_key("st", joint, i)]] <- if (i == 1) "Pos" else "ND"
    }
    set_pending_flags(joint, character(0))
    form_epoch(form_epoch() + 1)
  }
  
  confirm_new_patient <- function() {
    showModal(modalDialog(
      title = "🆕 Start a new patient?",
      p("This clears the patient details, evaluation entries, uploaded documents and the generated report on screen."),
      p("Records already saved in the database are not affected."),
      easyClose = TRUE,
      footer = tagList(modalButton("Cancel"), actionButton("btn_confirm_new_patient", "Clear & Start New", class = "btn btn-danger"))
    ))
  }
  observeEvent(input$btn_new_patient, confirm_new_patient())
  observeEvent(input$btn_new_patient_t1, confirm_new_patient())
  observeEvent(input$btn_confirm_new_patient, {
    removeModal()
    clear_form()
    updateTabsetPanel(session, "main_tabs", selected = "📁 Tab 1: Document Upload & Intake")
    showNotification("🆕 Form cleared. Ready for a new patient.", type = "message")
  })
  
  observeEvent(input$btn_load_demo, {
    showModal(modalDialog(
      title = "🧪 Load demo data?",
      p("Fills the whole form with sample patient data for training / testing. Anything currently entered will be replaced."),
      p(strong("Do not use demo data for real patients.")),
      easyClose = TRUE,
      footer = tagList(modalButton("Cancel"), actionButton("btn_confirm_demo", "Load Demo Data", class = "btn btn-warning"))
    ))
  })
  observeEvent(input$btn_confirm_demo, {
    removeModal()
    load_demo()
    showNotification("🧪 Demo data loaded.", type = "message")
  })
  
  # ==================== Patient details check ====================
  
  validate_patient <- function(go_to_tab1 = FALSE) {
    name <- trimws(input$ext_name %||% "")
    ic <- normalize_ic(input$ext_ic)
    vd <- input$visit_date
    problem <- NULL
    if (!nzchar(name)) {
      problem <- "⚠️ Patient name is required (Tab 1)."
    } else if (!nzchar(ic)) {
      problem <- "⚠️ IC / Passport number is required (Tab 1)."
    } else if (!is_valid_ic(ic)) {
      problem <- "⚠️ IC / Passport looks invalid. Use 12-digit IC (e.g. 900101-01-1234) or a passport number (letters / digits, 5-20 characters)."
    } else if (is.null(vd) || length(vd) != 1 || is.na(vd)) {
      problem <- "⚠️ Date of Assessment is required (Tab 1)."
    } else if (as.Date(vd) > Sys.Date()) {
      problem <- "⚠️ Date of Assessment cannot be in the future."
    }
    if (!is.null(problem)) {
      showNotification(problem, type = "error", duration = 8)
      if (go_to_tab1) updateTabsetPanel(session, "main_tabs", selected = "📁 Tab 1: Document Upload & Intake")
      return(NULL)
    }
    if (!identical(ic, trimws(input$ext_ic %||% ""))) updateTextInput(session, "ext_ic", value = ic)
    list(name = name, ic = ic, date = as.Date(vd))
  }
  
  observeEvent(input$btn_go_to_eval, {
    if (!isTRUE(input$trial_mode) && is.null(validate_patient())) return()
    updateTabsetPanel(session, "main_tabs", selected = "✍ Tab 2: Evaluation Form & SOAPER Matrix")
  })
  
  # ==================== Tab 1: file upload and preview ====================
  
  upload_label <- function(txt) tags$label(txt, style = "font-size: 13px; font-weight: 600; color: #334155; margin-bottom: 6px; display: block;")
  
  # An upload box: the standard Shiny file input (Browse button + progress bar), with the grey text
  # field replaced by a field that shows the uploaded file name(s) as chips, each with an x button.
  upload_slot <- function(label, id, multiple, placeholder, chips_id) {
    widget <- fileInput(id, label = NULL, width = "100%", multiple = multiple, accept = c(".pdf", ".png", ".jpg", ".jpeg"),
                        buttonLabel = "Browse...", placeholder = placeholder)
    chips <- uiOutput(chips_id, container = function(...) tags$div(..., class = "form-control file-chips"))
    if (utils::packageVersion("htmltools") >= "0.5.2") {
      widget <- htmltools::tagQuery(widget)$find(".input-group")$append(chips)$allTags()
    } else {
      widget <- tagList(widget, chips)   # very old htmltools: show the chips below the box instead
    }
    div(class = "file-slot", style = "margin-bottom: 16px;", upload_label(label), div(class = "input-wrapper-inner", widget))
  }
  
  output$ui_file_referral <- renderUI({
    upload_slot("1️⃣ Referral Letter:", paste0("file_referral_", nonce_ref()), FALSE, "Upload PDF or Image", "ui_chips_referral")
  })
  
  output$ui_file_imaging <- renderUI({
    upload_slot("2️⃣ Diagnostic Reports (MRI / CT / X-Ray):", paste0("files_imaging_", nonce_img()), TRUE, "Select multiple files...", "ui_chips_imaging")
  })
  
  ref_upload <- reactive(input[[paste0("file_referral_", nonce_ref())]])
  img_upload <- reactive(input[[paste0("files_imaging_", nonce_img())]])
  
  # Delete the staged copies and forget them
  clear_staged <- function() {
    for (d in staged_docs()) unlink(d$path)
    staged_docs(list())
  }
  
  # As soon as a file finishes uploading, copy it into the private staging folder and reset the
  # upload box, so it is ready for the next file. The staged files are shown (with a remove button)
  # inside each upload box, to the right of the Browse button.
  stage_upload <- function(up, role) {
    if (is.null(up) || nrow(up) == 0) return()
    cur <- staged_docs()
    for (i in seq_len(nrow(up))) {
      key <- if (role == "referral") "referral" else paste0("imaging|", up$name[i])
      old <- cur[[key]]
      if (!is.null(old)) unlink(old$path)
      dest <- tempfile(pattern = paste0(role, "_"), tmpdir = stage_dir, fileext = paste0(".", tools::file_ext(up$name[i])))
      if (isTRUE(file.copy(up$datapath[i], dest))) {
        cur[[key]] <- list(key = key, role = role, name = up$name[i], path = dest, persisted = FALSE)
      }
    }
    staged_docs(cur)
  }
  
  observeEvent(ref_upload(), {
    up <- ref_upload()
    req(up)
    stage_upload(up[1, , drop = FALSE], "referral")
    nonce_ref(nonce_ref() + 1)
  })
  
  observeEvent(img_upload(), {
    up <- img_upload()
    req(up)
    stage_upload(up, "imaging")
    nonce_img(nonce_img() + 1)
  })
  
  # All documents = attachments of a loaded record + newly staged uploads (a new upload replaces a
  # saved file in the same slot). Files removed with the x button are left out.
  doc_items <- reactive({
    items <- list()
    gone_saved <- removed_saved()
    for (d in loaded_docs()) {
      if (!(d$key %in% gone_saved)) items[[d$key]] <- d
    }
    for (d in staged_docs()) items[[d$key]] <- d
    items <- unname(items)
    if (length(items) > 1) items <- items[order(vapply(items, function(x) x$role != "referral", logical(1)))]
    items
  })
  
  observeEvent(input$doc_pick, {
    i <- suppressWarnings(as.integer(input$doc_pick$i))
    if (length(i) == 1 && !is.na(i)) doc_sel(i)
  })
  
  # The x button next to a file removes it from this record
  observeEvent(input$doc_remove, {
    items <- doc_items()
    i <- suppressWarnings(as.integer(input$doc_remove$i))
    if (length(i) != 1 || is.na(i) || i < 1 || i > length(items)) return()
    it <- items[[i]]
    if (isTRUE(it$persisted)) {
      removed_saved(c(removed_saved(), it$key))
    } else {
      cur <- staged_docs()
      unlink(it$path)
      cur[[it$key]] <- NULL
      staged_docs(cur)
    }
    doc_sel(1L)
    showNotification(
      if (isTRUE(it$persisted)) sprintf("🗑️ \"%s\" will be removed from this record when you click Generate.", it$name) else sprintf("🗑️ Removed \"%s\".", it$name),
      type = "message")
  })
  
  # The file names shown inside an upload box (to the right of Browse). Click a name to preview it;
  # click the x to remove it from this record.
  doc_chips <- function(role, placeholder) {
    items <- doc_items()
    idx <- which(vapply(items, function(x) identical(x$role, role), logical(1)))
    if (length(idx) == 0) return(tags$span(class = "file-ph", placeholder))
    sel <- doc_sel()
    div(class = "chip-row", lapply(idx, function(i) {
      it <- items[[i]]
      active <- identical(as.integer(sel), as.integer(i))
      div(class = if (active) "chip active" else "chip",
          title = paste0(it$name, if (isTRUE(it$persisted)) "  (saved with the record)" else "  (new)"),
          tags$span(class = "chip-name",
                    onclick = sprintf("Shiny.setInputValue('doc_pick', {i: %d, n: Math.random()}, {priority: 'event'});", i),
                    it$name),
          tags$button(type = "button", class = "chip-x", title = "Remove this file",
                      onclick = sprintf("Shiny.setInputValue('doc_remove', {i: %d, n: Math.random()}, {priority: 'event'});", i),
                      HTML("&times;"))
      )
    }))
  }
  
  output$ui_chips_referral <- renderUI(doc_chips("referral", "Upload PDF or Image"))
  output$ui_chips_imaging <- renderUI(doc_chips("imaging", "Select multiple files..."))
  
  output$ui_doc_preview <- renderUI({
    items <- doc_items()
    if (length(items) == 0) {
      return(div(style = "border: 2px dashed #cbd5e1; border-radius: 8px; padding: 40px; text-align: center; color: #94a3b8; background: #fafafa; height: 580px; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                 h4("📄 Original Document Preview", style = "font-weight: bold; color: #64748b; margin-bottom: 10px;"),
                 p("Upload a Referral Letter or Diagnostic Report on the left.", style = "font-size: 13px; color: #64748b; margin: 0;")
      ))
    }
    idx <- doc_sel()
    if (length(idx) == 0 || is.na(idx) || idx < 1 || idx > length(items)) idx <- 1L
    it <- items[[idx]]
    
    if (!file.exists(it$path)) {
      return(div(style = "padding: 30px; text-align: center; color: #b91c1c; border: 1px solid #fecaca; background: #fef2f2; border-radius: 8px;",
                 sprintf("The file \"%s\" could not be found on the server.", it$name)))
    }
    
    # The browser cannot read temporary paths on the server directly, so copy the file into this session's served folder first
    fname <- paste0(it$role, "_", idx, "_", safe_name(it$name))
    dest <- file.path(view_dir, fname)
    file.copy(it$path, dest, overwrite = TRUE)
    url <- paste0("doc_view/", session$token, "/", utils::URLencode(fname), "?v=", as.integer(file.mtime(dest)), "_", file.size(dest))
    
    if (grepl("\\.pdf$", it$name, ignore.case = TRUE)) {
      tags$iframe(src = url, style = "width:100%; height:540px; border:none; border-radius: 6px;")
    } else {
      div(style = "text-align: center; background: #fff; padding: 10px; border: 1px solid #ddd; border-radius: 8px; height: 540px; overflow: auto; display: flex; align-items: center; justify-content: center;",
          tags$img(src = url, style = "max-width: 100%; max-height: 520px; object-fit: contain;")
      )
    }
  })
  
  # On Generate: save the files permanently to patient_files/<IC>/<visit date>/ and write their paths into the record
  persist_documents <- function(ic, visit_date) {
    items <- doc_items()
    dest_dir <- file.path(UPLOAD_ROOT, safe_name(ic), as.character(visit_date))
    out <- list()
    attempted <- character(0)
    if (length(items) > 0) {
      dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
      for (it in items) {
        prefix <- if (it$role == "referral") "referral_" else "imaging_"
        dest <- file.path(dest_dir, paste0(prefix, safe_name(it$name)))
        attempted <- c(attempted, dest)
        same_file <- file.exists(dest) && identical(normalizePath(it$path, mustWork = FALSE), normalizePath(dest, mustWork = FALSE))
        ok <- if (same_file) TRUE else file.copy(it$path, dest, overwrite = TRUE)
        if (isTRUE(ok)) out[[length(out) + 1]] <- list(key = it$key, role = it$role, name = it$name, path = dest)
      }
    }
    # In this patient's folder for this day, delete files that were removed with the x button (only this app's own referral_ / imaging_ files)
    if (dir.exists(dest_dir)) {
      existing <- list.files(dest_dir, full.names = TRUE)
      mine <- existing[grepl("^(referral|imaging)_", basename(existing))]
      orphan <- setdiff(mine, attempted)
      if (length(orphan) > 0) unlink(orphan)
    }
    out
  }
  
  # ==================== Apply form data (shared by loading an old record and restoring a draft) ====================
  
  apply_form_data <- function(fd, restore_date = FALSE) {
    clear_form()
    
    vals <- BLANK_VALUES
    for (nm in names(BLANK_VALUES)) {
      v <- fd[[nm]]
      if (!is.null(v) && length(v) > 0) vals[[nm]] <- v
    }
    raw_id <- if (!is.null(fd$ext_id)) as.character(fd$ext_id[1]) else ""
    vals$ext_id_num <- gsub("^GC", "", raw_id, ignore.case = TRUE)
    set_form_values(vals)
    
    # Visit date: when loading an old record keep 'today' (otherwise generating would overwrite the old record); when restoring a draft, restore the date that was selected
    if (restore_date && !is.null(fd$visit_date) && nzchar(as.character(fd$visit_date[1]))) {
      d <- tryCatch(as.Date(as.character(fd$visit_date[1])), error = function(e) NA)
      if (!is.na(d)) updateDateInput(session, "visit_date", value = d)
    }
    
    
    # ROM / MMT / special tests / red flags
    ts <- fd$table_store
    if (is.list(ts)) {
      for (k in names(ts)) {
        v <- ts[[k]]
        if (!is.null(v) && length(v) > 0) form_store[[k]] <- as.character(v[1])
      }
    }
    set_pending_flags(fd$o_joint_select, fd$a_flags)
    
    # Attachments
    docs <- list()
    paths <- as.character(unlist(fd$attach_paths))
    if (length(paths) > 0) {
      keys <- as.character(unlist(fd$attach_keys)); roles <- as.character(unlist(fd$attach_roles)); nms <- as.character(unlist(fd$attach_names))
      for (i in seq_along(paths)) {
        if (file.exists(paths[i])) docs[[length(docs) + 1]] <- list(key = keys[i], role = roles[i], name = nms[i], path = paths[i], persisted = TRUE)
      }
    }
    loaded_docs(docs)
    
    form_epoch(form_epoch() + 1)
  }
  
  apply_record <- function(rec) {
    fd <- tryCatch(jsonlite::fromJSON(rec$form_data_json[1], simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(fd)) {
      showNotification("⚠️ This record's data could not be read.", type = "error")
      return()
    }
    apply_form_data(fd, restore_date = FALSE)
  }
  
  # ==================== Load the previous record (asks for confirmation first) ====================
  
  observeEvent(input$btn_load_last_visit, {
    if (isTRUE(input$trial_mode)) {
      showNotification("🔒 Loading previous records is turned off in trial mode.", type = "warning")
      return()
    }
    ic <- normalize_ic(input$ext_ic)
    if (!nzchar(ic)) {
      showNotification("⚠️ Please fill in the patient's IC / Passport number in Tab 1 first!", type = "warning")
      return()
    }
    rec <- get_latest_patient_visit_by_ic(ic)
    raw_ic <- trimws(input$ext_ic %||% "")
    if (nrow(rec) == 0 && nzchar(raw_ic) && raw_ic != ic) rec <- get_latest_patient_visit_by_ic(raw_ic)   # also try the older IC format
    
    if (nrow(rec) == 0) {
      showNotification(sprintf("⚠️ No history record found in database for IC [%s].", ic), type = "warning")
      return()
    }
    
    pending_record(rec[1, , drop = FALSE])
    showModal(modalDialog(
      title = "🔄 Load previous record?",
      p(sprintf("Found a record for %s (IC %s), dated %s.", rec$patient_name[1], rec$patient_ic[1], rec$visit_date[1])),
      p("Loading will replace everything currently entered in the form."),
      p("The Date of Assessment is reset to today, so generating afterwards saves a new visit. To amend that old record instead, change the date to its date."),
      easyClose = TRUE,
      footer = tagList(modalButton("Cancel"), actionButton("btn_confirm_load", "Load Record", class = "btn btn-success"))
    ))
  })
  
  observeEvent(input$btn_confirm_load, {
    rec <- pending_record()
    removeModal()
    req(rec)
    apply_record(rec)
    pending_record(NULL)
    showNotification(sprintf("✅ Loaded the record dated %s.", rec$visit_date[1]), type = "message")
  })
  
  # ==================== Tab 3: template manager ====================
  
  output$ui_tmpl_mode <- renderUI({
    id <- input$tmpl_edit_id
    if (!is.null(id) && length(id) == 1 && !is.na(id) && id > 0) {
      div(style = "background: #eff6ff; color: #1d4ed8; padding: 6px 10px; border-radius: 6px; font-size: 12px; margin-bottom: 12px;", paste0("✏️ Editing template #", id))
    } else {
      div(style = "background: #f0fdf4; color: #166534; padding: 6px 10px; border-radius: 6px; font-size: 12px; margin-bottom: 12px;", "➕ Creating a new template")
    }
  })
  
  clear_template_form <- function() {
    updateNumericInput(session, "tmpl_edit_id", value = 0)
    updateTextInput(session, "tmpl_title", value = "")
    updateTextAreaInput(session, "tmpl_content", value = "")
  }
  
  do_save_template <- function(type, title, content, edit_id) {
    if (edit_id > 0) {
      update_template_by_id(edit_id, type, title, content)
      showNotification("✅ Template updated successfully!", type = "message")
    } else {
      add_template(type, title, content)
      showNotification("✅ Template added successfully!", type = "message")
    }
    clear_template_form()
    tmpl_trigger(tmpl_trigger() + 1)
  }
  
  observeEvent(input$btn_save_template, {
    title <- trimws(input$tmpl_title %||% "")
    content <- trimws(input$tmpl_content %||% "")
    if (!nzchar(title) || !nzchar(content)) {
      showNotification("⚠️ Please fill in both the template title and content.", type = "warning")
      return()
    }
    edit_id <- input$tmpl_edit_id
    if (is.null(edit_id) || length(edit_id) != 1 || is.na(edit_id)) edit_id <- 0
    
    if (template_title_exists(input$tmpl_type, title, edit_id)) {
      pending_template(list(type = input$tmpl_type, title = title, content = content, edit_id = edit_id))
      showModal(modalDialog(
        title = "⚠️ Duplicate title",
        p(sprintf("A template titled \"%s\" already exists in this category.", title)),
        p("Save anyway, or go back and rename it?"),
        easyClose = TRUE,
        footer = tagList(modalButton("Go back"), actionButton("btn_confirm_dup_save", "Save Anyway", class = "btn btn-warning"))
      ))
      return()
    }
    do_save_template(input$tmpl_type, title, content, edit_id)
  })
  
  observeEvent(input$btn_confirm_dup_save, {
    pt <- pending_template()
    removeModal()
    req(pt)
    do_save_template(pt$type, pt$title, pt$content, pt$edit_id)
    pending_template(NULL)
  })
  
  observeEvent(input$btn_clear_template_form, clear_template_form())
  
  # Edit / delete: the buttons on the cards all report back through tmpl_action (no separate observer per button)
  observeEvent(input$tmpl_action, {
    a <- input$tmpl_action
    tm <- get_template_by_id(a$id)
    if (nrow(tm) == 0) {
      showNotification("⚠️ That template no longer exists.", type = "warning")
      tmpl_trigger(tmpl_trigger() + 1)
      return()
    }
    if (identical(a$type, "edit")) {
      updateNumericInput(session, "tmpl_edit_id", value = tm$id[1])
      updateSelectInput(session, "tmpl_type", selected = tm$type[1])
      updateTextInput(session, "tmpl_title", value = tm$title[1])
      updateTextAreaInput(session, "tmpl_content", value = tm$content[1])
    } else if (identical(a$type, "delete")) {
      pending_delete_id(tm$id[1])
      showModal(modalDialog(
        title = "🗑️ Delete template?",
        p(sprintf("\"%s\" will be permanently deleted.", tm$title[1])),
        easyClose = TRUE,
        footer = tagList(modalButton("Cancel"), actionButton("btn_confirm_delete_tmpl", "Delete", class = "btn btn-danger"))
      ))
    }
  })
  
  observeEvent(input$btn_confirm_delete_tmpl, {
    id <- pending_delete_id()
    removeModal()
    req(id)
    delete_template_by_id(id)
    cur <- input$tmpl_edit_id
    if (!is.null(cur) && length(cur) == 1 && !is.na(cur) && cur == id) clear_template_form()
    pending_delete_id(NULL)
    showNotification("🗑️ Template deleted.", type = "warning")
    tmpl_trigger(tmpl_trigger() + 1)
  })
  
  tmpl_search_d <- debounce(reactive(input$tmpl_search %||% ""), 250)
  
  filtered_templates <- function(type) {
    tm <- get_templates_by_type(type)
    total <- nrow(tm)
    q <- tolower(trimws(tmpl_search_d()))
    if (nzchar(q) && total > 0) {
      tm <- tm[grepl(q, tolower(paste(tm$title, tm$content)), fixed = TRUE), , drop = FALSE]
    }
    list(rows = tm, total = total)
  }
  
  render_template_cards <- function(res, badge_cls, type_label) {
    tmpls <- res$rows
    if (res$total == 0) {
      return(p("No templates available. Please add one using the form on the left.", style = "color: #94a3b8; font-style: italic; padding: 10px 0;"))
    }
    if (nrow(tmpls) == 0) {
      return(p("No templates match your search.", style = "color: #94a3b8; font-style: italic; padding: 10px 0;"))
    }
    cards <- lapply(seq_len(nrow(tmpls)), function(i) {
      row <- tmpls[i, ]
      id <- as.integer(row$id)
      div(class = "sub-box", style = "background: #ffffff; border-left: 4px solid #198754; margin-bottom: 15px;",
          div(class = "d-flex justify-content-between align-items-center mb-1",
              div(span(class = paste("section-badge", badge_cls), type_label), strong(row$title, style = "margin-left: 8px; font-size: 14px; color: #1e293b;")),
              div(
                tags$button(type = "button", class = "btn btn-outline-primary btn-xs", style = "padding: 1px 6px; font-size: 11px; margin-right: 4px;",
                            onclick = sprintf("Shiny.setInputValue('tmpl_action', {type: 'edit', id: %d, n: Math.random()}, {priority: 'event'});", id), "Edit"),
                tags$button(type = "button", class = "btn btn-outline-danger btn-xs", style = "padding: 1px 6px; font-size: 11px;",
                            onclick = sprintf("Shiny.setInputValue('tmpl_action', {type: 'delete', id: %d, n: Math.random()}, {priority: 'event'});", id), "Delete")
              )
          ),
          tags$pre(row$content, style = "white-space: pre-wrap; word-break: break-word; font-size: 12px; background: #f8fafc; padding: 10px; border-radius: 4px; margin: 5px 0 0 0; color: #334155;")
      )
    })
    tagList(
      div(style = "font-size: 11px; color: #94a3b8; margin-bottom: 8px;", sprintf("Showing %d of %d", nrow(tmpls), res$total)),
      cards
    )
  }
  
  output$ui_treatment_templates_list <- renderUI({
    tmpl_trigger()
    render_template_cards(filtered_templates("treatment"), "bg-p", "Treatment Plan")
  })
  
  output$ui_hep_templates_list <- renderUI({
    tmpl_trigger()
    render_template_cards(filtered_templates("hep"), "bg-s", "HEP")
  })
  
  # ---- Applying a template from Tab 2: picker dialog (reads the database every time it opens, so newly added templates work immediately) ----
  open_template_picker <- function(type, title, empty_msg) {
    tmpls <- get_templates_by_type(type)
    if (nrow(tmpls) == 0) {
      showModal(modalDialog(title = title, p(empty_msg), easyClose = TRUE, footer = modalButton("Close")))
      return()
    }
    btn_list <- lapply(seq_len(nrow(tmpls)), function(i) {
      row <- tmpls[i, ]
      tags$button(type = "button", class = "tmpl-select-item",
                  style = "width: 100%; text-align: left; background: #f8fafc; border: 1px solid #cbd5e1; margin-bottom: 8px; padding: 10px 14px;",
                  onclick = sprintf("Shiny.setInputValue('tmpl_pick', {id: %d, n: Math.random()}, {priority: 'event'});", as.integer(row$id)),
                  div(class = "tmpl-select-title", row$title))
    })
    showModal(modalDialog(title = "Click a Template to Populate", div(style = "max-height: 400px; overflow-y: auto;", btn_list), easyClose = TRUE, footer = modalButton("Cancel")))
  }
  
  observeEvent(input$btn_apply_treatment_tmpl, open_template_picker("treatment", "📋 Apply Treatment Plan Template", "No Treatment Plan templates found."))
  observeEvent(input$btn_apply_hep_tmpl, open_template_picker("hep", "🏠 Apply HEP Template", "No HEP templates found."))
  
  observeEvent(input$tmpl_pick, {
    tm <- get_template_by_id(input$tmpl_pick$id)
    if (nrow(tm) == 0) return()
    if (identical(tm$type[1], "treatment")) {
      updateTextAreaInput(session, "p_treatment_plan", value = tm$content[1])
    } else {
      updateTextAreaInput(session, "r_hep", value = tm$content[1])
    }
    removeModal()
    showNotification(sprintf("✅ Template successfully applied: %s", tm$title[1]), type = "message")
  })
  
  # ==================== Tab 2: red flags / ROM / MMT / special tests (several regions can be assessed at once) ====================
  
  joints <- reactive({
    j <- input$o_joint_select
    if (is.null(j)) character(0) else as.character(j[!is.na(j) & nzchar(j)])
  })
  
  joint_hint <- function(msg) {
    div(style = "padding: 10px; border: 1px dashed #cbd5e1; border-radius: 6px; color: #94a3b8; font-size: 12px; margin-bottom: 10px; background: #fff;", msg)
  }
  
  region_head <- function(joint) {
    div(style = "font-weight: 700; color: #92400e; background: #fef3c7; padding: 4px 10px; border-radius: 4px; margin: 8px 0 6px 0; font-size: 12px;", paste0("📍 ", joint))
  }
  
  # When several regions are assessed: the heading rows that separate regions inside the report tables
  region_row_html <- function(joint, colspan) {
    sprintf("<tr style='page-break-after: avoid; break-after: avoid;'><td colspan='%d' style='padding: 5px 10px; background: #fef3c7; color: #92400e; font-weight: 700; border: 1px solid #cbd5e1;'>📍 %s</td></tr>", colspan, htmltools::htmlEscape(joint))
  }
  
  store_get <- function(kind, joint, i, default = "") {
    v <- isolate(form_store[[store_key(kind, joint, i)]])
    if (is.null(v) || length(v) == 0) default else v
  }
  
  input_or_store <- function(id, kind, joint, i, default = "") {
    v <- input[[id]]
    if (is.null(v)) v <- isolate(form_store[[store_key(kind, joint, i)]])
    if (is.null(v) || length(v) == 0) default else v
  }
  
  # Remember the values entered under every selected region: deselect a region and select it again and its data is still there
  observe({
    for (joint in joints()) {
      slug <- joint_slug(joint)
      put <- function(kind, id, i) {
        v <- input[[id]]
        if (!is.null(v)) {
          k <- store_key(kind, joint, i)
          if (!identical(isolate(form_store[[k]]), v)) form_store[[k]] <- v
        }
      }
      for (i in seq_along(get_movements_for_joint(joint))) {
        put("rom_arom", paste0("rom_arom_", slug, "_", i), i)
        put("rom_prom", paste0("rom_prom_", slug, "_", i), i)
        put("rom_remark", paste0("rom_remark_", slug, "_", i), i)
      }
      for (i in seq_along(get_mmt_groups_for_joint(joint))) put("mmt", paste0("mmt_grp_", slug, "_", i), i)
      for (i in seq_along(get_special_tests_for_joint(joint))) put("st", paste0("st_res_", slug, "_", i), i)
    }
    so <- input$st_other
    if (!is.null(so) && !identical(isolate(form_store[["st_other"]]), so)) form_store[["st_other"]] <- so
  })
  
  flags_for_joint <- function(joint) {
    switch(joint,
           "Lumbar Spine" = c("Cauda Equina Syndrome (Saddle numbness / Incontinence)", "Progressive motor deficit / Foot drop"),
           "Cervical Spine" = c("Cervical Myelopathy (Clumsy hands / Gait instability)", "VBI Signs / 5D's 3N's (Dizziness / Diplopia / Dysphagia)"),
           "Shoulder Joint" = c("Visceral Referred Pain (Chest pressure / Gallbladder referral)"),
           "Thoracic Spine / Scoliosis" = c("Visceral Referred Pain (Chest pressure / Gallbladder referral)"),
           "Knee Joint" = c("Inability to bear weight", "DVT Signs (Severe calf pain / Swelling / Heat)"),
           "Hip Joint" = c("Inability to bear weight", "DVT Signs (Severe calf pain / Swelling / Heat)"),
           "Ankle Joint" = c("Inability to bear weight", "DVT Signs (Severe calf pain / Swelling / Heat)"),
           c("Local severe unexplained joint swelling")
    )
  }
  
  output$ui_dynamic_flags <- renderUI({
    form_epoch()
    js <- joints()
    common_flags <- c("Unexplained weight loss / Cancer history", "Night pain / Constant non-mechanical pain", "Recent major trauma / Suspected fracture", "Fever / Unexplained infection")
    specific_flags <- if (length(js) == 0) c("Local severe unexplained joint swelling") else unlist(lapply(js, flags_for_joint))
    all_choices <- unique(c(common_flags, specific_flags))
    
    pend <- isolate(pending_flags())
    if (!is.null(pend)) {
      sel <- pend$flags
      if (identical(pend$joints, sort(js))) isolate(pending_flags(NULL))   # regions have finished switching; used up, so clear it
    } else {
      sel <- isolate(input$a_flags)
    }
    checkboxGroupInput("a_flags", label = "🚩 Safety Red Flag Checklist:", choices = all_choices, selected = intersect(sel, all_choices))
  })
  
  output$ui_rom_table <- renderUI({
    form_epoch()
    js <- joints()
    if (length(js) == 0) return(joint_hint("Select one or more Target Joints / Regions above to record ROM."))
    multi <- length(js) > 1
    tagList(lapply(js, function(joint) {
      slug <- joint_slug(joint)
      movements <- get_movements_for_joint(joint)
      table_rows <- lapply(seq_along(movements), function(i) {
        tags$tr(
          tags$td(style = "font-size: 12px; font-weight: bold; width: 30%; vertical-align: middle;", movements[i]),
          tags$td(style = "width: 22%;", textInput(paste0("rom_arom_", slug, "_", i), label = NULL, value = store_get("rom_arom", joint, i), width = "100%")),
          tags$td(style = "width: 22%;", textInput(paste0("rom_prom_", slug, "_", i), label = NULL, value = store_get("rom_prom", joint, i), width = "100%")),
          tags$td(style = "width: 26%;", textInput(paste0("rom_remark_", slug, "_", i), label = NULL, value = store_get("rom_remark", joint, i), width = "100%"))
        )
      })
      tagList(
        if (multi) region_head(joint),
        tags$table(class = "table table-sm table-bordered table-rom", style = "margin-bottom: 10px; background: #fff;",
                   tags$thead(style = "background: #f8f9fa; font-size: 11px;", tags$tr(tags$th("Direction"), tags$th("AROM (°)"), tags$th("PROM (°)"), tags$th("Remarks"))),
                   tags$tbody(table_rows)
        )
      )
    }))
  })
  
  get_rom_dataframe <- reactive({
    js <- joints()
    if (length(js) == 0) return(data.frame())
    do.call(rbind, lapply(js, function(joint) {
      slug <- joint_slug(joint)
      movements <- get_movements_for_joint(joint)
      do.call(rbind, lapply(seq_along(movements), function(i) {
        data.frame(
          Region = joint,
          Movement = movements[i],
          AROM = as.character(input_or_store(paste0("rom_arom_", slug, "_", i), "rom_arom", joint, i)),
          PROM = as.character(input_or_store(paste0("rom_prom_", slug, "_", i), "rom_prom", joint, i)),
          Remarks = as.character(input_or_store(paste0("rom_remark_", slug, "_", i), "rom_remark", joint, i)),
          stringsAsFactors = FALSE
        )
      }))
    }))
  })
  
  output$ui_mmt_group_table <- renderUI({
    form_epoch()
    js <- joints()
    if (length(js) == 0) return(joint_hint("Select one or more Target Joints / Regions above to record muscle strength."))
    multi <- length(js) > 1
    mmt_choices <- c("5/5 (Normal)" = "5/5", "4+/5" = "4+/5", "4/5 (Good)" = "4/5", "4-/5" = "4-/5", "3+/5" = "3+/5", "3/5 (Fair)" = "3/5", "3-/5" = "3-/5", "2/5 (Poor)" = "2/5", "1/5 (Trace)" = "1/5", "0/5 (Zero)" = "0/5", "N/T (Not Tested)" = "N/T")
    tagList(lapply(js, function(joint) {
      slug <- joint_slug(joint)
      groups <- get_mmt_groups_for_joint(joint)
      table_rows <- lapply(seq_along(groups), function(i) {
        tags$tr(
          tags$td(style = "font-size: 12px; font-weight: bold; width: 45%; vertical-align: middle;", groups[i]),
          tags$td(style = "width: 55%; vertical-align: middle;", selectInput(paste0("mmt_grp_", slug, "_", i), label = NULL, choices = mmt_choices, selected = store_get("mmt", joint, i, "N/T"), width = "100%"))
        )
      })
      tagList(
        if (multi) region_head(joint),
        tags$table(class = "table table-sm table-bordered table-mmt", style = "margin-bottom: 10px; background: #fff;",
                   tags$thead(style = "background: #f8f9fa; font-size: 11px;", tags$tr(tags$th("Muscle Group / Myotome"), tags$th("Grade (0-5)"))),
                   tags$tbody(table_rows)
        )
      )
    }))
  })
  
  get_mmt_summary_table_html <- reactive({
    js <- joints()
    if (length(js) == 0) return("")
    multi <- length(js) > 1
    rows <- c()
    for (joint in js) {
      slug <- joint_slug(joint)
      groups <- get_mmt_groups_for_joint(joint)
      if (multi) rows <- c(rows, region_row_html(joint, 2))
      for (i in seq_along(groups)) {
        val <- input_or_store(paste0("mmt_grp_", slug, "_", i), "mmt", joint, i, "N/T")
        rows <- c(rows, sprintf("<tr><td style='padding: 6px 10px; border: 1px solid #cbd5e1; font-weight: 600;'>%s</td><td style='padding: 6px 10px; border: 1px solid #cbd5e1; text-align: center; color: #0284c7; font-weight: bold;'>%s</td></tr>", groups[i], htmltools::htmlEscape(val)))
      }
    }
    paste(rows, collapse = "")
  })
  
  output$ui_special_tests <- renderUI({
    form_epoch()
    js <- joints()
    heading <- tags$label("4. Special Tests", style = "font-size: 13px; font-weight: bold; color: #92400e; margin-top: 12px; display:block;")
    if (length(js) == 0) {
      return(tagList(heading, joint_hint("Select one or more Target Joints / Regions above to record special tests.")))
    }
    multi <- length(js) > 1
    blocks <- lapply(js, function(joint) {
      slug <- joint_slug(joint)
      tests <- get_special_tests_for_joint(joint)
      test_rows <- lapply(seq_along(tests), function(i) {
        tags$tr(
          tags$td(style = "font-size: 12px; font-weight: 600; width: 40%; vertical-align: middle;", tests[i]),
          tags$td(style = "width: 60%; vertical-align: middle; text-align: center;",
                  div(class = "st-radio-group",
                      radioButtons(inputId = paste0("st_res_", slug, "_", i), label = NULL, choices = c("Not Done" = "ND", "Negative (-)" = "Neg", "Positive (+)" = "Pos"), selected = store_get("st", joint, i, "ND"), inline = TRUE)
                  )
          )
        )
      })
      tagList(
        if (multi) region_head(joint),
        tags$table(class = "table table-sm table-bordered", style = "margin-bottom: 8px; background: #ffffff;",
                   tags$thead(style = "background: #f8f9fa; font-size: 11px;", tags$tr(tags$th(style = "width: 40%;", "Special Test Name"), tags$th(style = "width: 60%; text-align: center;", "Result"))),
                   tags$tbody(test_rows)
        )
      )
    })
    other_prev <- isolate(form_store[["st_other"]])
    tagList(
      heading,
      blocks,
      textInput("st_other", "Other Special Tests / Custom Remarks:", value = other_prev %||% "", placeholder = "e.g., Slump (+), SLR 45° (+)", width = "100%")
    )
  })
  
  get_special_tests_table_html <- reactive({
    js <- joints()
    if (length(js) == 0) return("")
    multi <- length(js) > 1
    rows <- c()
    for (joint in js) {
      slug <- joint_slug(joint)
      tests <- get_special_tests_for_joint(joint)
      if (multi) rows <- c(rows, region_row_html(joint, 2))
      for (i in seq_along(tests)) {
        val <- input_or_store(paste0("st_res_", slug, "_", i), "st", joint, i, "ND")
        res_display <- switch(val,
                              "Pos" = "<span style='color: #dc2626; font-weight: bold;'>POSITIVE (+)</span>",
                              "Neg" = "<span style='color: #16a34a; font-weight: bold;'>NEGATIVE (-)</span>",
                              "<span style='color: #94a3b8;'>Not Done</span>"
        )
        rows <- c(rows, sprintf("<tr><td style='padding: 6px 10px; border: 1px solid #cbd5e1; font-weight: 600;'>%s</td><td style='padding: 6px 10px; border: 1px solid #cbd5e1; text-align: center;'>%s</td></tr>", tests[i], res_display))
      }
    }
    other_val <- input$st_other
    if (is.null(other_val)) other_val <- isolate(form_store[["st_other"]])
    if (!is.null(other_val) && nchar(trimws(other_val)) > 0) {
      rows <- c(rows, sprintf("<tr><td style='padding: 6px 10px; border: 1px solid #cbd5e1; font-weight: 600;'>Other: %s</td><td style='padding: 6px 10px; border: 1px solid #cbd5e1; text-align: center; color: #0284c7;'>Custom Note</td></tr>", htmltools::htmlEscape(trimws(other_val))))
    }
    paste(rows, collapse = "")
  })
  
  # ==================== Collect form content (shared by Generate and draft autosave) ====================
  
  build_form_data <- function(attach = list(), p = NULL) {
    other_or <- function(val, other_txt) {
      if (!is.null(val) && length(val) == 1 && val == "Other" && !is.null(other_txt) && nchar(trimws(other_txt)) > 0) paste0("Other: ", trimws(other_txt)) else val
    }
    name <- if (!is.null(p)) p$name else trimws(input$ext_name %||% "")
    ic <- if (!is.null(p)) p$ic else (input$ext_ic %||% "")
    vd <- if (!is.null(p)) p$date else input$visit_date
    vd_txt <- if (is.null(vd) || length(vd) != 1 || is.na(vd)) "" else as.character(vd)
    list(
      ext_name = name, ext_ic = ic, ext_id = get_full_patient_id(), visit_date = vd_txt, ext_dx = input$ext_dx, ext_findings = input$ext_findings,
      s_chief_complaints = input$s_chief_complaints, s_past_med = input$s_past_med, s_personal_hx = input$s_personal_hx, s_family_hx = input$s_family_hx, s_hpi = input$s_hpi,
      sym_side = input$sym_side, sym_site = input$sym_site, sym_onset = input$sym_onset, sym_duration = input$sym_duration, sym_type = input$sym_type, sym_vas = input$sym_vas,
      sym_agg = other_or(input$sym_agg, input$sym_agg_other_text), sym_rel = other_or(input$sym_rel, input$sym_rel_other_text),
      sym_24h = other_or(input$sym_24h, input$sym_24h_other_text), sym_prev_tx = other_or(input$sym_prev_tx, input$sym_prev_tx_other_text), sym_func_impact = input$sym_func_impact,
      o_joint_select = joints(), obs_general = input$obs_general, obs_local = input$obs_local, obs_palpation = input$obs_palpation, joint_integrity = input$joint_integrity, meas_assess = input$meas_assess,
      bal_sitting = input$bal_sitting, bal_standing = input$bal_standing, bal_sls = input$bal_sls, bal_tandem = input$bal_tandem,
      func_bed = input$func_bed, func_transfer = input$func_transfer, func_sts = input$func_sts, func_gait = input$func_gait,
      a_flags = input$a_flags,
      a_pt_impression = input$a_pt_impression, a_impression = input$a_impression, a_precautions = input$a_precautions, p_treatment_plan = input$p_treatment_plan, e_vas_post = input$e_vas_post, r_hep = input$r_hep, r_next_plan = input$r_next_plan, r_next_schedule = input$r_next_schedule,
      table_store = Filter(Negate(is.null), reactiveValuesToList(form_store)),
      attach_keys = vapply(attach, function(d) d$key, character(1)),
      attach_roles = vapply(attach, function(d) d$role, character(1)),
      attach_names = vapply(attach, function(d) d$name, character(1)),
      attach_paths = vapply(attach, function(d) d$path, character(1))
    )
  }
  
  # ==================== Generate report ====================
  
  observeEvent(input$btn_generate_note, {
    trial <- isTRUE(input$trial_mode)
    if (trial) {
      # Trial mode: no patient details are required (blank fields stay blank), and nothing is saved
      vd <- input$visit_date
      vd <- if (is.null(vd) || length(vd) != 1 || is.na(vd) || as.Date(vd) > Sys.Date()) Sys.Date() else as.Date(vd)
      nm <- trimws(input$ext_name %||% "")
      ic_typed <- normalize_ic(input$ext_ic)
      p <- list(name = nm, ic = ic_typed, date = vd)
    } else {
      p <- validate_patient(go_to_tab1 = TRUE)
      if (is.null(p)) return()
    }
    
    full_ext_id <- get_full_patient_id()
    
    doc <- list(
      patient_name = p$name,
      patient_id = full_ext_id,
      patient_ic = p$ic,
      visit_date = p$date,
      trial = trial,
      primary_dx = input$ext_dx,
      radiology_findings = input$ext_findings
    )
    
    rom_df <- get_rom_dataframe()
    mmt_html <- get_mmt_summary_table_html()
    st_html <- get_special_tests_table_html()
    
    # Save the files permanently first (and delete the ones removed with the x button); write their paths into the record
    saved_docs <- list()
    if (!trial) {
      n_items <- length(doc_items())
      saved_docs <- tryCatch(persist_documents(p$ic, p$date), error = function(e) {
        showNotification(paste("⚠️ Could not save the uploaded files:", conditionMessage(e)), type = "warning", duration = 10)
        list()
      })
      loaded_docs(lapply(saved_docs, function(d) c(d, list(persisted = TRUE))))
      removed_saved(character(0))
      if (n_items > 0 && length(saved_docs) == n_items) {
        clear_staged()   # everything is saved: the list now shows the saved copies only
      }
    }
    
    raw_form_inputs <- build_form_data(saved_docs, p)
    
    fd <- raw_form_inputs
    eval_form_data <- list(
      chief_complaints = fd$s_chief_complaints, past_med = fd$s_past_med, personal_hx = fd$s_personal_hx, family_hx = fd$s_family_hx, s_hpi = fd$s_hpi,
      sym_side = fd$sym_side, sym_site = fd$sym_site, sym_onset = fd$sym_onset, sym_duration = fd$sym_duration, sym_type = fd$sym_type, sym_vas = fd$sym_vas, sym_agg = fd$sym_agg, sym_rel = fd$sym_rel,
      sym_24h = fd$sym_24h, sym_prev_tx = fd$sym_prev_tx, sym_func_impact = fd$sym_func_impact,
      obs_general = fd$obs_general, obs_local = fd$obs_local, obs_palpation = fd$obs_palpation,
      joint = fd$o_joint_select, rom_df = rom_df, mmt_table_html = mmt_html, special_tests_table_html = st_html, joint_integrity = fd$joint_integrity,
      bal_sitting = fd$bal_sitting, bal_standing = fd$bal_standing, bal_sls = fd$bal_sls, bal_tandem = fd$bal_tandem,
      func_bed = fd$func_bed, func_transfer = fd$func_transfer, func_sts = fd$func_sts, func_gait = fd$func_gait,
      measurement = fd$meas_assess,
      pt_impression = fd$a_pt_impression, impression = fd$a_impression, flags = if (length(fd$a_flags) > 0) paste(fd$a_flags, collapse = ", ") else "None", precautions = fd$a_precautions,
      treatment_plan = fd$p_treatment_plan, vas_post = fd$e_vas_post, hep = fd$r_hep, next_plan = fd$r_next_plan, next_schedule = fd$r_next_schedule
    )
    
    note <- synthesize_full_evaluation_form(doc, eval_form_data)
    current_report_text(note)
    
    if (trial) {
      showNotification("🧪 Report generated (trial mode: not saved).", type = "message")
      return()
    }
    
    save_status <- save_patient_visit(
      patient_ic = p$ic,
      patient_id = full_ext_id,
      patient_name = p$name,
      chief_complaints = input$s_chief_complaints,
      form_data = raw_form_inputs,
      note = note,
      visit_date = p$date
    )
    
    # Now properly saved, so the local draft is no longer needed
    last_generated(raw_form_inputs)
    session$sendCustomMessage("draft_clear", list())
    
    files_msg <- if (length(saved_docs) > 0) sprintf(" %d document(s) saved with the record.", length(saved_docs)) else ""
    showNotification(
      paste0(
        if (identical(save_status, "updated")) sprintf("✅ Report generated. The record for %s was updated.", format(p$date, "%d/%m/%Y")) else sprintf("✅ Report generated and saved as a new record (%s).", format(p$date, "%d/%m/%Y")),
        files_msg),
      type = "message"
    )
  })
  
  output$soaper_report_display <- renderUI({
    txt <- current_report_text()
    if (nchar(txt) == 0) {
      tags$div(style = "color: #64748b; font-style: italic;", "Click '⚡ Generate / Update SOAPER Note' on the left to synthesize the medical report.")
    } else {
      tags$div(class = "soap-report-view", HTML(txt))
    }
  })
  
  observeEvent(input$btn_copy, {
    txt <- current_report_text()
    if (nchar(txt) == 0) {
      showNotification("⚠️ Please click Generate / Update SOAPER Note first.", type = "warning")
      return()
    }
    session$sendCustomMessage("copy_to_clipboard", txt)
  })
  
  # ==================== Draft autosave (kept in this browser; can be restored after a refresh / disconnect / timeout) ====================
  
  draft_snapshot <- reactive({
    saved <- Filter(function(d) !(d$key %in% removed_saved()), loaded_docs())   # Only saved attachments are included; uploaded files that were never saved cannot be restored across sessions
    build_form_data(saved)
  })
  draft_debounced <- debounce(draft_snapshot, 1500)
  
  observeEvent(draft_debounced(), {
    if (isTRUE(input$trial_mode)) return()    # Trial mode: nothing is kept in this browser either
    if (!isTRUE(draft_armed())) return()      # Until the 'restore the old draft?' question has been answered, the local draft must never be overwritten
    fd <- draft_debounced()
    if (form_is_blank(fd) || identical(fd, last_generated())) {
      session$sendCustomMessage("draft_clear", list())
      return()
    }
    payload <- jsonlite::toJSON(
      list(saved_at = format(Sys.time(), "%d/%m/%Y %H:%M"), saved_ts = as.numeric(Sys.time()), form = fd),
      auto_unbox = TRUE, null = "null")
    session$sendCustomMessage("draft_save", as.character(payload))
  })
  
  # As soon as the page connects to the server, the browser hands over the local draft
  observeEvent(input$draft_probe, {
    pr <- input$draft_probe
    if (!isTRUE(pr$found) || !nzchar(pr$data %||% "")) {
      draft_armed(TRUE)
      return()
    }
    dr <- tryCatch(jsonlite::fromJSON(pr$data, simplifyVector = TRUE), error = function(e) NULL)
    age_h <- if (is.null(dr$saved_ts)) Inf else (as.numeric(Sys.time()) - as.numeric(dr$saved_ts)) / 3600
    if (is.null(dr) || is.null(dr$form) || !is.finite(age_h) || age_h > 72 || form_is_blank(dr$form)) {
      session$sendCustomMessage("draft_clear", list())    # Draft too old or empty: simply discard it
      draft_armed(TRUE)
      return()
    }
    pending_draft(dr)
    who <- trimws(as.character(dr$form$ext_name %||% ""))
    showModal(modalDialog(
      title = "📝 Restore unsaved draft?",
      p(sprintf("An unsaved draft was found on this computer (last edited %s%s).", dr$saved_at %||% "?", if (nzchar(who)) paste0(", patient: ", who) else "")),
      p("Files that were uploaded but not yet saved with a report cannot be restored and must be uploaded again."),
      easyClose = FALSE,
      footer = tagList(
        actionButton("btn_discard_draft", "Discard draft", class = "btn btn-outline-danger"),
        actionButton("btn_restore_draft", "Restore draft", class = "btn btn-success")
      )
    ))
  })
  
  observeEvent(input$btn_restore_draft, {
    dr <- pending_draft()
    removeModal()
    req(dr)
    apply_form_data(dr$form, restore_date = TRUE)
    pending_draft(NULL)
    draft_armed(TRUE)
    showNotification("📝 Draft restored.", type = "message")
  })
  
  observeEvent(input$btn_discard_draft, {
    removeModal()
    pending_draft(NULL)
    session$sendCustomMessage("draft_clear", list())
    draft_armed(TRUE)
  })
}

shinyApp(ui, server)