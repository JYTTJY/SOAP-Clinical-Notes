# logic.R - Database & Form Note Synthesizer
library(DBI)
library(RSQLite)
library(jsonlite)

# Initialize SQLite Database (contains patient visit records & template management tables)
init_sqlite_db <- function(db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  
  # Patient evaluation visit records table
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
  
  # Template management table
  dbExecute(conn, "
    CREATE TABLE IF NOT EXISTS templates_v1 (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      type TEXT,         -- 'treatment' or 'hep'
      title TEXT,        -- Template title/name
      content TEXT       -- Template detailed content
    )
  ")
  
  # If template table is empty, seed with default initial templates
  template_count <- dbGetQuery(conn, "SELECT COUNT(*) as count FROM templates_v1")$count[1]
  if (template_count == 0) {
    dbExecute(conn, "
      INSERT INTO templates_v1 (type, title, content) VALUES
      ('treatment', 'Lumbar Radiculopathy Standard Care', '- Manual Therapy: L4-L5 Joint Mobilization Grade II-III (3 sets x 60s)\n- Electrotherapy: TENS on lumbar paraspinals 20 mins\n- Traction: Mechanical Lumbar Traction (1/3 BW, 15 mins)\n- Exercise: Lumbar extension in prone (3 sets x 10 reps)'),
      ('treatment', 'Shoulder Impingement Protocol', '- Manual Therapy: Glenohumeral posterior glide Grade II (3 sets x 60s)\n- Exercise Therapy: Rotator cuff isometric + Scapular setting (3 sets x 10 reps)\n- Modality: Ice pack 15 mins post-treatment'),
      ('hep', 'Lumbar Spine Extension & Core Control', '1. Prone Press-ups: 10 reps, hold 5s, 3 sets daily.\n2. Bird-Dog exercise: 10 reps/side, hold 3s.\n3. Avoid prolonged sitting (>30 mins) and forward bending.'),
      ('hep', 'Shoulder Rotator Cuff & Scapular Rehab', '1. Pendulum exercises: 2 mins clock-wise and counter-clockwise.\n2. Isometric External Rotation against wall: 10s hold x 10 reps, 2 sets.\n3. Scapular Retraction (Squeeze shoulder blades): 5s hold x 15 reps.')
    ")
  }
  
  dbDisconnect(conn)
}

# Save current evaluation record to SQLite database
save_patient_visit <- function(patient_ic, patient_id, patient_name, chief_complaints, form_data, note, db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  
  json_data <- jsonlite::toJSON(form_data, auto_unbox = TRUE)
  visit_date <- as.character(Sys.Date())
  
  query <- "INSERT INTO patient_visits_v6 (patient_ic, patient_id, patient_name, visit_date, chief_complaints, form_data_json, soaper_note) VALUES (?, ?, ?, ?, ?, ?, ?)"
  dbExecute(conn, query, params = list(patient_ic, patient_id, patient_name, visit_date, chief_complaints, json_data, note))
  
  dbDisconnect(conn)
}

# Fetch the latest record for a patient based on IC / Passport number
get_latest_patient_visit_by_ic <- function(patient_ic, db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  query <- "SELECT * FROM patient_visits_v6 WHERE patient_ic = ? ORDER BY id DESC LIMIT 1"
  res <- dbGetQuery(conn, query, params = list(patient_ic))
  dbDisconnect(conn)
  return(res)
}

# ----------------- Template Database Functions -----------------

# Fetch templates by type
get_templates_by_type <- function(template_type, db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  query <- "SELECT * FROM templates_v1 WHERE type = ? ORDER BY id DESC"
  res <- dbGetQuery(conn, query, params = list(template_type))
  dbDisconnect(conn)
  return(res)
}

# Fetch all templates
get_all_templates <- function(db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  res <- dbGetQuery(conn, "SELECT * FROM templates_v1 ORDER BY type, id DESC")
  dbDisconnect(conn)
  return(res)
}

# Add new template
add_template <- function(type, title, content, db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  query <- "INSERT INTO templates_v1 (type, title, content) VALUES (?, ?, ?)"
  dbExecute(conn, query, params = list(type, title, content))
  dbDisconnect(conn)
}

# Update template by ID
update_template_by_id <- function(id, type, title, content, db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  query <- "UPDATE templates_v1 SET type = ?, title = ?, content = ? WHERE id = ?"
  dbExecute(conn, query, params = list(type, title, content, id))
  dbDisconnect(conn)
}

# Delete template by ID
delete_template_by_id <- function(id, db_path = "rehab_records.db") {
  conn <- dbConnect(RSQLite::SQLite(), db_path)
  query <- "DELETE FROM templates_v1 WHERE id = ?"
  dbExecute(conn, query, params = list(id))
  dbDisconnect(conn)
}

# Synthesize full SOAPER evaluation note
synthesize_full_evaluation_form <- function(doc, f) {
  rom_str <- ""
  if (!is.null(f$rom_df) && nrow(f$rom_df) > 0) {
    rom_lines <- apply(f$rom_df, 1, function(row) {
      sprintf("  │ %-14s │ AROM: %-5s° │ PROM: %-5s° │ %-20s │", 
              row["Movement"], row["AROM"], row["PROM"], row["Remarks"])
    })
    rom_str <- paste(rom_lines, collapse = "\n")
  } else {
    rom_str <- "  │ No ROM data recorded."
  }
  
  # Handle null value fallback
  pre_vas <- if(!is.null(f$sym_vas)) as.numeric(f$sym_vas) else 0
  post_vas <- if(!is.null(f$vas_post)) as.numeric(f$vas_post) else 0
  vas_delta <- pre_vas - post_vas
  
  pt_impression_text <- if(!is.null(f$pt_impression)) f$pt_impression else "Not Recorded"
  treatment_plan_text <- if(!is.null(f$treatment_plan)) f$treatment_plan else "Not Specified"
  hep_text <- if(!is.null(f$hep)) f$hep else "None"
  next_sched_text <- if(!is.null(f$next_schedule)) f$next_schedule else "As needed"
  next_plan_text <- if(!is.null(f$next_plan)) f$next_plan else "Review progress next session."
  
  sprintf(
    "================================================================================
MUSCULOSKELETAL PHYSIOTHERAPY EVALUATION FORM
Green Care Physiotherapy | Date: %s
================================================================================
[PATIENT DEMOGRAPHICS & DIAGNOSIS]
- Patient Name & ID : %s
- Primary Diagnosis : %s
- Imaging / MRI     : %s

--------------------------------------------------------------------------------
I. SUBJECTIVE HISTORY
- Chief Complaints : %s
- Past Medical hx  : %s
- Personal hx       : %s
- Family hx         : %s

[Symptoms hx]
- Side    : %-15s | Site     : %s
- Onset   : %-15s | Duration : %s
- Type    : %-15s | VAS Pain : %d / 10
- Aggravating Factor: %s
- Relieving Factor  : %s

--------------------------------------------------------------------------------
II. OBJECTIVE EXAMINATION
[Observation & Palpation]
- General Observation: %s
- Local Observation  : %s
- Palpation          : %s

[Musculoskeletal Examination Table - Target Joint: %s]
┌──────────────────────────────────────────────────────────────────────────────┐
│ Range of Motion (ROM)                                                        │
%s
├──────────────────────────────────────────────────────────────────────────────┤
│ Muscle Strength Testing (MMT) : %s
│ Joint Integrity / Stability   : %s
│ Special Tests                 : %s
│ Balance Assessment            : %s
│ Functional Assessment         : %s
│ Special Measurements          : %s
└──────────────────────────────────────────────────────────────────────────────┘

--------------------------------------------------------------------------------
III. CLINICAL ASSESSMENT & IMPRESSION
- PT Clinical Impression: %s
- Progress Status       : %s
- Safety Flags          : %s
- Precautions           : %s

--------------------------------------------------------------------------------
IV. PLAN OF CARE & TREATMENT EXECUTED
%s

--------------------------------------------------------------------------------
V. EVALUATION & HOME PLAN
- Post-Tx VAS Pain    : %d / 10 (Pain Reduction Delta: -%d)
- Home Exercise (HEP) : %s
- Follow-Up Schedule  : %s
- Next Session Plan   : %s
================================================================================",
    Sys.Date(),
    doc$patient_id, doc$primary_dx, doc$radiology_findings,
    f$chief_complaints, f$past_med, f$personal_hx, f$family_hx,
    f$sym_side, f$sym_site,
    f$sym_onset, f$sym_duration,
    f$sym_type, pre_vas,
    f$sym_agg, f$sym_rel,
    f$obs_general, f$obs_local, f$obs_palpation,
    f$joint,
    rom_str,
    f$mmt,
    f$joint_integrity,
    f$special_tests,
    f$balance,
    f$functional,
    f$measurement,
    pt_impression_text, f$impression, f$flags, f$precautions,
    treatment_plan_text,
    post_vas, vas_delta,
    hep_text,
    next_sched_text,
    next_plan_text
  )
}