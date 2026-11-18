# SOAP — Musculoskeletal Physiotherapy Evaluation & Documentation System

![R](https://img.shields.io/badge/R-4.x-276DC3?logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-app-blue)
![SQLite](https://img.shields.io/badge/SQLite-local%20storage-003B57?logo=sqlite&logoColor=white)

> **An R Shiny app that turns a physiotherapist's musculoskeletal assessment into a structured, print-ready SOAPER report in minutes — with region-specific prompts, built-in red-flag screening and templates for treatment plans and home exercise programmes.**

**Live demo:** <https://greencare-physio.shinyapps.io/SOAP/>

> ⚠️ **The public demo is for evaluation only. Do not enter real patient data.** Slide the **TRIAL** switch (top right of the tab bar) to try the whole form without any patient details; nothing is saved in trial mode.

---

## Screenshots

<!-- Add your own screenshots to docs/screenshots/ and keep these file names (or edit the links). -->

| Intake & document viewer | Evaluation form (S / O / A / P / E / R) |
|---|---|
| ![Intake](docs/screenshots/01-intake.png) | ![Evaluation form](docs/screenshots/02-evaluation-form.png) |

| Generated, print-ready report | Template manager |
|---|---|
| ![Report](docs/screenshots/03-report.png) | ![Templates](docs/screenshots/04-templates.png) |

---

## Why it matters

Musculoskeletal assessments are documented under time pressure. In practice this leads to problems that matter for both quality of care and patient safety:

| Real-world pain point | How this tool addresses it |
|---|---|
| Free-text notes vary from clinician to clinician and are hard to compare between visits | One fixed **SOAPER** structure (Subjective, Objective, Assessment, Plan, Education, Review) with consistent fields |
| Region-specific tests and measures are easy to forget | Choosing the body region(s) loads the matching **ROM movements, manual muscle testing groups and special tests**; several regions can be assessed in one report |
| Red flags can be missed when notes are written quickly | A **safety red-flag checklist** (common flags plus region-specific ones such as cauda equina signs, cervical myelopathy / VBI signs, DVT signs) is part of every assessment |
| Re-typing the same treatment plans and home exercise advice every visit | Reusable **treatment-plan and home-exercise templates** that can be searched, edited and applied in one click |
| Outcomes are hard to follow over time | Pre- and post-treatment **VAS** pain scores and follow-up plan are recorded on every report |
| Mixing up patients or leaving another patient's details behind | Forms start **completely blank** (no pre-filled sample data), name and IC / passport are mandatory before a report is generated, and overwriting or clearing data always asks for confirmation |

The goal is not to replace clinical judgement. It is to make complete, legible, consistent documentation the path of least resistance.

---

## Key features

**Tab 1 — Document Upload & Intake**
- Upload a referral letter and diagnostic reports (PDF / PNG / JPG, up to 50 MB each) and preview them side by side with the form.
- Patient demographics, diagnosis and key imaging findings.
- IC / passport numbers are normalised (e.g. `880512015431` → `880512-01-5431`) so the same patient is always matched correctly.
- Assessment date can be back-dated (future dates are blocked).

**Tab 2 — Evaluation Form & SOAPER Matrix**
- Collapsible S / O / A / P / E / R sections with a pinned **Generate** button.
- Multi-region objective examination: ROM (AROM / PROM), MMT grades, special tests, balance, functional assessment, joint stability and measurements.
- Red-flag checklist, clinical impression, precautions, treatment plan, home exercise programme and next-session plan.
- Generates a formatted report that can be **printed / saved as PDF**, or **copied as plain text**.
- **Print layout:** A4, tables are never split across pages (a table that does not fit moves to the next page as a whole), patient name and *Page X of Y* in the footer, PDF named `PatientName_Date`, and a blank signature block (name, signature, date) for signing by hand.
- **Import last record by IC** (with a confirmation step) to start a follow-up visit from the previous one.
- **Draft autosave** in the browser: after a refresh, disconnect or timeout the app offers to restore the unsaved form.
- **Trial mode** for demos and training: no patient details required, nothing saved, previous records cannot be loaded.

**Tab 3 — Templates Manager**
- Create, search, edit and delete treatment-plan and home-exercise templates (with a warning on duplicate titles and a confirmation before deleting).
- Four example templates are seeded on first run (lumbar radiculopathy care, shoulder impingement protocol, lumbar extension / core control HEP, shoulder rotator cuff & scapular HEP). They are examples only — review and replace them with your own protocols.

**Supported regions:** shoulder, lumbar spine, cervical spine, thoracic spine / scoliosis, knee, hip and ankle.

---

## Technical stack

| Layer | Technology |
|---|---|
| Language / framework | **R**, **Shiny** |
| Persistence | **SQLite** via `DBI` + `RSQLite` (single local file) |
| Data exchange | `jsonlite` (form state is stored as JSON alongside each record) |
| UI helpers | `shinyjs`, custom HTML / CSS / JavaScript |
| Report generation | Server-side HTML report; printed through an isolated hidden `iframe` so page breaks are controlled |
| Deployment | shinyapps.io (`rsconnect`) or any Shiny Server / Posit Connect host |

**Design notes**
- The whole app lives in a single `app.R` for easy deployment and review.
- Dynamic ROM / MMT / special-test tables keep what has been entered when you deselect and reselect a region.
- All user input is HTML-escaped before it is placed into a report.
- The app makes **no external network calls**; everything runs on your own server or machine.

---

## Clinical basis & scope

- The documentation structure follows the widely used **SOAP** note format, extended with **Education** and **Review** (SOAPER).
- The region-specific lists of movements, muscle groups, special tests and red flags are **documentation prompts** for a musculoskeletal assessment. They are **not** a diagnostic algorithm, and the app does not make clinical decisions.
- This project is **not** a certified medical device and does not claim conformity with a specific clinical guideline. If you adapt the content to a guideline or local protocol, please list the reference here:

  > _Guideline / protocol references: add here_

- Always apply clinical reasoning. Treatment templates are starting points, not prescriptions.

---

## Data privacy & safety

What the app does:
- Stores records locally in `rehab_records.db` (SQLite) and uploaded files in `patient_files/<IC>/<date>/`. Nothing is sent to third parties.
- Uploaded files are **not served publicly**. Previews are copied into a private, per-session temporary folder that is deleted when the session ends.
- Unsaved drafts stay **in the user's own browser only**, are cleared once a report is saved, and are discarded after 72 hours.
- **Trial mode** writes nothing to the database or to disk and blocks loading existing records.
- Forms are blank by default; sample data only appears if someone explicitly clicks **Demo Data** (fictional patient).

What you must still take care of before using it with real patients:
- **There is no login or user management.** Anyone who can open the app can use it. Put it behind your own authentication (or run it on an internal network) if it will hold real data.
- Files and databases are **not encrypted at rest** and there is no audit trail.
- On shinyapps.io the local file system may not persist across restarts or redeployments. Use a host with persistent storage (and backups) for real records.
- **Never commit patient data to Git.** Add this to `.gitignore`:

  ```gitignore
  rehab_records.db
  patient_files/
  ```
- Compliance with local data-protection law (for example PDPA, GDPR or HIPAA, depending on where you work) is the responsibility of whoever deploys the app.

---

## Quick start

### Option 1 — Try it online
Open the live demo: <https://greencare-physio.shinyapps.io/SOAP/> and use **Trial mode**.

### Option 2 — Run locally

```bash
git clone https://github.com/<your-username>/SOAP.git
cd SOAP
```

Install the dependencies once (in R):

```r
install.packages(c("shiny", "shinyjs", "jsonlite", "DBI", "RSQLite"))
```

Run the app:

```r
shiny::runApp("app.R")
```

or open `SOAP.Rproj` in RStudio and click **Run App**.

On first run the app creates `rehab_records.db` and the `patient_files/` folder in the working directory and seeds the example templates.

**Browser note:** the *Page X of Y* footer in printed reports needs a recent Chrome or Edge (131+). Other browsers ignore it and print everything else normally.

### Deploying to shinyapps.io

```r
install.packages("rsconnect")
rsconnect::deployApp("app.R", appName = "SOAP")
```

---

## Project structure

```
SOAP/
├── app.R            # the complete Shiny application (UI + server + database layer)
├── SOAP.Rproj       # RStudio project file
├── README.md
├── docs/
│   └── screenshots/ # images used in this README
├── rehab_records.db # created on first run (do not commit)
└── patient_files/   # created on first save (do not commit)
```

---

## Disclaimer

This software is provided for documentation support only and comes with no warranty. It does not provide medical advice or replace professional clinical judgement. The authors are not responsible for any decisions made using its output.
