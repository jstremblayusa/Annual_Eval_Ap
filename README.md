# PBSci Annual Self-Evaluation Portfolio

An R Shiny application for faculty to calculate and document annual Teaching, Research/Scholarship/Creative Activity, and Service evaluations. Supabase provides persistent storage and personalized revocable smart links. The chair is the sole administrator.

## Included functionality

- Private faculty roster loaded directly into Supabase, outside GitHub
- Personalized token links without names or N-numbers in the URL
- One shared activity-entry engine for Teaching, Research, and Service
- Official point ranges and assignment-dependent rating thresholds
- Required narratives, minimum-requirement checks, and point justifications
- Cross-category double-counting warnings
- Chair dashboard
- Downloadable Word portfolio report
- Non-destructive database setup

## 1. Configure Supabase

1. Create a new Supabase project or use the project intended for this application.
2. Open **SQL Editor** and run `supabase_setup.sql`. This GitHub-safe file contains no faculty roster.
3. Make a private local copy of `faculty_roster_seed_TEMPLATE.sql`, name it `faculty_roster_seed.sql`, replace its placeholder with the complete roster, and run the private copy directly in Supabase. Never commit that private file to GitHub.
4. Run `issue_access_links.sql` once.
5. Immediately save the returned name, email, and token table outside the repository. Supabase stores only hashes and cannot display the original tokens again.
6. Do not run destructive reset scripts. The supplied setup uses `CREATE IF NOT EXISTS` and `ON CONFLICT DO UPDATE`; it does not truncate evaluation data.

Construct links after the app has been deployed:

```text
https://YOUR-APP.share.connect.posit.cloud/?access=RETURNED_TOKEN
```

## 2. Test locally

Install R packages:

```r
install.packages(c(
  "shiny", "bslib", "DT", "httr2", "jsonlite",
  "officer", "flextable", "rsconnect"
))
```

Copy `.Renviron.example` to `.Renviron` and enter the Supabase project URL and publishable key. Restart R, open `app.R`, and select **Run App**. Add your administrator token to the local URL:

```text
?access=YOUR_ADMIN_TOKEN
```

## 3. Create the Connect Cloud manifest

From the RStudio console, with the project directory active:

```r
source("generate_manifest.R")
```

This creates `manifest.json`. Commit that file along with the other application files.

## 4. Deploy to Posit Connect Cloud

1. Upload this project structure to a GitHub repository.
2. In Connect Cloud, publish a **Shiny** application from the repository.
3. Select `app.R` as the primary file.
4. Add these secret variables:
   - `SUPABASE_URL`
   - `SUPABASE_ANON_KEY`
5. Publish the application.
6. Combine the deployed URL with each token returned by `issue_access_links.sql`.

Before committing, search the repository for `@unf.edu` and real N-numbers. A public repository should return no matches.

## Important implementation notes

- A smart link is a bearer credential. Anyone possessing the link can open that faculty portfolio.
- N-numbers and email addresses are never placed in URLs.
- Tokens are stored only as SHA-256 hashes.
- Faculty names, UNF emails, and N-numbers are not included in the GitHub-safe project files.
- Private roster seeds, roster CSV files, and access-link exports are excluded by `.gitignore`.
- Revoking an access-link row immediately disables that link.
- No document uploads are included in this version.
- Generated ratings are explicitly provisional and do not replace the chair's evaluation.
- Teaching, Research, and Service receive separate ratings; they are not collapsed into a single mathematical rating.

## Project files

- `app.R` — application shell, setup, dashboard, review, and reporting pages
- `R/components.R` — reusable activity-entry module
- `R/scoring.R` — validation and rating calculations
- `R/supabase.R` — Supabase REST interface
- `R/report.R` — Word report generation
- `www/styles.css` — UNF/PBSci visual styling
- `supabase_setup.sql` — non-destructive schema, rules, and policies with no roster data
- `faculty_roster_seed_TEMPLATE.sql` — placeholder structure for making a private roster seed outside GitHub
- `issue_access_links.sql` — creates missing smart links and returns their tokens once
- `generate_manifest.R` — creates the Posit Connect Cloud manifest
- `DESCRIPTION` — R dependency declaration
