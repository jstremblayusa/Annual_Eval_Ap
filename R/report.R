library(officer)
library(flextable)

safe_text <- function(x, fallback = "") {
  if (is.null(x) || !length(x) || is.na(x[1])) fallback else as.character(x[1])
}

add_area_report <- function(doc, area, narrative, activities, summary) {
  doc <- body_add_par(doc, area, style = "heading 1")
  doc <- body_add_par(doc, "Faculty narrative", style = "heading 2")
  doc <- body_add_par(doc, safe_text(narrative, "No narrative supplied."))

  area_rows <- activities[activities$portfolio_area == tolower(area), , drop = FALSE]
  if (nrow(area_rows)) {
    report_table <- data.frame(
      Category = area_rows$category,
      Activity = area_rows$activity_title,
      Description = area_rows$activity_description,
      Role = area_rows$faculty_role,
      Supporting.Information = area_rows$supporting_information,
      Points = area_rows$claimed_points_total,
      Justification = area_rows$point_justification,
      check.names = FALSE
    )
    ft <- flextable(report_table) |>
      theme_vanilla() |>
      bg(part = "header", bg = "#0A233F") |>
      color(part = "header", color = "white") |>
      fontsize(size = 8, part = "all") |>
      autofit()
    doc <- body_add_flextable(doc, ft)
  } else {
    doc <- body_add_par(doc, "No activities entered.")
  }

  doc <- body_add_par(doc, sprintf("Total points: %s", summary$total), style = "heading 2")
  doc <- body_add_par(doc, sprintf("Provisional rating: %s", summary$rating))
  doc <- body_add_par(doc, "Chair assessment: ______________________________________________")
  doc
}

create_portfolio_report <- function(file, faculty, evaluation, activities, summaries) {
  doc <- read_docx()
  doc <- body_add_par(doc, "PBSci Annual Self Evaluation Portfolio", style = "Title")
  doc <- body_add_par(doc, paste(faculty$first_name, faculty$last_name), style = "Subtitle")
  doc <- body_add_par(doc, paste("Evaluation year:", evaluation$evaluation_year))
  doc <- body_add_par(doc, paste("Position:", faculty$position))
  doc <- body_add_par(doc, paste("Teaching assignment:", evaluation$teaching_assignment))
  doc <- body_add_par(doc, paste("Research assignment:", evaluation$research_assignment_category))
  doc <- body_add_par(doc, paste("Service assignment:", evaluation$service_assignment_category))
  doc <- body_add_par(doc, paste("Sabbatical status:", evaluation$sabbatical_status))

  doc <- add_area_report(doc, "Teaching", evaluation$teaching_narrative,
                         activities, summaries$teaching)
  doc <- add_area_report(doc, "Research", evaluation$research_narrative,
                         activities, summaries$research)
  doc <- add_area_report(doc, "Service", evaluation$service_narrative,
                         activities, summaries$service)

  doc <- body_add_par(doc, "Certification", style = "heading 1")
  doc <- body_add_par(
    doc,
    paste(
      "The provisional ratings in this report are calculations based on",
      "faculty-entered information. They do not replace the chair's evaluation."
    )
  )
  doc <- body_add_par(doc, paste("Generated:", format(Sys.time(), "%B %d, %Y at %I:%M %p")))
  print(doc, target = file)
}
