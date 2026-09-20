library(shiny)
library(bslib)
library(DT)

source("R/supabase.R")
source("R/scoring.R")
source("R/report.R")
source("R/components.R")

`%||%` <- function(x, y) if (is.null(x) || !length(x) || is.na(x[1])) y else x

app_theme <- bs_theme(
  version = 5,
  primary = "#0A233F",
  secondary = "#A7A8A9",
  info = "#00B2E2",
  base_font = font_google("Open Sans")
)

ui <- page_fillable(
  theme = app_theme,
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$title("PBSci Annual Self-Evaluation Portfolio")
  ),
  uiOutput("application_ui")
)

server <- function(input, output, session) {
  state <- reactiveValues(
    token = "", profile = NULL, selected_faculty = NULL,
    evaluation = data.frame(), activities = data.frame(),
    requirement_responses = data.frame(), scoring_rules = data.frame(),
    rating_rules = data.frame(), requirement_definitions = data.frame(),
    faculty = data.frame(), error = NULL, ready = FALSE
  )

  token <- reactive(state$token)
  evaluation <- reactive(state$evaluation)
  activities <- reactive(state$activities)
  scoring_rules <- reactive(state$scoring_rules)
  rating_rules <- reactive(state$rating_rules)
  requirement_definitions <- reactive(state$requirement_definitions)
  requirement_responses <- reactive(state$requirement_responses)

  current_year <- reactive(as.integer(input$evaluation_year %||% format(Sys.Date(), "%Y")))

  normalize_activities <- function(a, rules) {
    if (!nrow(a)) {
      return(data.frame(
        activity_id = character(), evaluation_id = character(), rule_id = character(),
        portfolio_area = character(), activity_title = character(), activity_period = character(),
        activity_description = character(), faculty_role = character(), time_effort = character(),
        supporting_information = character(), quantity = numeric(), claimed_points_per_unit = numeric(),
        claimed_points_total = numeric(), point_justification = character(), possible_duplicate = logical(),
        primary_portfolio_part = character(), category = character(), activity_name = character(),
        unit_label = character(), indicator_type = character(), stringsAsFactors = FALSE
      ))
    }
    merge(a, rules[, c("rule_id", "category", "activity_name", "unit_label", "indicator_type")],
          by = "rule_id", all.x = TRUE, sort = FALSE)
  }

  normalize_requirements <- function(responses, definitions) {
    base <- definitions[, c("requirement_id", "portfolio_area", "requirement_text", "points", "required")]
    if (!nrow(responses)) {
      base$met <- FALSE
      return(base)
    }
    merged <- merge(base, responses[, c("requirement_id", "met")], by = "requirement_id", all.x = TRUE, sort = FALSE)
    merged$met[is.na(merged$met)] <- FALSE
    merged
  }

  refresh_all <- function() {
    req(state$profile, state$selected_faculty)
    fid <- state$selected_faculty$faculty_id[1]
    year <- current_year()
    state$faculty <- sb_get("faculty", state$token,
                            list(select = "faculty_id,n_number,first_name,formal_first_name,last_name,position,email,system_role,active,display_order",
                                 active = "eq.true", order = "display_order.asc"))
    raw <- load_evaluation_data(state$token, fid, year)
    state$evaluation <- raw$evaluation
    state$activities <- normalize_activities(raw$activities, state$scoring_rules)
    state$requirement_responses <- normalize_requirements(raw$requirements, state$requirement_definitions)
  }

  observeEvent(session$clientData$url_search, {
    state$token <- extract_access_token(session$clientData$url_search)
    if (!nzchar(state$token)) {
      state$error <- "This application requires a personalized access link."
      return()
    }
    tryCatch({
      state$profile <- load_profile(state$token)
      state$selected_faculty <- state$profile
      state$scoring_rules <- sb_get("scoring_rules", state$token,
                                    list(select = "*", active = "eq.true", order = "portfolio_area,display_order"))
      state$rating_rules <- sb_get("rating_rules", state$token,
                                   list(select = "*", order = "portfolio_area,assignment_category,rank_order"))
      state$requirement_definitions <- sb_get("minimum_requirements", state$token,
                                              list(select = "*", active = "eq.true", order = "portfolio_area,display_order"))
      state$ready <- TRUE
      refresh_all()
    }, error = function(e) state$error <- conditionMessage(e))
  }, once = TRUE, ignoreInit = FALSE)

  output$application_ui <- renderUI({
    if (!is.null(state$error)) {
      return(div(class = "access-error", h2("Access unavailable"), p(state$error)))
    }
    if (!isTRUE(state$ready)) return(div(class = "loading-screen", h3("Opening your portfolio…")))

    is_admin <- identical(state$profile$system_role[1], "admin")
    nav_items <- list(
      nav_panel("Setup", setup_ui(is_admin)),
      nav_panel("Teaching", area_module_ui("teaching", "Teaching")),
      nav_panel("Research", area_module_ui("research", "Research Scholarship and Creative Activity")),
      nav_panel("Service", area_module_ui("service", "Service")),
      nav_panel("Review", review_ui()),
      nav_panel("Report", report_ui())
    )
    if (is_admin) nav_items <- append(list(nav_panel("Chair Dashboard", dashboard_ui())), nav_items)

    do.call(
      page_navbar,
      c(list(title = div(class = "brand-title", "PBSci Annual Self-Evaluation"),
             theme = app_theme, fillable = FALSE), nav_items)
    )
  })

  setup_ui <- function(is_admin) {
    tagList(
      div(class = "page-heading", h2("Portfolio setup"),
          p("Confirm the evaluation year and assignment categories used for scoring.")),
      card(class = "portfolio-card", card_body(
        if (is_admin) selectInput("admin_faculty", "Faculty portfolio",
                                  choices = setNames(state$faculty$faculty_id,
                                                     paste(state$faculty$last_name, state$faculty$first_name))) else NULL,
        layout_columns(col_widths = c(6, 6),
          textInput("display_name", "Faculty member", value = paste(state$selected_faculty$first_name, state$selected_faculty$last_name), width = "100%"),
          textInput("display_position", "Position", value = state$selected_faculty$position, width = "100%")
        ),
        layout_columns(col_widths = c(4, 4, 4),
          selectInput("evaluation_year", "Evaluation year", choices = 2026:2030, selected = 2026),
          selectInput("teaching_assignment", "Teaching assignment",
                      choices = c("2/2", "2/3", "3/3", "3/4", "4/4", "Other")),
          textInput("other_assignment", "Other assignment details")
        ),
        layout_columns(col_widths = c(4, 4, 4),
          selectInput("teaching_fte_category", "Teaching FTE category",
                      choices = c("50% or greater", "25%–49%", "Less than 25%")),
          selectInput("research_assignment_category", "Research assignment category",
                      choices = c("1–12%", "12.5–25%", "26–37.5%", "38–50%", "51%+")),
          selectInput("service_assignment_category", "Service assignment category",
                      choices = c("Typical service effort", "Greater than typical service effort"))
        ),
        layout_columns(col_widths = c(6, 6),
          selectInput("sabbatical_status", "Sabbatical status",
                      choices = c("No", "Fall only", "Spring only", "Fall and Spring")),
          selectInput("sabbatical_basis", "Sabbatical evaluation basis",
                      choices = c("Typical research assignment", "Pre-agreed plan with chair"))
        ),
        textAreaInput("assignment_notes", "Assignment notes", rows = 3),
        actionButton("save_setup", "Save setup", class = "btn-primary")
      ))
    )
  }

  dashboard_ui <- function() {
    tagList(
      div(class = "page-heading", h2("Chair dashboard"), p("Open any faculty portfolio and monitor completion.")),
      card(class = "portfolio-card", card_body(DTOutput("dashboard_table")))
    )
  }

  review_ui <- function() {
    tagList(
      div(class = "page-heading", h2("Review and resolve"),
          p("Resolve incomplete requirements, missing narratives, and possible duplicate claims before generating the report.")),
      uiOutput("review_content")
    )
  }

  report_ui <- function() {
    tagList(
      div(class = "page-heading", h2("Generate report"),
          p("Download a formatted Word summary of the complete portfolio.")),
      uiOutput("report_summary"),
      downloadButton("download_report", "Generate Word report", class = "btn-primary"),
      actionButton("mark_complete", "Mark portfolio complete", class = "btn-outline-primary")
    )
  }

  observeEvent(input$admin_faculty, {
    req(identical(state$profile$system_role[1], "admin"), input$admin_faculty)
    chosen <- state$faculty[state$faculty$faculty_id == input$admin_faculty, , drop = FALSE]
    if (nrow(chosen)) {
      state$selected_faculty <- chosen[1, ]
      refresh_all()
    }
  })

  observeEvent(input$evaluation_year, {
    if (state$ready && !is.null(state$selected_faculty)) refresh_all()
  }, ignoreInit = TRUE)

  observeEvent(input$save_setup, {
    fid <- state$selected_faculty$faculty_id[1]
    values <- list(
      faculty_id = fid, evaluation_year = current_year(),
      teaching_assignment = input$teaching_assignment,
      other_assignment = input$other_assignment %||% "",
      teaching_fte_category = input$teaching_fte_category,
      research_assignment_category = input$research_assignment_category,
      service_assignment_category = input$service_assignment_category,
      sabbatical_status = input$sabbatical_status,
      sabbatical_basis = input$sabbatical_basis,
      assignment_notes = input$assignment_notes %||% ""
    )
    tryCatch({
      sb_upsert("evaluations", state$token, values, "faculty_id,evaluation_year")
      showNotification("Portfolio setup saved.", type = "message")
      refresh_all()
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
  })

  observe({
    ev <- state$evaluation
    if (nrow(ev)) {
      updateSelectInput(session, "teaching_assignment", selected = ev$teaching_assignment[1])
      updateTextInput(session, "other_assignment", value = ev$other_assignment[1] %||% "")
      updateSelectInput(session, "teaching_fte_category", selected = ev$teaching_fte_category[1])
      updateSelectInput(session, "research_assignment_category", selected = ev$research_assignment_category[1])
      updateSelectInput(session, "service_assignment_category", selected = ev$service_assignment_category[1])
      updateSelectInput(session, "sabbatical_status", selected = ev$sabbatical_status[1])
      updateSelectInput(session, "sabbatical_basis", selected = ev$sabbatical_basis[1])
      updateTextAreaInput(session, "assignment_notes", value = ev$assignment_notes[1] %||% "")
    }
  })

  teaching_summary <- area_module_server(
    "teaching", "teaching", token, evaluation, activities, scoring_rules, rating_rules,
    requirement_definitions, requirement_responses,
    reactive(state$evaluation$teaching_fte_category[1] %||% ""), refresh_all
  )
  research_summary <- area_module_server(
    "research", "research", token, evaluation, activities, scoring_rules, rating_rules,
    requirement_definitions, requirement_responses,
    reactive(state$evaluation$research_assignment_category[1] %||% ""), refresh_all
  )
  service_summary <- area_module_server(
    "service", "service", token, evaluation, activities, scoring_rules, rating_rules,
    requirement_definitions, requirement_responses,
    reactive(state$evaluation$service_assignment_category[1] %||% ""), refresh_all
  )

  output$dashboard_table <- renderDT({
    req(identical(state$profile$system_role[1], "admin"))
    evals <- sb_get("evaluations", state$token,
                    list(select = "faculty_id,evaluation_year,status,updated_at"))
    f <- state$faculty
    dash <- merge(f[, c("faculty_id", "last_name", "first_name", "position")], evals,
                  by = "faculty_id", all.x = TRUE)
    dash$evaluation_year[is.na(dash$evaluation_year)] <- current_year()
    dash$status[is.na(dash$status)] <- "Not started"
    dash <- dash[, c("last_name", "first_name", "position", "evaluation_year", "status")]
    names(dash) <- c("Last", "First", "Position", "Year", "Status")
    datatable(dash, rownames = FALSE, options = list(pageLength = 25, dom = "tip"))
  })

  output$review_content <- renderUI({
    ev <- state$evaluation
    if (!nrow(ev)) return(div(class = "warning-panel", "Save the Setup page before entering activities."))
    summaries <- list(teaching = teaching_summary(), research = research_summary(), service = service_summary())
    warnings <- character()
    for (area in names(summaries)) {
      if (!summaries[[area]]$requirements_met) warnings <- c(warnings, paste(tools::toTitleCase(area), "minimum requirements are incomplete."))
      field <- paste0(area, "_narrative")
      if (!nzchar(trimws(ev[[field]][1] %||% ""))) warnings <- c(warnings, paste(tools::toTitleCase(area), "narrative is missing."))
    }
    dupes <- state$activities[state$activities$possible_duplicate %in% TRUE, , drop = FALSE]
    if (nrow(dupes)) warnings <- c(warnings, paste(nrow(dupes), "activity record(s) are flagged for cross-category review."))
    if (!length(warnings)) div(class = "success-panel", h3("Portfolio checks passed"), p("No unresolved validation warnings were found."))
    else div(class = "warning-panel", h3("Items to review"), tags$ul(lapply(warnings, tags$li)))
  })

  output$report_summary <- renderUI({
    if (!nrow(state$evaluation)) return(p("Save the Setup page first."))
    s <- list(teaching_summary(), research_summary(), service_summary())
    labels <- c("Teaching", "Research", "Service")
    div(class = "report-grid", lapply(seq_along(s), function(i) {
      div(class = "report-result", h4(labels[i]),
          p(class = "summary-number", s[[i]]$total), p(s[[i]]$rating))
    }))
  })

  output$download_report <- downloadHandler(
    filename = function() {
      paste0("PBSci_Annual_Evaluation_", gsub("[^A-Za-z0-9]+", "_", state$selected_faculty$last_name),
             "_", current_year(), ".docx")
    },
    content = function(file) {
      req(nrow(state$evaluation))
      summaries <- list(teaching = teaching_summary(), research = research_summary(), service = service_summary())
      create_portfolio_report(file, state$selected_faculty, state$evaluation[1, ], state$activities, summaries)
    }
  )

  observeEvent(input$mark_complete, {
    req(nrow(state$evaluation))
    tryCatch({
      sb_patch("evaluations", state$token,
               list(evaluation_id = paste0("eq.", state$evaluation$evaluation_id[1])),
               list(status = "Faculty complete", submitted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")))
      refresh_all(); showNotification("Portfolio marked complete.", type = "message")
    }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
  })
}

shinyApp(ui, server)
