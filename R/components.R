area_module_ui <- function(id, title) {
  ns <- NS(id)
  tagList(
    div(class = "section-intro",
        h2(title),
        p("Enter a concise narrative, confirm the minimum requirements, and add each scored activity.")),

    card(
      class = "portfolio-card",
      card_header("Narrative"),
      card_body(
        textAreaInput(ns("narrative"), NULL, rows = 6,
                      placeholder = paste("Summarize the quality, outcomes, context, and overall significance of your", tolower(title), "work.")),
        actionButton(ns("save_narrative"), "Save narrative", class = "btn-primary")
      )
    ),

    card(
      class = "portfolio-card",
      card_header("Minimum requirements"),
      card_body(
        uiOutput(ns("requirements_ui")),
        actionButton(ns("save_requirements"), "Save requirements", class = "btn-outline-primary")
      )
    ),

    card(
      class = "portfolio-card",
      card_header("Add activity"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          selectInput(ns("category"), "Category", choices = character()),
          selectInput(ns("rule_id"), "Activity", choices = character())
        ),
        uiOutput(ns("rule_help")),
        layout_columns(
          col_widths = c(8, 4),
          textInput(ns("activity_title"), "Activity title"),
          textInput(ns("activity_period"), "Date or period")
        ),
        textAreaInput(ns("description"), "Accomplishment or outcome", rows = 3),
        layout_columns(
          col_widths = c(6, 6),
          textAreaInput(ns("role"), "Your role and contribution", rows = 3),
          textAreaInput(ns("effort"), "Time and effort", rows = 3)
        ),
        textAreaInput(ns("supporting"), "Supporting information", rows = 3,
                      placeholder = "Enter concise evidence; no document upload is required."),
        layout_columns(
          col_widths = c(4, 4, 4),
          numericInput(ns("quantity"), "Quantity", value = 1, min = 1, step = 1),
          uiOutput(ns("points_ui")),
          textOutput(ns("calculated_total"))
        ),
        textAreaInput(ns("justification"), "Point justification", rows = 3),
        checkboxInput(ns("possible_duplicate"), "This activity is described in another portfolio section", FALSE),
        conditionalPanel(
          condition = sprintf("input['%s']", ns("possible_duplicate")),
          selectInput(ns("primary_area"), "Section receiving points",
                      choices = c("Teaching" = "teaching", "Research" = "research", "Service" = "service"))
        ),
        actionButton(ns("add_activity"), "Add activity", class = "btn-primary")
      )
    ),

    card(
      class = "portfolio-card",
      card_header("Recorded activities"),
      card_body(
        DTOutput(ns("activity_table")),
        actionButton(ns("delete_activity"), "Remove selected activity", class = "btn-outline-danger")
      )
    ),

    uiOutput(ns("summary_ui"))
  )
}

area_module_server <- function(id, area, token, evaluation, activities,
                               scoring_rules, rating_rules, requirement_definitions,
                               requirement_responses, assignment_category,
                               refresh_all) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    area_rules <- reactive({
      rules <- scoring_rules()
      rules[rules$portfolio_area == area & rules$active %in% TRUE, , drop = FALSE]
    })

    observe({
      rules <- area_rules()
      updateSelectInput(session, "category", choices = unique(rules$category))
    })

    observeEvent(input$category, {
      rules <- area_rules()
      rules <- rules[rules$category == input$category, , drop = FALSE]
      choices <- setNames(rules$rule_id, rules$activity_name)
      updateSelectInput(session, "rule_id", choices = choices)
    }, ignoreInit = FALSE)

    selected_rule <- reactive({
      req(input$rule_id)
      rules <- area_rules()
      rules[rules$rule_id == input$rule_id, , drop = FALSE][1, ]
    })

    output$rule_help <- renderUI({
      r <- selected_rule()
      div(
        class = "rule-help",
        tags$strong(sprintf("Official range: %s–%s points per %s.",
                            r$minimum_points, r$maximum_points, r$unit_label)),
        if (nzchar(r$instructions)) tags$p(r$instructions)
      )
    })

    output$points_ui <- renderUI({
      r <- selected_rule()
      numericInput(ns("points"), paste("Points per", r$unit_label),
                   value = r$minimum_points, min = r$minimum_points,
                   max = r$maximum_points, step = 1)
    })

    output$calculated_total <- renderText({
      paste("Item total:", (input$quantity %||% 1) * (input$points %||% 0))
    })

    observe({
      ev <- evaluation()
      if (!is.null(ev) && nrow(ev)) {
        field <- paste0(area, "_narrative")
        updateTextAreaInput(session, "narrative", value = ev[[field]][1] %||% "")
      }
    })

    observeEvent(input$save_narrative, {
      ev <- evaluation()
      req(nrow(ev))
      field <- paste0(area, "_narrative")
      values <- setNames(list(input$narrative %||% ""), field)
      tryCatch({
        sb_patch("evaluations", token(),
                 list(evaluation_id = paste0("eq.", ev$evaluation_id[1])), values)
        showNotification("Narrative saved.", type = "message")
        refresh_all()
      }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
    })

    area_requirements <- reactive({
      defs <- requirement_definitions()
      defs[defs$portfolio_area == area, , drop = FALSE]
    })

    output$requirements_ui <- renderUI({
      defs <- area_requirements()
      responses <- requirement_responses()
      tagList(lapply(seq_len(nrow(defs)), function(i) {
        rid <- defs$requirement_id[i]
        matched <- responses[responses$requirement_id == rid, , drop = FALSE]
        checked <- nrow(matched) && isTRUE(matched$met[1])
        checkboxInput(ns(paste0("req_", rid)), defs$requirement_text[i], checked)
      }))
    })

    observeEvent(input$save_requirements, {
      ev <- evaluation(); req(nrow(ev))
      defs <- area_requirements()
      tryCatch({
        for (i in seq_len(nrow(defs))) {
          rid <- defs$requirement_id[i]
          met <- isTRUE(input[[paste0("req_", rid)]])
          sb_upsert(
            "requirement_responses", token(),
            list(evaluation_id = ev$evaluation_id[1], requirement_id = rid, met = met),
            "evaluation_id,requirement_id"
          )
        }
        showNotification("Minimum requirements saved.", type = "message")
        refresh_all()
      }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
    })

    observeEvent(input$add_activity, {
      ev <- evaluation(); req(nrow(ev))
      r <- selected_rule()
      errors <- validate_activity(
        r, input$activity_title %||% "", input$description %||% "",
        input$role %||% "", input$effort %||% "", input$supporting %||% "",
        input$quantity, input$points, input$justification %||% ""
      )
      if (length(errors)) {
        showModal(modalDialog(title = "Complete this activity", tags$ul(lapply(errors, tags$li)), easyClose = TRUE))
        return()
      }
      primary_area <- if (isTRUE(input$possible_duplicate)) input$primary_area else area
      claimed_total <- if (identical(primary_area, area)) input$quantity * input$points else 0
      values <- list(
        evaluation_id = ev$evaluation_id[1], rule_id = r$rule_id,
        portfolio_area = area, activity_title = trimws(input$activity_title),
        activity_period = trimws(input$activity_period %||% ""),
        activity_description = trimws(input$description), faculty_role = trimws(input$role),
        time_effort = trimws(input$effort), supporting_information = trimws(input$supporting),
        quantity = input$quantity, claimed_points_per_unit = input$points,
        claimed_points_total = claimed_total, point_justification = trimws(input$justification),
        possible_duplicate = isTRUE(input$possible_duplicate), primary_portfolio_part = primary_area
      )
      tryCatch({
        sb_insert("activities", token(), values)
        showNotification("Activity added.", type = "message")
        updateTextInput(session, "activity_title", value = "")
        updateTextInput(session, "activity_period", value = "")
        updateTextAreaInput(session, "description", value = "")
        updateTextAreaInput(session, "role", value = "")
        updateTextAreaInput(session, "effort", value = "")
        updateTextAreaInput(session, "supporting", value = "")
        updateTextAreaInput(session, "justification", value = "")
        updateCheckboxInput(session, "possible_duplicate", value = FALSE)
        refresh_all()
      }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
    })

    area_activities <- reactive({
      a <- activities()
      a[a$portfolio_area == area, , drop = FALSE]
    })

    output$activity_table <- renderDT({
      a <- area_activities()
      if (!nrow(a)) return(datatable(data.frame(Message = "No activities entered."), options = list(dom = "t")))
      display <- a[, c("category", "activity_title", "quantity", "claimed_points_per_unit", "claimed_points_total"), drop = FALSE]
      names(display) <- c("Category", "Activity", "Quantity", "Points per unit", "Total")
      datatable(display, rownames = FALSE, selection = "single", options = list(pageLength = 8, dom = "tip"))
    })

    observeEvent(input$delete_activity, {
      selected <- input$activity_table_rows_selected
      a <- area_activities()
      if (!length(selected) || !nrow(a)) {
        showNotification("Select an activity first.", type = "warning")
        return()
      }
      showModal(modalDialog(
        title = "Remove activity?",
        p(a$activity_title[selected]),
        footer = tagList(modalButton("Cancel"), actionButton(ns("confirm_delete"), "Remove", class = "btn-danger"))
      ))
    })

    observeEvent(input$confirm_delete, {
      selected <- input$activity_table_rows_selected
      a <- area_activities(); req(length(selected), nrow(a))
      tryCatch({
        sb_delete("activities", token(), list(activity_id = paste0("eq.", a$activity_id[selected])))
        removeModal(); refresh_all()
      }, error = function(e) showNotification(conditionMessage(e), type = "error", duration = NULL))
    })

    current_summary <- reactive({
      req(nzchar(assignment_category()))
      area_summary(area, activities(), requirement_responses(), rating_rules(), assignment_category())
    })

    output$summary_ui <- renderUI({
      s <- current_summary()
      div(class = "area-summary",
          div(h4("Total points"), span(class = "summary-number", s$total)),
          div(h4("Minimum requirements"),
              span(class = if (s$requirements_met) "status-good" else "status-warning",
                   if (s$requirements_met) "Met" else "Incomplete")),
          div(h4("Provisional rating"), span(class = "summary-rating", s$rating)))
    })

    current_summary
  })
}
