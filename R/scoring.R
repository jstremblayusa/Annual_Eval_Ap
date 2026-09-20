rating_from_rules <- function(area, assignment_category, total_points,
                              indicator_points = 0, major_points = 0,
                              requirements_met = TRUE, rating_rules) {
  rules <- rating_rules[
    rating_rules$portfolio_area == area &
      rating_rules$assignment_category == assignment_category,
    , drop = FALSE
  ]

  if (!nrow(rules)) return("Assignment category required")

  rules <- rules[order(rules$rank_order, decreasing = TRUE), , drop = FALSE]
  result <- "Unsatisfactory"

  for (i in seq_len(nrow(rules))) {
    r <- rules[i, ]
    total_ok <- total_points >= r$minimum_total &&
      (is.na(r$maximum_total) || total_points <= r$maximum_total)
    indicator_ok <- indicator_points >= r$minimum_indicator_points
    major_ok <- !isTRUE(r$major_required) || major_points > 0

    if (total_ok && indicator_ok && major_ok) {
      result <- r$rating
      break
    }
  }

  if (!isTRUE(requirements_met) &&
      result %in% c("Meets Expectations", "Exceeds Expectations",
                    "Far Exceeds Expectations")) {
    result <- "Below Expectations"
  }

  result
}

area_summary <- function(area, activities, requirements, rating_rules,
                         assignment_category) {
  a <- activities[activities$portfolio_area == area, , drop = FALSE]
  activity_total <- sum(a$claimed_points_total, na.rm = TRUE)
  major <- sum(a$claimed_points_total[a$indicator_type == "Major"], na.rm = TRUE)
  minor <- sum(a$claimed_points_total[a$indicator_type == "Minor"], na.rm = TRUE)
  indicator <- major + minor

  req <- requirements[requirements$portfolio_area == area, , drop = FALSE]
  required_rows <- req[isTRUE(req$required) | req$required %in% TRUE, , drop = FALSE]
  requirements_met <- nrow(required_rows) > 0 && all(required_rows$met %in% TRUE)
  requirement_points <- sum(req$points[req$met %in% TRUE], na.rm = TRUE)
  total <- activity_total + requirement_points

  list(
    total = total,
    activity_total = activity_total,
    requirement_points = requirement_points,
    major = major,
    minor = minor,
    indicator = indicator,
    requirements_met = requirements_met,
    rating = rating_from_rules(
      area, assignment_category, total, indicator, major,
      requirements_met, rating_rules
    )
  )
}

validate_activity <- function(rule, title, description, role, effort,
                              supporting_info, quantity, points, justification) {
  errors <- character()
  if (!nzchar(trimws(title))) errors <- c(errors, "Enter an activity title.")
  if (!nzchar(trimws(description))) errors <- c(errors, "Describe the accomplishment or outcome.")
  if (!nzchar(trimws(role))) errors <- c(errors, "Describe your role and contribution.")
  if (!nzchar(trimws(effort))) errors <- c(errors, "Describe the time or effort devoted.")
  if (!nzchar(trimws(supporting_info))) errors <- c(errors, "Enter concise supporting information.")
  if (is.na(quantity) || quantity < 1) errors <- c(errors, "Quantity must be at least 1.")
  if (is.na(points) || points < rule$minimum_points || points > rule$maximum_points) {
    errors <- c(errors, sprintf("Points must be between %s and %s per %s.",
                                rule$minimum_points, rule$maximum_points,
                                rule$unit_label))
  }
  if (isTRUE(rule$justification_required) && !nzchar(trimws(justification))) {
    errors <- c(errors, "A point justification is required.")
  }
  errors
}
