library(httr2)
library(jsonlite)

supabase_config <- function() {
  url <- sub("/$", "", Sys.getenv("SUPABASE_URL"))
  key <- Sys.getenv("SUPABASE_ANON_KEY")
  if (!nzchar(url) || !nzchar(key)) {
    stop("SUPABASE_URL and SUPABASE_ANON_KEY must be configured.")
  }
  list(url = url, key = key)
}

sb_request <- function(path, token, method = "GET", body = NULL,
                       query = list(), prefer = NULL) {
  cfg <- supabase_config()
  req <- request(paste0(cfg$url, "/rest/v1/", path)) |>
    req_headers(
      apikey = cfg$key,
      Authorization = paste("Bearer", cfg$key),
      `x-access-token` = token,
      Accept = "application/json"
    ) |>
    req_method(method)

  if (length(query)) req <- do.call(req_url_query, c(list(req), query))
  if (!is.null(prefer)) req <- req_headers(req, Prefer = prefer)
  if (!is.null(body)) req <- req_body_json(req, body, auto_unbox = TRUE)

  resp <- req_perform(req)
  if (resp_status(resp) >= 300) {
    stop(resp_body_string(resp))
  }
  text <- resp_body_string(resp)
  if (!nzchar(text)) return(invisible(NULL))
  fromJSON(text, simplifyDataFrame = TRUE)
}

sb_get <- function(table, token, query = list()) {
  sb_request(table, token, query = query)
}

sb_insert <- function(table, token, values) {
  sb_request(table, token, "POST", values,
             prefer = "return=representation")
}

sb_upsert <- function(table, token, values, conflict) {
  sb_request(
    table, token, "POST", values,
    query = list(on_conflict = conflict),
    prefer = "resolution=merge-duplicates,return=representation"
  )
}

sb_patch <- function(table, token, filters, values) {
  sb_request(table, token, "PATCH", values, query = filters,
             prefer = "return=representation")
}

sb_delete <- function(table, token, filters) {
  sb_request(table, token, "DELETE", query = filters,
             prefer = "return=representation")
}

extract_access_token <- function(search) {
  if (is.null(search) || !nzchar(search)) return("")
  params <- parseQueryString(sub("^\\?", "", search))
  trimws(params$access %||% "")
}

load_profile <- function(token) {
  result <- sb_get(
    "faculty", token,
    list(select = "faculty_id,n_number,first_name,formal_first_name,last_name,position,email,system_role,active",
         active = "eq.true")
  )
  if (!nrow(result)) stop("This access link is invalid or has been revoked.")
  if (any(result$system_role == "admin")) {
    result[result$system_role == "admin", , drop = FALSE][1, ]
  } else {
    result[1, , drop = FALSE]
  }
}

load_evaluation_data <- function(token, faculty_id, year) {
  evaluations <- sb_get(
    "evaluations", token,
    list(select = "*", faculty_id = paste0("eq.", faculty_id),
         evaluation_year = paste0("eq.", year))
  )
  activities <- data.frame()
  responses <- data.frame()
  if (nrow(evaluations)) {
    eid <- evaluations$evaluation_id[1]
    activities <- sb_get(
      "activities", token,
      list(select = "*", evaluation_id = paste0("eq.", eid), order = "created_at.asc")
    )
    responses <- sb_get(
      "requirement_responses", token,
      list(select = "*", evaluation_id = paste0("eq.", eid))
    )
  }
  list(evaluation = evaluations, activities = activities, requirements = responses)
}
