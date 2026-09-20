required <- c("shiny", "bslib", "DT", "httr2", "jsonlite", "officer", "flextable", "rsconnect")
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) install.packages(missing)
rsconnect::writeManifest(appDir = ".", appPrimaryDoc = "app.R")
message("manifest.json created. Commit it with the application files.")
