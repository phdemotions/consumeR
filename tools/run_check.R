#!/usr/bin/env Rscript

# tools/run_check.R
# CRAN-like check runner for CI + Codex consumption.
# Writes:
#   artifacts/check_stdout.txt       (stdout+stderr captured by rcmdcheck)
#   artifacts/check_summary.json     (structured errors/warnings/notes)
#   artifacts/check_sessionInfo.txt  (repro metadata)
#   artifacts/Rcheck/*               (archived .Rcheck logs/artifacts)
#
# Run:
#   Rscript tools/run_check.R
#
# Env vars:
#   CHECK_ARGS="--as-cran --no-manual"
#   CHECK_ERROR_ON="warning"  # "never" | "note" | "warning" | "error"

stop_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Missing package '", pkg, "'. Install it with install.packages('", pkg, "').")
  }
}

stop_if_missing("rcmdcheck")
stop_if_missing("jsonlite")

`%||%` <- function(x, y) if (is.null(x)) y else x

out_dir <- "artifacts"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

check_args <- Sys.getenv("CHECK_ARGS", unset = "--as-cran --no-manual")
check_args <- strsplit(check_args, "\\s+")[[1]]
check_args <- check_args[nzchar(check_args)]

error_on <- Sys.getenv("CHECK_ERROR_ON", unset = "never")

# Helpful defaults for consistent output and surfacing warnings
options(
  warn = 1,
  stringsAsFactors = FALSE
)

res <- rcmdcheck::rcmdcheck(
  path = ".",
  args = check_args,
  error_on = error_on,
  quiet = FALSE
)

# Plaintext transcript (include stderr when available)
stdout_txt <- c(res$stdout %||% character(0), res$stderr %||% character(0))
writeLines(stdout_txt, file.path(out_dir, "check_stdout.txt"))

# Structured summary
make_item <- function(x) {
  if (is.null(x) || length(x) == 0) return(list())
  lapply(x, function(it) as.list(it))
}

summary <- list(
  package = res$package %||% NA_character_,
  checkdir = res$checkdir %||% NA_character_,
  platform = res$platform %||% NA_character_,
  rversion = res$rversion %||% NA_character_,
  args = check_args,
  errors = make_item(res$errors),
  warnings = make_item(res$warnings),
  notes = make_item(res$notes)
)

jsonlite::write_json(
  x = summary,
  path = file.path(out_dir, "check_summary.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  null = "null"
)

# Repro metadata
sess <- capture.output(utils::sessionInfo())
writeLines(sess, file.path(out_dir, "check_sessionInfo.txt"))

# Archive the full .Rcheck directory (canonical debugging artifacts)
rcheck_src <- res$checkdir %||% NA_character_
if (!is.na(rcheck_src) && dir.exists(rcheck_src)) {
  rcheck_dst <- file.path(out_dir, "Rcheck")
  if (dir.exists(rcheck_dst)) unlink(rcheck_dst, recursive = TRUE, force = TRUE)
  dir.create(rcheck_dst, recursive = TRUE)

  # Copy everything; this is typically not huge and avoids missing key logs.
  file.copy(
    from = list.files(rcheck_src, full.names = TRUE, all.files = TRUE, no.. = TRUE),
    to = rcheck_dst,
    recursive = TRUE,
    overwrite = TRUE
  )
}

cat("\nWrote artifacts to '", out_dir, "/'\n", sep = "")
