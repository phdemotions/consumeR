validate_spec <- function(spec) {
  if (!is.list(spec)) {
    rlang::abort("StudySpec must be a list.")
  }

  required_top <- c("version", "study", "data", "exclusions", "constructs", "standardize", "models", "tables", "pipeline")
  missing_top <- setdiff(required_top, names(spec))
  if (length(missing_top) > 0) {
    rlang::abort(paste0("StudySpec is missing required fields: ", paste(missing_top, collapse = ", "), "."))
  }

  required_study <- c("id", "title", "description")
  missing_study <- setdiff(required_study, names(spec$study))
  if (length(missing_study) > 0) {
    rlang::abort(paste0("StudySpec$study is missing required fields: ", paste(missing_study, collapse = ", "), "."))
  }

  required_data <- c("source", "format", "id_var")
  missing_data <- setdiff(required_data, names(spec$data))
  if (length(missing_data) > 0) {
    rlang::abort(paste0("StudySpec$data is missing required fields: ", paste(missing_data, collapse = ", "), "."))
  }

  if (!is.list(spec$exclusions)) {
    rlang::abort("StudySpec$exclusions must be a list (can be empty).")
  }

  validate_exclusion_entries(spec$exclusions, "StudySpec$exclusions")

  if (!is.null(spec$checks)) {
    if (!is.list(spec$checks)) {
      rlang::abort("StudySpec$checks must be a list when provided.")
    }
    allowed_checks <- c("pretest", "screening", "attention", "manipulation")
    unknown_checks <- setdiff(names(spec$checks), allowed_checks)
    if (length(unknown_checks) > 0) {
      rlang::abort(paste0("StudySpec$checks has unknown sections: ", paste(unknown_checks, collapse = ", "), "."))
    }
    purrr::walk(names(spec$checks), function(check_type) {
      checks <- spec$checks[[check_type]]
      if (!is.list(checks)) {
        rlang::abort(paste0("StudySpec$checks$", check_type, " must be a list (can be empty)."))
      }
      validate_exclusion_entries(checks, paste0("StudySpec$checks$", check_type))
    })
  }

  if (!is.list(spec$constructs)) {
    rlang::abort("StudySpec$constructs must be a list (can be empty).")
  }

  purrr::walk(spec$constructs, function(construct) {
    required_construct <- c("name", "items", "method")
    missing_construct <- setdiff(required_construct, names(construct))
    if (length(missing_construct) > 0) {
      rlang::abort(paste0("Each construct must include: ", paste(required_construct, collapse = ", "), "."))
    }
    if (!is.logical(construct$na_rm) && !is.null(construct$na_rm)) {
      rlang::abort("constructs$na_rm must be TRUE, FALSE, or NULL.")
    }
  })

  required_standardize <- c("vars", "method")
  missing_standardize <- setdiff(required_standardize, names(spec$standardize))
  if (length(missing_standardize) > 0) {
    rlang::abort(paste0("StudySpec$standardize is missing required fields: ", paste(missing_standardize, collapse = ", "), "."))
  }

  if (!is.list(spec$models)) {
    rlang::abort("StudySpec$models must be a list (can be empty).")
  }

  purrr::walk(spec$models, function(model) {
    required_model <- c("id", "description", "formula", "family")
    missing_model <- setdiff(required_model, names(model))
    if (length(missing_model) > 0) {
      rlang::abort(paste0("Each model must include: ", paste(required_model, collapse = ", "), "."))
    }
  })

  if (!is.list(spec$tables)) {
    rlang::abort("StudySpec$tables must be a list (can be empty).")
  }

  purrr::walk(spec$tables, function(table) {
    required_table <- c("id", "type", "model_id")
    missing_table <- setdiff(required_table, names(table))
    if (length(missing_table) > 0) {
      rlang::abort(paste0("Each table must include: ", paste(required_table, collapse = ", "), "."))
    }
  })

  if (!is.list(spec$pipeline) || is.null(spec$pipeline$steps)) {
    rlang::abort("StudySpec$pipeline must include a steps field.")
  }

  list(valid = TRUE, spec = spec)
}

validate_exclusion_entries <- function(entries, context) {
  purrr::walk(entries, function(entry) {
    required_exclusion <- c("id", "description", "filter")
    missing_exclusion <- setdiff(required_exclusion, names(entry))
    if (length(missing_exclusion) > 0) {
      rlang::abort(paste0(context, " entries must include: ", paste(required_exclusion, collapse = ", "), "."))
    }
  })
}

studyspec_template_paths <- function() {
  templates <- c("schema", "example")
  formats <- c("yaml", "json")
  grid <- expand.grid(template = templates, format = formats, stringsAsFactors = FALSE)
  paths <- apply(grid, 1, function(row) {
    system.file("studyspec", paste0(row[["template"]], ".", row[["format"]]), package = "consumeR")
  })
  tibble::tibble(
    template = grid$template,
    format = grid$format,
    path = unname(paths)
  )
}

resolve_spec_path <- function(path, template, format) {
  if (!is.null(path) && !is.null(template)) {
    rlang::abort("Provide either a path or a template, not both.")
  }
  if (!is.null(path)) {
    if (!is.character(path) || length(path) != 1) {
      rlang::abort("path must be a single string.")
    }
    return(path)
  }
  template <- rlang::arg_match(template, c("schema", "example"))
  format <- rlang::arg_match(format, c("yaml", "json"))
  resolved <- system.file("studyspec", paste0(template, ".", format), package = "consumeR")
  if (resolved == "") {
    rlang::abort("StudySpec template not found in the package installation.")
  }
  resolved
}

write_studyspec_template <- function(path, template = c("schema", "example"), format = c("yaml", "json"), overwrite = FALSE) {
  if (missing(path)) {
    rlang::abort("path is required to write a StudySpec template.")
  }
  if (!is.character(path) || length(path) != 1) {
    rlang::abort("path must be a single string.")
  }
  source_path <- resolve_spec_path(path = NULL, template = template, format = format)
  if (file.exists(path) && !isTRUE(overwrite)) {
    rlang::abort(paste0("File already exists at: ", path))
  }
  if (!dir.exists(dirname(path))) {
    rlang::abort(paste0("Directory does not exist: ", dirname(path)))
  }
  success <- file.copy(source_path, path, overwrite = overwrite)
  if (!isTRUE(success)) {
    rlang::abort("Failed to write StudySpec template.")
  }
  list(path = path, template = template, format = format)
}

read_spec_yaml <- function(path = NULL, template = NULL) {
  resolved_path <- resolve_spec_path(path, template, format = "yaml")
  if (!file.exists(resolved_path)) {
    rlang::abort(paste0("YAML spec not found at: ", resolved_path))
  }
  spec <- yaml::read_yaml(resolved_path)
  validate_spec(spec)
  list(spec = spec, source = resolved_path)
}

read_spec_json <- function(path = NULL, template = NULL) {
  resolved_path <- resolve_spec_path(path, template, format = "json")
  if (!file.exists(resolved_path)) {
    rlang::abort(paste0("JSON spec not found at: ", resolved_path))
  }
  spec <- jsonlite::fromJSON(resolved_path, simplifyVector = FALSE)
  validate_spec(spec)
  list(spec = spec, source = resolved_path)
}

safely_select <- function(data, vars, context) {
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    rlang::abort(paste0("Missing variables for ", context, ": ", paste(missing_vars, collapse = ", "), "."))
  }
  vars
}

ingest_data <- function(data_or_path, spec) {
  validate_spec(spec)

  if (inherits(data_or_path, "data.frame")) {
    data <- tibble::as_tibble(data_or_path)
  } else if (is.character(data_or_path) && length(data_or_path) == 1) {
    if (!file.exists(data_or_path)) {
      rlang::abort(paste0("Data file not found at: ", data_or_path))
    }
    format <- spec$data$format
    data <- switch(
      format,
      csv = readr::read_csv(data_or_path, show_col_types = FALSE),
      rds = readr::read_rds(data_or_path),
      rlang::abort(paste0("Unsupported data format: ", format))
    )
  } else {
    rlang::abort("data_or_path must be a data.frame or a file path string.")
  }

  if (!spec$data$id_var %in% names(data)) {
    rlang::abort(paste0("ID variable not found in data: ", spec$data$id_var))
  }

  list(data = data, source = data_or_path)
}

apply_exclusions <- function(data, spec) {
  validate_spec(spec)

  data <- tibble::as_tibble(data)
  data <- dplyr::mutate(data, .row_id = dplyr::row_number())
  audit_entries <- list()
  exclusion_logs <- list()
  exclusions <- assemble_exclusions(spec)

  if (length(exclusions) == 0) {
    audit <- tibble::tibble(
      exclusion_id = character(),
      exclusion_type = character(),
      description = character(),
      n_before = integer(),
      n_after = integer(),
      n_excluded = integer()
    )
    data <- dplyr::select(data, -".row_id")
    return(list(data = data, audit = audit, excluded = tibble::tibble()))
  }

  for (exclusion in exclusions) {
    filter_expr <- rlang::parse_expr(exclusion$filter)
    before_n <- nrow(data)
    excluded_rows <- dplyr::filter(data, !!filter_expr)
    data <- dplyr::filter(data, !(!!filter_expr))
    after_n <- nrow(data)

    audit_entries <- append(audit_entries, list(tibble::tibble(
      exclusion_id = exclusion$id,
      exclusion_type = exclusion$type,
      description = exclusion$description,
      n_before = before_n,
      n_after = after_n,
      n_excluded = nrow(excluded_rows)
    )))

    exclusion_logs <- append(exclusion_logs, list(tibble::tibble(
      subject_id = excluded_rows[[spec$data$id_var]],
      exclusion_id = exclusion$id,
      exclusion_type = exclusion$type,
      description = exclusion$description
    )))
  }

  audit <- dplyr::bind_rows(audit_entries)
  excluded <- dplyr::bind_rows(exclusion_logs)
  data <- dplyr::select(data, -".row_id")

  list(data = data, audit = audit, excluded = excluded)
}

score_constructs_from_dictionary <- function(data, spec) {
  validate_spec(spec)

  scored <- tibble::as_tibble(data)

  scored <- purrr::reduce(spec$constructs, function(current_data, construct) {
    items <- safely_select(current_data, construct$items, paste0("construct ", construct$name))
    na_rm <- if (is.null(construct$na_rm)) FALSE else construct$na_rm
    values <- dplyr::select(current_data, dplyr::all_of(items))

    scores <- switch(
      construct$method,
      mean = rowMeans(values, na.rm = na_rm),
      sum = rowSums(values, na.rm = na_rm),
      rlang::abort(paste0("Unsupported construct method: ", construct$method))
    )

    dplyr::mutate(current_data, !!construct$name := scores)
  }, .init = scored)

  list(data = scored, constructs = spec$constructs)
}

standardize_vars <- function(data, spec) {
  validate_spec(spec)

  vars <- safely_select(data, spec$standardize$vars, "standardize")
  method <- spec$standardize$method

  standardized <- switch(
    method,
    z = dplyr::mutate(
      data,
      dplyr::across(
        dplyr::all_of(vars),
        ~ as.numeric(stats::scale(.x))
      )
    ),
    rlang::abort(paste0("Unsupported standardization method: ", method))
  )

  list(data = standardized, method = method, vars = vars)
}

run_models <- function(data, spec) {
  validate_spec(spec)

  model_list <- purrr::map(spec$models, function(model) {
    formula <- stats::as.formula(model$formula)
    family <- model$family

    if (family == "gaussian") {
      fit <- stats::lm(formula, data = data)
    } else if (family == "binomial") {
      fit <- stats::glm(formula, data = data, family = stats::binomial())
    } else {
      rlang::abort(paste0("Unsupported model family: ", family))
    }

    list(id = model$id, description = model$description, fit = fit)
  })

  list(models = model_list)
}

build_tables <- function(model_results, spec) {
  validate_spec(spec)

  model_map <- purrr::map(model_results$models, ~ stats::coef(.x$fit))
  names(model_map) <- purrr::map_chr(model_results$models, "id")

  tables <- purrr::map(spec$tables, function(table) {
    model_id <- table$model_id
    if (!model_id %in% names(model_map)) {
      rlang::abort(paste0("Table references unknown model_id: ", model_id))
    }

    coefs <- model_map[[model_id]]
    tibble::tibble(
      term = names(coefs),
      estimate = unname(coefs),
      model_id = model_id,
      table_id = table$id
    )
  })

  list(tables = tables)
}

run_pipeline <- function(spec, data_or_path) {
  validate_spec(spec)

  ingest <- ingest_data(data_or_path, spec)
  exclusions <- apply_exclusions(ingest$data, spec)
  scored <- score_constructs_from_dictionary(exclusions$data, spec)
  standardized <- standardize_vars(scored$data, spec)
  model_results <- run_models(standardized$data, spec)
  tables <- build_tables(model_results, spec)

  list(
    data = standardized$data,
    exclusions = exclusions$audit,
    excluded_subjects = exclusions$excluded,
    constructs = scored$constructs,
    models = model_results$models,
    tables = tables$tables
  )
}

assemble_exclusions <- function(spec) {
  base_exclusions <- purrr::map(spec$exclusions, function(entry) {
    c(entry, list(type = "exclusion"))
  })
  if (is.null(spec$checks)) {
    return(base_exclusions)
  }
  check_entries <- purrr::map(names(spec$checks), function(check_type) {
    purrr::map(spec$checks[[check_type]], function(entry) {
      c(entry, list(type = check_type))
    })
  })
  c(base_exclusions, unlist(check_entries, recursive = FALSE))
}
