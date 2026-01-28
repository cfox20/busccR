#' Create a new consulting project
#'
#' Creates a new consulting project by:
#' \itemize{
#'   \item prompting the user to select the project folder (relative to the SCC Box root),
#'   \item generating a project ID of the form \code{term_department_contact},
#'   \item writing a canonical JSON project record to the project registry.
#' }
#'
#' The project registry is stored at \code{<Box Root>/project_registry/},
#' with one JSON file per project named \code{<project_id>.json}.
#'
#' @param project_name Project name (title).
#' @param term Academic term label (e.g., \code{"2026SP"}). If \code{NULL},
#'   the term is inferred from the system date.
#' @param start_date Optional project start date. Defaults to \code{Sys.Date()}.
#' @param category Optional category label (e.g., \code{"Education"}).
#' @param contact Primary contact name or email. Used to form the project ID.
#' @param department Department name. Used to form the project ID.
#' @param organization Optional organization name.
#' @param status Initial project status. Defaults to \code{"intake"}.
#' @param consultants Character vector of consultant names. Defaults to the current system user.
#' @param notes String of any additional notes about the project.
#'
#' @return
#' Invisibly returns a named list containing:
#' \itemize{
#'   \item \code{project_id}
#'   \item \code{project_path} (relative to the Box root)
#'   \item \code{registry_file} (full path to the JSON file)
#' }
#'
#' @examples
#' \dontrun{
#' create_project(
#'   project_name = "Student retention analysis",
#'   consultants = c("Jane Doe", "A. Smith"),
#'   category = "Education",
#'   department = "Student Success",
#'   contact = "jdoe@example.edu",
#'   organization = "Example University"
#' )
#' }
#'
#' @export
create_project <- function(
  project_name,
  term = NULL,
  start_date = NULL,
  category = NA,
  contact,
  department,
  organization = NA,
  status = "intake",
  consultants = Sys.info()[["user"]],
  notes = NA
) {
  if (missing(project_name) || !nzchar(project_name)) {
    stop("`project_name` must be provided.", call. = FALSE)
  }
  if (missing(department) || !nzchar(department)) {
    stop("`department` must be provided (used to form the project ID).", call. = FALSE)
  }
  if (missing(contact) || !nzchar(contact)) {
    stop("`contact` must be provided (used to form the project ID).", call. = FALSE)
  }

  # ---- resolve Box root ----
  box_root <- get_box_root()

  # ---- infer term if needed ----
  if (is.null(term)) {
    yr <- format(Sys.Date(), "%Y")
    mo <- as.integer(format(Sys.Date(), "%m"))
    term <- if (mo <= 5) {
      paste0(yr, "SP")
    } else if (mo <= 8) {
      paste0(yr, "SU")
    } else {
      paste0(yr, "FA")
    }
  }

  # ---- default start_date ----
  if (is.null(start_date)) {
    start_date <- Sys.Date()
  }

  # ---- helper: slugify for IDs/filenames ----
  slugify <- function(x) {
    x <- tolower(trimws(x))
    x <- gsub("[^a-z0-9]+", "_", x)
    x <- gsub("^_+|_+$", "", x)
    x
  }

  # ---- build project id: term_department_contact ----
  id <- paste(
    slugify(term),
    slugify(department),
    slugify(contact),
    sep = "_"
  )

  # ---- prompt user to choose project directory (must be inside box_root) ----
  project_abs <- NULL

  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::showDialog(
      title = "Select Project Folder",
      message = "Select the local file path to the project folder within the Statistical Consulting Center Box root.",
      # buttons = c("Next", "Cancel")
    )

    project_abs <- rstudioapi::selectDirectory(
      caption = "Select the project folder (must be inside the SCC Box root)"
    )
  } else if (.Platform$OS.type == "windows") {
    message("Select the project folder (must be inside the SCC Box root).")
    project_abs <- utils::choose.dir(
      caption = "Select the project folder (must be inside the SCC Box root)"
    )
  } else {
    message("Select any file within the project folder (must be inside the SCC Box root).")
    project_abs <- dirname(utils::file.choose())
  }

  if (is.null(project_abs) || !nzchar(project_abs)) {
    stop("No project directory selected. Project was not created.", call. = FALSE)
  }

  project_abs <- normalizePath(project_abs, winslash = "/", mustWork = TRUE)
  box_root_norm <- normalizePath(box_root, winslash = "/", mustWork = TRUE)

  # Ensure the selected folder is under the Box root.
  # Append trailing slash to avoid false prefix matches (e.g., /BoxRoot1 vs /BoxRoot).
  root_prefix <- paste0(box_root_norm, "/")
  proj_prefix <- paste0(project_abs, "/")

  if (!startsWith(proj_prefix, root_prefix)) {
    stop(
      "Selected project folder is not inside the configured Box root.\n",
      "Box root: ", box_root_norm, "\n",
      "Selected: ", project_abs,
      call. = FALSE
    )
  }

  # Store a *relative* path in the JSON record
  project_path <- sub(root_prefix, "", proj_prefix, fixed = TRUE)
  project_path <- sub("/$", "", project_path)

  # ---- define registry paths ----
  registry_dir <- file.path(box_root, "project_registry")
  registry_file <- file.path(registry_dir, paste0(id, ".json"))

  # ---- create registry directory ----
  if (!dir.exists(registry_dir)) {
    dir.create(registry_dir, recursive = TRUE)
  }

  # ---- prevent accidental overwrite ----
  if (file.exists(registry_file)) {
    stop(
      "A project with this id already exists in the registry: ",
      id,
      call. = FALSE
    )
  }

  # ---- build project record ----
  project_record <- list(
    term = term,
    id = id,
    start_date = as.character(start_date),
    end_date = NA_character_, # set only in complete_project()
    category = category,
    project_name = project_name,
    contact = contact,
    department = department,
    organization = organization,
    status = status,
    consultants = consultants,
    topics = character(0),
    methods = character(0),
    keywords = character(0),
    abstract = NA_character_,
    project_path = project_path, # relative to get_box_root()
    notes = notes
  )

  # ---- write JSON project card ----
  jsonlite::write_json(
    project_record,
    registry_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

  message("Project created: ", id)

  invisible(list(
    project_id = id,
    project_path = project_path,
    registry_file = registry_file
  ))
}


#' Update an existing consulting project record
#'
#' Updates fields in an existing JSON project record stored in the project registry.
#' Commonly updated fields include \code{status}, \code{keywords}, \code{methods},
#' \code{abstract}, and the project file path.
#'
#' @param id Project identifier (JSON filename without extension). If \code{NULL},
#'   the user is prompted to select a project JSON file.
#' @param status Optional new status. If \code{NULL}, status is unchanged.
#' @param keywords Optional character vector of keywords.
#' @param methods Optional character vector of methods.
#' @param abstract Optional new abstract.
#' @param project_path Optional. Use \code{"choose"} to select a new project
#'   folder via the file system. If \code{NULL}, the project path is unchanged.
#' @param append Logical; if \code{TRUE}, \code{keywords} and \code{methods}
#'   are appended (deduplicated). Defaults to \code{FALSE}.
#' @param overwrite Logical; skip the concurrency check if \code{TRUE}.
#'
#' @return Invisibly returns a list with \code{id}, \code{registry_file},
#'   and \code{updated_fields}.
#'
#' @export
update_project <- function(
  id = NULL,
  status = NULL,
  keywords = NULL,
  methods = NULL,
  abstract = NULL,
  project_path = NULL,
  append = FALSE,
  overwrite = FALSE
) {
  # ---- resolve Box root ----
  box_root <- get_box_root()
  registry_dir <- file.path(box_root, "project_registry")

  if (!dir.exists(registry_dir)) {
    stop("Project registry does not exist:\n", registry_dir, call. = FALSE)
  }

  # ---- locate registry file ----
  if (!is.null(id)) {
    registry_file <- file.path(registry_dir, paste0(id, ".json"))
    if (!file.exists(registry_file)) {
      stop("No project record found for id: ", id, call. = FALSE)
    }
  } else {
    if (!interactive()) {
      stop("Provide `id` in non-interactive sessions.", call. = FALSE)
    }

    files <- list.files(registry_dir, pattern = "\\.json$", full.names = TRUE)
    if (length(files) == 0) {
      stop("No project records found.", call. = FALSE)
    }

    if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
      registry_file <- rstudioapi::selectFile(
        caption = "Select a project to update",
        path = registry_dir
      )
    } else {
      registry_file <- utils::file.choose()
    }

    if (!nzchar(registry_file)) {
      stop("No project selected.", call. = FALSE)
    }

    id <- sub("\\.json$", "", basename(registry_file))
  }

  # ---- nothing to do guard ----
  if (is.null(status) && is.null(keywords) && is.null(methods) &&
    is.null(abstract) && is.null(project_path)) {
    stop(
      "Nothing to update. Provide at least one field to change.",
      call. = FALSE
    )
  }

  # ---- read record + mtime ----
  mtime_before <- file.info(registry_file)$mtime
  rec <- jsonlite::read_json(registry_file, simplifyVector = TRUE)

  updated_fields <- character(0)

  normalize_vec <- function(x) {
    x <- trimws(as.character(x))
    unique(x[nzchar(x)])
  }

  # ---- update status ----
  if (!is.null(status)) {
    rec$status <- as.character(status)
    updated_fields <- c(updated_fields, "status")
  }

  # ---- update keywords ----
  if (!is.null(keywords)) {
    new_kw <- normalize_vec(keywords)
    old_kw <- if (append) normalize_vec(rec$keywords) else character(0)
    rec$keywords <- unique(c(old_kw, new_kw))
    updated_fields <- c(updated_fields, "keywords")
  }

  # ---- update methods ----
  if (!is.null(methods)) {
    new_m <- normalize_vec(methods)
    old_m <- if (append) normalize_vec(rec$methods) else character(0)
    rec$methods <- unique(c(old_m, new_m))
    updated_fields <- c(updated_fields, "methods")
  }

  # ---- update abstract ----
  if (!is.null(abstract)) {
    rec$abstract <- as.character(abstract)
    updated_fields <- c(updated_fields, "abstract")
  }

  # ---- update project path ----
  if (!is.null(project_path)) {
    if (!interactive() && identical(project_path, "choose")) {
      stop("Cannot choose a project path in a non-interactive session.", call. = FALSE)
    }

    if (identical(project_path, "choose")) {
      if (requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()) {
        new_abs <- rstudioapi::selectDirectory(
          caption = "Select the new project folder (must be inside Box root)"
        )
      } else if (.Platform$OS.type == "windows") {
        new_abs <- utils::choose.dir(
          caption = "Select the new project folder (must be inside Box root)"
        )
      } else {
        new_abs <- dirname(utils::file.choose())
      }

      if (!nzchar(new_abs)) {
        stop("No project folder selected.", call. = FALSE)
      }
    } else {
      new_abs <- project_path
    }

    new_abs <- normalizePath(new_abs, winslash = "/", mustWork = TRUE)
    box_norm <- normalizePath(box_root, winslash = "/", mustWork = TRUE)

    root_prefix <- paste0(box_norm, "/")
    proj_prefix <- paste0(new_abs, "/")

    if (!startsWith(proj_prefix, root_prefix)) {
      stop(
        "Selected project folder is not inside the Box root.\n",
        "Box root: ", box_norm, "\n",
        "Selected: ", new_abs,
        call. = FALSE
      )
    }

    rec$project_path <- sub(root_prefix, "", proj_prefix, fixed = TRUE)
    rec$project_path <- sub("/$", "", rec$project_path)

    updated_fields <- c(updated_fields, "project_path")
  }

  # ---- timestamp ----
  rec$updated_at <- format(Sys.time(), tz = "UTC")

  # ---- concurrency check ----
  if (!overwrite) {
    if (!is.na(mtime_before) &&
      file.info(registry_file)$mtime != mtime_before) {
      stop(
        "Project record changed on disk during update.\n",
        "Re-run update_project() or set overwrite = TRUE.",
        call. = FALSE
      )
    }
  }

  # ---- write record ----
  jsonlite::write_json(
    rec,
    registry_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

  message("Project updated: ", id)

  invisible(list(
    id = id,
    registry_file = registry_file,
    updated_fields = unique(updated_fields)
  ))
}


#' Complete a consulting project record
#'
#' Marks a project as complete by:
#' \itemize{
#'   \item setting \code{end_date} (defaults to today),
#'   \item prompting the user to provide or confirm \code{methods},
#'   \item setting \code{status} to \code{"complete"}.
#' }
#'
#' The canonical JSON project record is updated in
#' \code{<Box Root>/project_registry/}.
#'
#' @param id Project identifier (JSON filename without extension). If \code{NULL},
#'   the user is prompted to select a project JSON file.
#' @param end_date Project end date. Defaults to \code{Sys.Date()}. May be a
#'   \code{Date} or a character string coercible to \code{Date}.
#' @param overwrite Logical; if \code{FALSE} (default), performs an optimistic
#'   concurrency check using file modification time. Set to \code{TRUE} to
#'   skip this check.
#'
#' @return
#' Invisibly returns a named list containing:
#' \itemize{
#'   \item \code{id}
#'   \item \code{registry_file}
#'   \item \code{end_date}
#'   \item \code{methods}
#' }
#'
#' @examples
#' \dontrun{
#' # Complete with today's date
#' complete_project(id = "2026sp_student_success_jdoe_example_edu")
#'
#' # Complete with a specified date
#' complete_project(
#'   id = "2026sp_student_success_jdoe_example_edu",
#'   end_date = "2026-02-15"
#' )
#' }
#'
#' @export
complete_project <- function(
  id = NULL,
  end_date = Sys.Date(),
  overwrite = FALSE
) {
  # ---- resolve Box root ----
  box_root <- get_box_root()
  registry_dir <- file.path(box_root, "project_registry")

  if (!dir.exists(registry_dir)) {
    stop("Project registry does not exist:\n", registry_dir, call. = FALSE)
  }

  # ---- locate registry file ----
  if (!is.null(id)) {
    registry_file <- file.path(registry_dir, paste0(id, ".json"))
    if (!file.exists(registry_file)) {
      stop("No project record found for id: ", id, call. = FALSE)
    }
  } else {
    if (!interactive()) {
      stop("Provide `id` in non-interactive sessions.", call. = FALSE)
    }

    files <- list.files(registry_dir, pattern = "\\.json$", full.names = TRUE)
    if (length(files) == 0) {
      stop("No project records found.", call. = FALSE)
    }

    if (requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable()) {
      registry_file <- rstudioapi::selectFile(
        caption = "Select a project to complete",
        path = registry_dir
      )
    } else {
      registry_file <- utils::file.choose()
    }

    if (!nzchar(registry_file)) {
      stop("No project selected.", call. = FALSE)
    }

    registry_file <- normalizePath(registry_file, winslash = "/", mustWork = TRUE)
    id <- sub("\\.json$", "", basename(registry_file))
  }

  # ---- coerce end_date ----
  if (!inherits(end_date, "Date")) {
    end_date <- as.Date(end_date)
    if (is.na(end_date)) {
      stop("`end_date` must be a Date or coercible to Date (YYYY-MM-DD).", call. = FALSE)
    }
  }
  end_date_chr <- as.character(end_date)

  # ---- read record + mtime ----
  mtime_before <- file.info(registry_file)$mtime
  rec <- jsonlite::read_json(registry_file, simplifyVector = TRUE)

  # ---- prompt user for methods (required) ----
  if (!interactive()) {
    stop(
      "Completing a project requires an interactive session to prompt for methods.\n",
      "Update methods first using update_project().",
      call. = FALSE
    )
  }

  normalize_vec <- function(x) {
    x <- trimws(as.character(x))
    x <- x[nzchar(x)]
    unique(x)
  }

  existing_methods <- normalize_vec(rec$methods)

  if (length(existing_methods) > 0) {
    message("Current methods: ", paste(existing_methods, collapse = ", "))
  } else {
    message("No methods currently recorded.")
  }

  methods_input <- readline(
    prompt = "Enter methods (comma-separated). Leave blank to keep existing: "
  )

  if (nzchar(methods_input)) {
    rec$methods <- normalize_vec(strsplit(methods_input, ",", fixed = TRUE)[[1]])
  } else {
    if (length(existing_methods) == 0) {
      stop(
        "Methods are required to complete a project.\n",
        "Re-run complete_project() and enter at least one method.",
        call. = FALSE
      )
    }
    rec$methods <- existing_methods
  }

  # ---- apply completion updates ----
  rec$end_date <- end_date_chr
  rec$status <- "complete"
  rec$updated_at <- format(Sys.time(), tz = "UTC")

  # ---- optimistic concurrency check ----
  if (!overwrite) {
    if (!is.na(mtime_before) &&
      file.info(registry_file)$mtime != mtime_before) {
      stop(
        "Project record changed on disk during completion.\n",
        "Re-run complete_project() or set overwrite = TRUE.",
        call. = FALSE
      )
    }
  }

  # ---- write record ----
  jsonlite::write_json(
    rec,
    registry_file,
    pretty = TRUE,
    auto_unbox = TRUE,
    na = "null"
  )

  message("Project marked as complete: ", id)

  invisible(list(
    id = id,
    registry_file = registry_file,
    end_date = rec$end_date,
    methods = rec$methods
  ))
}


#' Build a CSV registry of all consulting projects (base R only)
#'
#' Reads all JSON project records from the \code{project_registry} folder
#' under the Box root and compiles them into a single CSV file named
#' \code{project_registry.csv}, stored in the Box root directory.
#'
#' This implementation uses base R only (no package dependencies). Since base R
#' does not provide a full JSON parser, it supports the simple schema used by
#' this package: top-level scalar fields and arrays of strings (keywords, methods,
#' topics, consultants).
#'
#' @param overwrite Logical; if \code{FALSE} (default) and
#'   \code{project_registry.csv} already exists, the function stops.
#'   Set to \code{TRUE} to overwrite the existing file.
#'
#' @return Invisibly returns a data.frame containing the compiled registry.
#'
#' @examples
#' \dontrun{
#' build_registry()
#' build_registry(overwrite = TRUE)
#' }
#'
#' @export
build_registry <- function(overwrite = FALSE) {
  # ---- resolve Box root ----
  box_root <- get_box_root()

  registry_dir <- file.path(box_root, "project_registry")
  if (!dir.exists(registry_dir)) {
    stop("Project registry folder does not exist:\n", registry_dir, call. = FALSE)
  }

  json_files <- list.files(registry_dir, pattern = "\\.json$", full.names = TRUE)
  if (length(json_files) == 0) {
    stop("No project JSON files found in project_registry.", call. = FALSE)
  }

  output_csv <- file.path(box_root, "project_registry.csv")
  if (file.exists(output_csv) && !overwrite) {
    stop(
      "project_registry.csv already exists.\n",
      "Set overwrite = TRUE to rebuild it.",
      call. = FALSE
    )
  }

  # ---- helpers ----
  read_text <- function(path) {
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }

  # Convert any value into a single character scalar suitable for CSV.
  scalar_chr <- function(x) {
    if (is.null(x)) {
      return(NA_character_)
    }
    if (length(x) == 0) {
      return(NA_character_)
    }
    x <- as.character(x)
    x <- trimws(x)
    x <- x[nzchar(x)]
    if (length(x) == 0) {
      return(NA_character_)
    }
    if (length(x) == 1) {
      return(x)
    }
    paste(unique(x), collapse = "; ")
  }

  # Extract scalar string: "key": "value" or "key": null
  json_get_scalar <- function(txt, key) {
    pat_str <- paste0('"', key, '"\\s*:\\s*"([^"]*)"')
    m <- regexec(pat_str, txt, perl = TRUE)
    g <- regmatches(txt, m)[[1]]
    if (length(g) > 1) {
      return(g[2])
    }

    pat_null <- paste0('"', key, '"\\s*:\\s*null')
    if (grepl(pat_null, txt, perl = TRUE)) {
      return(NA_character_)
    }

    NA_character_
  }

  # Extract array of strings: "key": ["a","b"] or "key": []
  json_get_array <- function(txt, key) {
    pat <- paste0('"', key, '"\\s*:\\s*\\[(.*?)\\]')
    m <- regexec(pat, txt, perl = TRUE)
    g <- regmatches(txt, m)[[1]]
    if (length(g) <= 1) {
      return(character(0))
    }

    inside <- trimws(g[2])
    if (!nzchar(inside)) {
      return(character(0))
    }

    vals <- gregexpr('"([^"]*)"', inside, perl = TRUE)
    out <- regmatches(inside, vals)[[1]]
    out <- gsub('^"|"$', "", out)
    out <- trimws(out)
    out[nzchar(out)]
  }

  preferred_order <- c(
    "id", "project_name", "term", "status",
    "start_date", "end_date",
    "department", "organization", "contact",
    "category",
    "consultants",
    "keywords", "methods", "topics",
    "abstract",
    "project_path",
    "notes"
  )

  # ---- read each record into a named list of character scalars ----
  records <- lapply(json_files, function(f) {
    txt <- read_text(f)

    rec <- list(
      id           = json_get_scalar(txt, "id"),
      project_name = json_get_scalar(txt, "project_name"),
      term         = json_get_scalar(txt, "term"),
      status       = json_get_scalar(txt, "status"),
      start_date   = json_get_scalar(txt, "start_date"),
      end_date     = json_get_scalar(txt, "end_date"),
      department   = json_get_scalar(txt, "department"),
      organization = json_get_scalar(txt, "organization"),
      contact      = json_get_scalar(txt, "contact"),
      category     = json_get_scalar(txt, "category"),
      abstract     = json_get_scalar(txt, "abstract"),
      project_path = json_get_scalar(txt, "project_path"),
      notes        = json_get_scalar(txt, "notes"),
      consultants  = scalar_chr(json_get_array(txt, "consultants")),
      keywords     = scalar_chr(json_get_array(txt, "keywords")),
      methods      = scalar_chr(json_get_array(txt, "methods")),
      topics       = scalar_chr(json_get_array(txt, "topics"))
    )

    # Fallback id from filename
    rec$id <- scalar_chr(rec$id)
    if (is.na(rec$id) || !nzchar(rec$id)) {
      rec$id <- sub("\\.json$", "", basename(f))
    }

    # Force all fields to single character scalars (prevents list-columns)
    for (nm in names(rec)) rec[[nm]] <- scalar_chr(rec[[nm]])

    rec
  })

  # ---- ensure uniform columns ----
  all_names <- unique(c(preferred_order, unlist(lapply(records, names), use.names = FALSE)))

  records_filled <- lapply(records, function(rec) {
    for (nm in setdiff(all_names, names(rec))) rec[[nm]] <- NA_character_
    rec[all_names]
  })

  # ---- bind to data.frame safely (no list columns) ----
  row_dfs <- lapply(records_filled, function(rec) {
    as.data.frame(rec, stringsAsFactors = FALSE)
  })

  registry_df <- do.call(rbind, row_dfs)

  # ---- column order ----
  ordered_cols <- c(
    intersect(preferred_order, names(registry_df)),
    setdiff(names(registry_df), preferred_order)
  )
  registry_df <- registry_df[, ordered_cols, drop = FALSE]

  # ---- write CSV ----
  utils::write.csv(registry_df, output_csv, row.names = FALSE, na = "")

  message("Project registry CSV written to:\n", output_csv)

  invisible(registry_df)
}
