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
    end_date = NA_character_,          # set only in complete_project()
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
    project_path = project_path,       # relative to get_box_root()
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
