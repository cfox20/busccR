#' Set the Box root folder for the Statistical Consulting Center
#'
#' Prompts the user with a dialog instructing them to select the local
#' filesystem path to the Statistical Consulting Center Box folder, then
#' opens a directory chooser. The selected folder path is stored in a
#' user-level configuration file for reuse by other package functions.
#'
#' @details
#' In RStudio, this function displays a modal dialog first. After the user
#' clicks "Next", it opens a folder selection window.
#'
#' Outside of RStudio, the function prints an informational message and
#' uses an OS-native selection method when available.
#'
#' The configuration is stored per-user and does not affect other users.
#'
#' @return Invisibly returns the normalized path to the selected Box root directory.
#'
#' @examples
#' \dontrun{
#' set_box_root()
#' }
#'
#' @export
set_box_root <- function() {
  # ---- Step 1: user-facing instruction ----
  if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
    rstudioapi::showDialog(
      title = "Select Box Folder",
      message = "Select the local file path to the Statistical Consulting Center Box folder."
    )

    # If the user cancels the dialog, showDialog throws in some environments;
    # defend with a tryCatch.
    # (If it returns, proceed.)
  } else {
    message("Select the local file path to the Statistical Consulting Center Box folder.")
  }

  # ---- Step 2: open directory chooser ----
  path <- NULL

  if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
    path <- rstudioapi::selectDirectory(
      caption = "Select the Statistical Consulting Center Box folder"
    )
  } else if (.Platform$OS.type == "windows") {
    path <- utils::choose.dir(
      caption = "Select the Statistical Consulting Center Box folder"
    )
  } else {
    message("Please select any file within the Box folder (directory selection may not be available).")
    path <- dirname(utils::file.choose())
  }

  if (is.null(path) || !nzchar(path)) {
    stop("No directory selected. Box root was not set.", call. = FALSE)
  }

  path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  # ---- Step 3: persist config ----
  cfg_dir <- tools::R_user_dir("consultingpkg", which = "config")
  if (!dir.exists(cfg_dir)) {
    dir.create(cfg_dir, recursive = TRUE)
  }

  cfg_file <- file.path(cfg_dir, "config.json")

  cfg <- list(
    box_root = path,
    set_at   = format(Sys.time(), tz = "UTC"),
    user     = Sys.info()[["user"]]
  )

  jsonlite::write_json(cfg, cfg_file, pretty = TRUE, auto_unbox = TRUE)

  message("Box root directory set to:\n", path)

  invisible(path)
}


#' Get the configured Box root folder
#'
#' Returns the Box root directory previously set with
#' \code{set_box_root()}.
#'
#' @return
#' A character string containing the normalized Box root path.
#'
#' @export
get_box_root <- function() {
  cfg_file <- file.path(
    tools::R_user_dir("consultingpkg", which = "config"),
    "config.json"
  )

  # ---- config missing: prompt interactively ----
  if (!file.exists(cfg_file)) {
    if (!interactive()) {
      stop(
        "Box root is not set and this session is non-interactive.\n",
        "Run set_box_root() in an interactive session first.",
        call. = FALSE
      )
    }

    message("Box root is not set. Please select the Statistical Consulting Center Box folder.")
    set_box_root()
  }

  # ---- read config ----
  cfg <- jsonlite::read_json(cfg_file, simplifyVector = TRUE)

  # ---- configured path no longer exists ----
  if (!dir.exists(cfg$box_root)) {
    if (!interactive()) {
      stop(
        "Configured Box root directory no longer exists:\n",
        cfg$box_root,
        call. = FALSE
      )
    }

    message(
      "The configured Box root directory no longer exists.\n",
      "Please reselect the Statistical Consulting Center Box folder."
    )
    set_box_root()
    cfg <- jsonlite::read_json(cfg_file, simplifyVector = TRUE)
  }

  normalizePath(cfg$box_root, winslash = "/", mustWork = TRUE)
}
