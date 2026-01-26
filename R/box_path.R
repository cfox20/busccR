#' Set the Box root folder for the Statistical Consulting Center
#'
#' Configure the local path to the Statistical Consulting Center Box folder.
#' The path is stored in a user-specific configuration file.
#'
#' @details
#' On macOS, the Box Drive path is typically:
#' \code{/Users/$USER/Library/CloudStorage/Box-Box}
#'
#' @param path Optional. A character string specifying the path to the Box root.
#'   If \code{NULL} (the default), a directory selection dialog will be opened.
#'
#' @return Invisibly returns the normalized path to the selected Box root directory.
#' @export
set_box_root <- function(path = NULL) {
  # 1. If path is provided directly (programmatic use)
  if (!is.null(path)) {
    if (!dir.exists(path)) {
      stop("The provided directory does not exist: ", path, call. = FALSE)
    }
  } else {
    # 2. Interactive Selection
    if (!interactive()) {
      stop(
        "Argument `path` is missing and session is not interactive.",
        call. = FALSE
      )
    }

    # --- macOS Hint ---
    if (Sys.info()[["sysname"]] == "Darwin") {
      user <- Sys.info()[["user"]]
      message(
        "Tip: On macOS, the Box path is usually: /Users/",
        user,
        "/Library/CloudStorage/Box-Box"
      )
    }

    # Method A: RStudio API (Preferred)
    if (
      requireNamespace("rstudioapi", quietly = TRUE) &&
        rstudioapi::isAvailable()
    ) {
      # Inform user before opening picker
      tryCatch(
        {
          rstudioapi::showDialog(
            title = "Select Box Folder",
            message = "Please select the 'Statistical Consulting Center' Box folder on your computer."
          )
        },
        error = function(e) {}
      )

      # Wrap in tryCatch in case API is technically available but fails (e.g. testing)
      path <- tryCatch(
        {
          rstudioapi::selectDirectory(caption = "Select SCC Box Root Folder")
        },
        error = function(e) {
          NULL
        }
      )
    }

    # Method B: Windows Native (if Method A failed or wasn't available)
    if (is.null(path) && .Platform$OS.type == "windows") {
      path <- utils::choose.dir(caption = "Select SCC Box Root Folder")
    }

    # Method C: Tcl/Tk (Mac/Linux X11)
    if (
      is.null(path) &&
        capabilities("tcltk") &&
        requireNamespace("tcltk", quietly = TRUE)
    ) {
      path <- tcltk::tk_choose.dir(caption = "Select SCC Box Root Folder")
    }

    # Method D: Console Fallback
    if (is.null(path)) {
      message("Directory selection dialog not available.")
      path <- readline(prompt = "Enter the full path to the SCC Box folder: ")
    }
  }

  # 3. Validate Selection
  if (is.null(path) || is.na(path) || !nzchar(path)) {
    stop("No directory selected. Configuration not saved.", call. = FALSE)
  }

  # Normalize path (expand ~/, convert backslashes)
  full_path <- normalizePath(path, winslash = "/", mustWork = TRUE)

  # 4. Save Configuration
  cfg_dir <- tools::R_user_dir("busccR", which = "config")
  if (!dir.exists(cfg_dir)) {
    dir.create(cfg_dir, recursive = TRUE)
  }

  cfg_file <- file.path(cfg_dir, "config.json")

  cfg_data <- list(
    box_root = full_path,
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    user = Sys.info()[["user"]]
  )

  jsonlite::write_json(cfg_data, cfg_file, pretty = TRUE, auto_unbox = TRUE)

  message("Box root set to: ", full_path)
  invisible(full_path)
}


#' Get the configured Box root folder
#'
#' Retrieves the stored path to the Statistical Consulting Center Box folder.
#'
#' @param error_if_missing Logical. If \code{TRUE} (default), the function throws
#'   an error (or prompts in interactive sessions) if the path is not set.
#'   If \code{FALSE}, it returns \code{NULL}.
#' @param allow_interactive Logical. If \code{TRUE} (default), the function will
#'   prompt the user to set the Box root if it is missing and the session is interactive.
#'   Set to \code{FALSE} to force an error in tests or scripts.
#'
#' @return A character string containing the normalized path, or \code{NULL}.
#' @export
get_box_root <- function(
  error_if_missing = TRUE,
  allow_interactive = interactive()
) {
  cfg_file <- file.path(
    tools::R_user_dir("busccR", which = "config"),
    "config.json"
  )

  path <- NULL

  # 1. Attempt to read config
  if (file.exists(cfg_file)) {
    tryCatch(
      {
        cfg <- jsonlite::read_json(cfg_file, simplifyVector = TRUE)
        if (!is.null(cfg$box_root) && nzchar(cfg$box_root)) {
          path <- cfg$box_root
        }
      },
      error = function(e) {
        warning("Could not read config file: ", e$message, call. = FALSE)
      }
    )
  }

  # 2. Verify existence on disk
  if (!is.null(path)) {
    if (!dir.exists(path)) {
      warning("Configured Box path no longer exists: ", path, call. = FALSE)
      path <- NULL
    } else {
      # Ensure canonical path
      path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    }
  }

  # 3. Handle missing path
  if (is.null(path)) {
    if (error_if_missing) {
      # Only prompt if allowed AND interactive
      if (allow_interactive) {
        message("Box root is not configured or invalid.")
        return(set_box_root())
      } else {
        stop(
          "Box root is not configured. Run `set_box_root()` first.",
          call. = FALSE
        )
      }
    } else {
      return(NULL)
    }
  }

  path
}
