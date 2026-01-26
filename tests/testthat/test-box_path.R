library(testthat)
library(busccR)

test_that("Box root configuration works correctly", {
  # --- Setup: Define paths and safely backup existing user config ---
  cfg_dir <- tools::R_user_dir("busccR", which = "config")
  cfg_file <- file.path(cfg_dir, "config.json")

  # Rename existing config if present
  backup_file <- paste0(cfg_file, ".bak")
  has_existing_config <- file.exists(cfg_file)

  if (has_existing_config) {
    file.rename(cfg_file, backup_file)
  }

  # Register cleanup
  on.exit(
    {
      if (file.exists(cfg_file)) {
        unlink(cfg_file)
      }
      if (has_existing_config && file.exists(backup_file)) {
        file.rename(backup_file, cfg_file)
      }
    },
    add = TRUE
  )

  # --- Create a temporary "Box Root" ---
  mock_root <- tempfile(pattern = "mock_box_root")
  dir.create(mock_root)
  on.exit(unlink(mock_root, recursive = TRUE), add = TRUE)

  # --- Test Case 1: set_box_root rejects invalid paths ---
  expect_error(
    set_box_root(path = file.path(tempdir(), "non_existent_folder_xyz")),
    "does not exist"
  )

  # --- Test Case 2: set_box_root saves valid configuration ---
  expect_message(
    returned_path <- set_box_root(path = mock_root),
    "Box root set to"
  )

  expected_path <- normalizePath(mock_root, winslash = "/")
  expect_equal(normalizePath(returned_path, winslash = "/"), expected_path)

  expect_true(file.exists(cfg_file))
  saved_cfg <- jsonlite::read_json(cfg_file)
  expect_equal(normalizePath(saved_cfg$box_root, winslash = "/"), expected_path)

  # --- Test Case 3: get_box_root retrieves the path ---
  retrieved_path <- get_box_root()
  expect_equal(retrieved_path, expected_path)

  # --- Test Case 4: get_box_root handles broken paths (folder deleted) ---
  unlink(mock_root, recursive = TRUE)

  # Warning + NULL return
  expect_warning(
    val <- get_box_root(error_if_missing = FALSE),
    "Configured Box path no longer exists"
  )
  expect_null(val)

  # Error (with allow_interactive = FALSE to stop it from trying to pop up a dialog)
  expect_error(
    get_box_root(error_if_missing = TRUE, allow_interactive = FALSE),
    "Box root is not configured"
  )

  # --- Test Case 5: get_box_root handles missing config file ---
  unlink(cfg_file)

  expect_null(get_box_root(error_if_missing = FALSE))
  expect_error(get_box_root(error_if_missing = TRUE, allow_interactive = FALSE))
})
