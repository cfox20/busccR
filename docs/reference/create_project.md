# Create a new consulting project

Creates a new consulting project by:

- prompting the user to select the project folder (relative to the SCC
  Box root),

- generating a project ID of the form `term_department_contact`,

- writing a canonical JSON project record to the project registry.

## Usage

``` r
create_project(
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
)
```

## Arguments

- project_name:

  Project name (title).

- term:

  Academic term label (e.g., `"2026SP"`). If `NULL`, the term is
  inferred from the system date.

- start_date:

  Optional project start date. Defaults to
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- category:

  Optional category label (e.g., `"Education"`).

- contact:

  Primary contact name or email. Used to form the project ID.

- department:

  Department name. Used to form the project ID.

- organization:

  Optional organization name.

- status:

  Initial project status. Defaults to `"intake"`.

- consultants:

  Character vector of consultant names. Defaults to the current system
  user.

- notes:

  String of any additional notes about the project.

## Value

Invisibly returns a named list containing:

- `project_id`

- `project_path` (relative to the Box root)

- `registry_file` (full path to the JSON file)

## Details

The project registry is stored at `<Box Root>/project_registry/`, with
one JSON file per project named `<project_id>.json`.

## Examples

``` r
if (FALSE) { # \dontrun{
create_project(
  project_name = "Student retention analysis",
  consultants = c("Jane Doe", "A. Smith"),
  category = "Education",
  department = "Student Success",
  contact = "jdoe@example.edu",
  organization = "Example University"
)
} # }
```
