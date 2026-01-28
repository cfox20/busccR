# Update an existing consulting project record

Updates fields in an existing JSON project record stored in the project
registry. Commonly updated fields include `status`, `keywords`,
`methods`, `abstract`, and the project file path.

## Usage

``` r
update_project(
  id = NULL,
  status = NULL,
  keywords = NULL,
  methods = NULL,
  abstract = NULL,
  project_path = NULL,
  append = FALSE,
  overwrite = FALSE
)
```

## Arguments

- id:

  Project identifier (JSON filename without extension). If `NULL`, the
  user is prompted to select a project JSON file.

- status:

  Optional new status. If `NULL`, status is unchanged.

- keywords:

  Optional character vector of keywords.

- methods:

  Optional character vector of methods.

- abstract:

  Optional new abstract.

- project_path:

  Optional. Use `"choose"` to select a new project folder via the file
  system. If `NULL`, the project path is unchanged.

- append:

  Logical; if `TRUE`, `keywords` and `methods` are appended
  (deduplicated). Defaults to `FALSE`.

- overwrite:

  Logical; skip the concurrency check if `TRUE`.

## Value

Invisibly returns a list with `id`, `registry_file`, and
`updated_fields`.
