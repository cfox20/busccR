# Complete a consulting project record

Marks a project as complete by:

- setting `end_date` (defaults to today),

- prompting the user to provide or confirm `methods`,

- setting `status` to `"complete"`.

## Usage

``` r
complete_project(id = NULL, end_date = Sys.Date(), overwrite = FALSE)
```

## Arguments

- id:

  Project identifier (JSON filename without extension). If `NULL`, the
  user is prompted to select a project JSON file.

- end_date:

  Project end date. Defaults to
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html). May be a `Date`
  or a character string coercible to `Date`.

- overwrite:

  Logical; if `FALSE` (default), performs an optimistic concurrency
  check using file modification time. Set to `TRUE` to skip this check.

## Value

Invisibly returns a named list containing:

- `id`

- `registry_file`

- `end_date`

- `methods`

## Details

The canonical JSON project record is updated in
`<Box Root>/project_registry/`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Complete with today's date
complete_project(id = "2026sp_student_success_jdoe_example_edu")

# Complete with a specified date
complete_project(
  id = "2026sp_student_success_jdoe_example_edu",
  end_date = "2026-02-15"
)
} # }
```
