# Build a CSV registry of all consulting projects (base R only)

Reads all JSON project records from the `project_registry` folder under
the Box root and compiles them into a single CSV file named
`project_registry.csv`, stored in the Box root directory.

## Usage

``` r
build_registry(overwrite = FALSE)
```

## Arguments

- overwrite:

  Logical; if `FALSE` (default) and `project_registry.csv` already
  exists, the function stops. Set to `TRUE` to overwrite the existing
  file.

## Value

Invisibly returns a data.frame containing the compiled registry.

## Details

This implementation uses base R only (no package dependencies). Since
base R does not provide a full JSON parser, it supports the simple
schema used by this package: top-level scalar fields and arrays of
strings (keywords, methods, topics, consultants).

## Examples

``` r
if (FALSE) { # \dontrun{
build_registry()
build_registry(overwrite = TRUE)
} # }
```
