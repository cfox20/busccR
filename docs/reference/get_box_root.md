# Get the configured Box root folder

Retrieves the stored path to the Statistical Consulting Center Box
folder.

## Usage

``` r
get_box_root(error_if_missing = TRUE, allow_interactive = interactive())
```

## Arguments

- error_if_missing:

  Logical. If `TRUE` (default), the function throws an error (or prompts
  in interactive sessions) if the path is not set. If `FALSE`, it
  returns `NULL`.

- allow_interactive:

  Logical. If `TRUE` (default), the function will prompt the user to set
  the Box root if it is missing and the session is interactive. Set to
  `FALSE` to force an error in tests or scripts.

## Value

A character string containing the normalized path, or `NULL`.
