# Create a new presentation

This function creates a new .qmd file for a RevealJS presentation using
the Baylor Statistics Consulting Center theme.

## Usage

``` r
create_presentation(file_name, title, authors, subtitle = "")
```

## Arguments

- file_name:

  The name of the file to create (e.g., "presentation.qmd"). If it does
  not end in .qmd, the extension will be appended.

- title:

  The title of the presentation.

- authors:

  A character vector of author names.

- subtitle:

  The subtitle of the presentation (optional).

## Value

The name of the created file (invisibly).
