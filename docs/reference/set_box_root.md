# Set the Box root folder for the Statistical Consulting Center

Configure the local path to the Statistical Consulting Center Box
folder. The path is stored in a user-specific configuration file.

## Usage

``` r
set_box_root(path = NULL)
```

## Arguments

- path:

  Optional. A character string specifying the path to the Box root. If
  `NULL` (the default), a directory selection dialog will be opened.

## Value

Invisibly returns the normalized path to the selected Box root
directory.

## Details

On macOS, the Box Drive path is typically:
`/Users/$USER/Library/CloudStorage/Box-Box`
