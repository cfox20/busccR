# Create a new report

This function creates a new .qmd file with a standardized YAML header
for Baylor Statistics Consulting Center projects.

## Usage

``` r
create_report(
  file_name,
  title,
  client,
  authors,
  email = "rodney_strudivant@baylor.edu",
  phone = "(254) 710-1663",
  keywords = NULL
)
```

## Arguments

- file_name:

  The name of the file to create (e.g., "analysis.qmd"). If it does not
  end in .qmd, the extension will be appended.

- title:

  The title of the project.

- client:

  The name of the client.

- authors:

  A character vector of author names.

- email:

  The email address for the contact info (default:
  "rodney_strudivant@baylor.edu").

- phone:

  The phone number for the contact info (default: "(254) 710-1663").

- keywords:

  Optional keywords to include in the report.

## Value

The name of the created file (invisibly).

## Examples

``` r
if (FALSE) { # \dontrun{
create_report("project_analysis", "New Project", "Dr. Client", c("Student A", "Student B"))
} # }
```
