# Getting Started With busccR

## Installation

busccR is available for download via github.

``` r

devtools::install_github("cfox20/busccR")
```

## Box Drive Setup

Project tracking within the package relies on local access to the
Statistical Consulting box folder. Box Drive must be installed and setup
with your Baylor account to provide access to the folder through your
local file path. Installation instructions can be found
[here](https://support.box.com/hc/en-us/articles/360043697474-Installing-and-Updating-Box-Drive).

When using the package, the
[`set_box_root()`](https://cfox20.github.io/busccR/reference/set_box_root.md)
will prompt the user to choose the root folder for the statistical
consulting center folder. The prompt will open the file explorer and the
user should navigate to the “Statistical Consulting Center” and then
select the folder for the package to find the necessary folders.

## Project Tracking

The status and information of consulting projects are stored within a
folder called `project_registry` which contains a JSON file for each
project with all information. Available functions within `busccR` to
update this information include: - create_project() - update_project() -
complete_project()

The
[`build_registry()`](https://cfox20.github.io/busccR/reference/build_registry.md)
function then compiles all the JSON files in `project_registry` into a
csv file called project_registry.csv which contains information about
all projects.

## Templates

New templates are included in the package to provide consistency in
reporting. Currently there is one template for reports and one for
presentations. More templates may be addded later.

### Report Templates

A new report is created with the
[`create_report()`](https://cfox20.github.io/busccR/reference/create_report.md)
function. This function creates a qmd file in the current directory
containing the project information (this may eventually be called inside
the
[`create_project()`](https://cfox20.github.io/busccR/reference/create_project.md)
function).

### Presentation Templates

Presentations are created with the
[`create_presentation()`](https://cfox20.github.io/busccR/reference/create_presentation.md)
function which creates a qmd file in the current directory using
revealjs for the presentation.
