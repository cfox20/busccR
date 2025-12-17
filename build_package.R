# Build and Install busccR Package

# 1. Document the package (updates NAMESPACE and .Rd files)
if (requireNamespace("devtools", quietly = TRUE)) {
    message("Documenting package...")
    devtools::document()
} else if (requireNamespace("roxygen2", quietly = TRUE)) {
    message("Documenting package (roxygen2)...")
    roxygen2::roxygenize()
} else {
    warning("Neither 'devtools' nor 'roxygen2' is installed. Documentation may be outdated.")
}

# 2. Install the package
message("Installing package...")
if (requireNamespace("devtools", quietly = TRUE)) {
    devtools::install(upgrade = "never")
} else {
    install.packages(".", repos = NULL, type = "source")
}

# 3. Load the package
message("Loading busccR...")
library(busccR)

message("SUCCESS: busccR has been documented, installed, and loaded.")
message("You can now test functions like: create_report(...)")
