#' Create a new presentation
#'
#' This function creates a new .qmd file for a RevealJS presentation using the Baylor Statistics Consulting Center theme.
#'
#' @param file_name The name of the file to create (e.g., "presentation.qmd"). If it does not end in .qmd, the extension will be appended.
#' @param title The title of the presentation.
#' @param authors A character vector of author names.
#' @param subtitle The subtitle of the presentation (optional).
#'
#' @return The name of the created file (invisibly).
#' @export
create_presentation <- function(file_name, title, authors, subtitle = "") {
    # Ensure file_name ends with .qmd
    if (!grepl("\\.qmd$", file_name, ignore.case = TRUE)) {
        file_name <- paste0(file_name, ".qmd")
    }

    # Prepare authors string
    # For YAML, we can use a list format: ["Author 1", "Author 2"]
    authors_list_str <- paste0("[", paste(paste0("\"", authors, "\""), collapse = ", "), "]")

    # --- Copy Assets to quarto-assets/ ---
    asset_dir <- "quarto-assets"
    if (!dir.exists(asset_dir)) {
        dir.create(asset_dir)
    }

    # 1. Logo
    logo_source <- system.file("images", "baylor_logo_horizontal.png", package = "busccR")
    if (logo_source != "") {
        file.copy(logo_source, file.path(asset_dir, "baylor.png"), overwrite = TRUE)
    } else {
        warning("Horizontal logo (baylor_logo_horizontal.png) not found in package.")
    }

    # 2. Theme SCSS
    theme_source <- system.file("rmarkdown", "templates", "bscc_presentation", "skeleton", "presentation-theme.scss", package = "busccR")
    if (theme_source != "") {
        file.copy(theme_source, file.path(asset_dir, "presentation-theme.scss"), overwrite = TRUE)
    } else {
        warning("Theme file (presentation-theme.scss) not found in package.")
    }

    # 3. Macros TeX
    macros_source <- system.file("rmarkdown", "templates", "bscc_presentation", "skeleton", "_macros.tex", package = "busccR")
    if (macros_source != "") {
        file.copy(macros_source, file.path(asset_dir, "_macros.tex"), overwrite = TRUE)
    } else {
        warning("Macros file (_macros.tex) not found in package.")
    }

    # --- Generate Content ---

    content <- paste0(
        "---\n",
        "format: \n",
        "  revealjs:\n",
        "    theme: [simple, quarto-assets/presentation-theme.scss]\n",
        "    footer: \"Baylor Statistics Consulting Center\"\n",
        "    embed-resources: true\n",
        "    slide-number: true\n",
        "    auto-animate: true\n",
        "    chalkboard: false\n",
        "    preview-links: false\n",
        "    multiplex: false\n",
        "    scrollable: true\n",
        "title: \"", title, "\"\n",
        "subtitle: \"", subtitle, "\"\n",
        "author: ", authors_list_str, "\n",
        "toc: false\n",
        "toc-depth: 1\n",
        "execute:\n",
        "  echo: true\n",
        "  cache: false\n",
        "knitr:\n",
        "  opts_chunk: \n",
        "    fig-format: svg\n",
        "    collapse: true\n",
        "    comment: \"#\"\n",
        "logo: \"quarto-assets/baylor.png\"\n",
        "---\n\n",
        "::: {.content-hidden}\n",
        "$$\n",
        "{{< include quarto-assets/_macros.tex >}}\n",
        "$$\n",
        ":::\n\n",
        "## Introduction\n\n",
        "This is a RevealJS presentation using the BSCC theme.\n\n",
        "## Slide 1\n\n",
        "- Bullet point 1\n",
        "- Bullet point 2\n\n",
        "## Slide 2\n\n",
        "Content goes here.\n"
    )

    writeLines(content, file_name)
    message("Created presentation: ", file_name)
    invisible(file_name)
}
