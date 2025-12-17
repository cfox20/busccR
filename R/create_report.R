#' Create a new report
#'
#' This function creates a new .qmd file with a standardized YAML header for Baylor Statistics Consulting Center projects.
#'
#' @param file_name The name of the file to create (e.g., "analysis.qmd"). If it does not end in .qmd, the extension will be appended.
#' @param title The title of the project.
#' @param client The name of the client.
#' @param authors A character vector of author names.
#'
#' @param email The email address for the contact info (default: "rodney_strudivant@baylor.edu").
#' @param phone The phone number for the contact info (default: "(254) 710-1663").
#' @param keywords Optional keywords to include in the report.
#'
#' @return The name of the created file (invisibly).
#' @export
#'
#' @examples
#' \dontrun{
#' create_report("project_analysis", "New Project", "Dr. Client", c("Student A", "Student B"))
#' }
create_report <- function(file_name, title, client, authors,
                          email = "rodney_strudivant@baylor.edu",
                          phone = "(254) 710-1663",
                          keywords = NULL) {
  # Ensure file_name ends with .qmd
  if (!grepl("\\.qmd$", file_name, ignore.case = TRUE)) {
    file_name <- paste0(file_name, ".qmd")
  }

  # Prepare authors for params (list format for YAML is handled by R's yaml parsing if we used it,
  # but here we are constructing string manually.
  # For the params block, we want a list if multiple, or string if single.
  authors_list_str <- paste0("[", paste(paste0("\"", authors, "\""), collapse = ", "), "]")

  # Format keywords block if present
  if (!is.null(keywords) && length(keywords) > 0) {
    keywords_tex <- paste0("\\vspace{0.25cm}\n\n  {\\small Keywords: ", paste(keywords, collapse = ", "), "\\par}\n")
  } else {
    keywords_tex <- ""
  }

  # Escape underscores in email
  email <- gsub("_", "\\\\_", email)

  # Check and copy logo
  if (!dir.exists("images")) {
    dir.create("images")
  }

  logo_source <- system.file("images", "bscc_logo_stacked.png", package = "busccR")
  if (logo_source == "") {
    warning("Logo not found in package. Using placeholder or ensuring local file exists.")
  } else {
    file.copy(logo_source, "images/bscc_logo_stacked.png", overwrite = FALSE)
  }

  # Create the file content
  content <- paste0(
    "---\n",
    "format: \n",
    "  pdf:\n",
    "    documentclass: scrreprt\n",
    "    toc: false\n",
    "    colorlinks: true\n",
    "    linkcolor: black\n",
    "    urlcolor: black\n",
    "    mainfont: \"Times New Roman\"\n",
    "    sansfont: \"Times New Roman\"\n",
    "    monofont: \"Times New Roman\"\n",
    "    geometry: margin=1in\n",
    "    include-in-header: \n",
    "      text: |\n",
    "        \\usepackage{tikz}\n",
    "        \\usetikzlibrary{calc}\n",
    "        \\definecolor{bugreen}{HTML}{154734}\n",
    "        \\KOMAoptions{toc=chapterentrywithdots}\n",
    "params:\n",
    "  title: \"", title, "\"\n",
    "  client: \"", client, "\"\n",
    "  authors: ", authors_list_str, "\n",
    "  date: \"", Sys.Date(), "\"\n",
    "---\n\n",
    "\\begin{titlepage}\n",
    "\\thispagestyle{empty}\n\n",
    "% ------- border frame + bottom logo/contact --------\n",
    "\\begin{tikzpicture}[remember picture,overlay, draw=bugreen]\n\n",
    "  % define corners of the frame\n",
    "  \\coordinate (NW) at ($(current page.north west)+(1.2cm,-1.5cm)$);\n",
    "  \\coordinate (NE) at ($(current page.north east)+(-1.2cm,-1.5cm)$);\n",
    "  \\coordinate (SW) at ($(current page.south west)+(1.2cm,1.5cm)$);\n",
    "  \\coordinate (SE) at ($(current page.south east)+(-1.2cm,1.5cm)$);\n\n",
    "  % Main Content in a Node to measure height for gap\n",
    "  % Positioned relative to NW. The xshift puts it slightly right of the border.\n",
    "  \\node[anchor=north west, inner sep=0pt, align=left] (ContentNode) at ($(NW) + (0.8cm, -3cm)$) {%\n",
    "    \\begin{minipage}{0.7\\textwidth}\n",
    "      {\\bfseries\\fontsize{26}{32}\\selectfont\n",
    "        ", toupper(title), "\\par}\n\n",
    "      \\vspace{0.25cm}\n\n",
    "      {\\small Authors: ", paste(authors, collapse = ", "), "\\par}\n\n",
    "      \\vspace{0.25cm}\n\n",
    "      {\\small Client: ", client, "\\par}\n\n",
    "      ", keywords_tex, "\n",
    "      \\vspace{0.25cm}\n\n",
    "      \\today\\par\n",
    "    \\end{minipage}%\n",
    "  };\n\n",
    "  % Calculate Gap coordinates based on ContentNode\n",
    "  % We split the calculation to avoid complex calc syntax issues\n",
    "  \\coordinate (TitleTop) at (NW |- ContentNode.north);\n",
    "  \\coordinate (TitleBottom) at (NW |- ContentNode.south);\n",
    "  \\coordinate (GapTop)    at ($(TitleTop) + (0, 0.5cm)$);\n",
    "  \\coordinate (GapBottom) at ($(TitleBottom) + (0, -0.5cm)$);\n\n",
    "  % draw top border\n",
    "  \\draw[line width=0.4pt] (NW) -- (NE);\n",
    "  % draw right border\n",
    "  \\draw[line width=0.4pt] (NE) -- (SE);\n",
    "  % draw bottom border\n",
    "  \\draw[line width=0.4pt] (SE) -- (SW);\n",
    "  % draw left border with dynamic gap\n",
    "  \\draw[line width=0.4pt] (NW) -- (GapTop);\n",
    "  \\draw[line width=0.4pt] (GapBottom) -- (SW);\n\n",
    "% contact info bottom-left (inside frame)\n",
    "\\node[anchor=south west] at ($(SW)+(0.2cm,0.3cm)$) {%\n",
    "  \\scriptsize\n",
    "  \\parbox{4cm}{%\n",
    "    ", email, " \\par\n",
    "    ", phone, " \\par\n",
    "    \\href{https://statistics.artsandsciences.baylor.edu/consulting}{Statistical Consulting Center}\n",
    "  }\n",
    "};\n\n",
    "  % logo bottom-right (inside frame)\n",
    "  \\node[anchor=south east] at ($(SE)+(-0.2cm,0.3cm)$) {%\n",
    "    \\includegraphics[width=5cm]{images/bscc_logo_stacked.png}\n",
    "  };\n",
    "\\end{tikzpicture}\n\n",
    "\\end{titlepage}\n",
    "\\clearpage\n",
    "\\tableofcontents\n",
    "\\clearpage\n\n",
    "# Introduction\n\n",
    "# Methods\n\n",
    "# Results\n\n",
    "# Conclusion\n\n",
    "\\newpage\n\n",
    "# References\n\n",
    "\\newpage\n\n",
    "# Apppendix \n"
  )

  # Write to file
  writeLines(content, file_name)

  message("Created report: ", file_name)
  invisible(file_name)
}
