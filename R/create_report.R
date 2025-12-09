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
    "  % choose where the left border \"gap\" should be\n",
    "  % (tweak these two offsets to surround your title vertically)\n",
    "  \\coordinate (GapTop)    at ($(NW)+(0,-4cm)$);   % end of upper left segment\n",
    "  \\coordinate (GapBottom) at ($(SW)+(0,13.5cm)$);    % start of lower left segment\n\n",
    "  % draw top border\n",
    "  \\draw[line width=0.4pt] (NW) -- (NE);\n",
    "  % draw right border\n",
    "  \\draw[line width=0.4pt] (NE) -- (SE);\n",
    "  % draw bottom border\n",
    "  \\draw[line width=0.4pt] (SE) -- (SW);\n",
    "  % draw left border with a gap around the title\n",
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
    "% ------- main content block (title + author info) -------\n\n",
    "\\vspace*{2.5cm}         % move content down\n",
    "\\hspace*{-1.2cm}% move content right so it aligns with inside of left border\n",
    "\\begin{minipage}[t]{0.7\\textwidth}\n\n",
    "  \\vspace{1.5cm} % adjust to taste\n\n",
    "  % big bold title, flush with border\n",
    "  {\\bfseries\\fontsize{26}{32}\\selectfont\n",
    "    ", toupper(title), "\\par}\n\n",
    "  \\vspace{0.25cm}\n\n",
    "  {\\small Authors: ", paste(authors, collapse=', '), "\\par}\n\n",
    "  \\vspace{0.25cm}\n\n",
    "  {\\small Client: ", client, "\\par}\n\n",
    keywords_tex,
    "  \\vspace{0.25cm}\n\n",
    "  \\today\\par\n\n",
    "\n",
    "\\end{minipage}\n\n",
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

