library(readxl)
library(stringr)

# set paths
main = "C:/Till/Repositories/GitHub/academic_website"
input = "C:/Till/Repositories/GitHub/academic_website"

# Read publications
pubs <- read_excel(paste(input, "Publications.xlsx", sep="/"))

# Helper: safe filename creation
make_filename <- function(year, title_short, outlet) {
  title_clean  <- str_replace_all(title_short, "[^A-Za-z0-9]+", "-")
  outlet_clean <- str_replace_all(outlet, "[^A-Za-z0-9]+", "-")
  paste0(year, "-", title_clean, "-", outlet_clean, ".md")
}

# Ensure folders exist
if (!dir.exists("_publications")) dir.create("_publications")
if (!dir.exists("_policy")) dir.create("_policy")

# Loop through publications
for (i in 1:nrow(pubs)) {
  
  row <- pubs[i, ]
  
  # Bold your name everywhere in authors
  authors_fmt <- str_replace(row$`List authors`, "Köveker, T\\.", "<strong>Köveker, T.</strong>")
  
  # Abstract → excerpt field (fallback empty if missing)
  excerpt_text <- ifelse(!is.na(row$Abstract), row$Abstract, "")
  
  # German link
  german_block <- ""
  if (!is.na(row$`Title German`) && !is.na(row$`Link German`)) {
    german_block <- paste0("\n\n[German version](", row$`Link German`, ")")
  }
  
  # Choose YAML structure
  if (row$Type == "Journal" | row$Type == "Policy") {
    
    md <- c(
      "---",
      paste0('title: "', row$Title, '"'),
      'collection: publications',
      'category: Publications',
      "permalink:",
      paste0("date: ", row$Year, "-01-01"),
      paste0('authors: "', authors_fmt, '"'),
      paste0('venue: "', row$`Journal/Outlet`, '"'),
      'status_note: ""',
      paste0('paperurl: "', row$Link, '"'),
      paste0('excerpt: "', excerpt_text, '"'),
      "---",
      german_block
    )
    
  } else if (row$Type == "Discussion paper") {
    
    md <- c(
      "---",
      paste0('title: "', row$Title, '"'),
      'collection: publications',
      paste0("permalink: ", row$Link),
      paste0("date: ", row$Year, "-01-01"),
      paste0('authors: "', authors_fmt, '"'),
      paste0('venue: "', row$`Journal/Outlet`, '"'),
      paste0('paperurl: "', row$Link, '"'),
      'category: "Working papers"',
      paste0('excerpt: "', excerpt_text, '"'),
      "---",
      german_block
    )
  } else {
    next
  }
  
  # Create filename
  filename <- make_filename(row$Year, row$Title, row$`Journal/Outlet`)
  
  # Select output folder
  if (row$Type == "Policy") {
    outfile <- file.path(paste(main, "_policy", sep = "/"), filename)
  } else {
    outfile <- file.path(paste(main, "_publications", sep = "/"), filename)
  }
  
  # Write file
  writeLines(md, outfile)
}