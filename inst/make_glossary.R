
# Extract the needed pages, 9-68:
cmd <- "pdftk inst/extdata/FishGlossary.pdf cat 9-68 output inst/extdata/NMFS_glossary.pdf"
res <- try(system(cmd), silent = TRUE)
if(class(res[1]) == "try-error") stop("Error extracting pages.")

# Convert to text using pdftotext:
cmd <- "pdftotext -r 300 -nopgbrk inst/extdata/NMFS_glossary.pdf inst/extdata/NMFS_glossary.txt"
res <- try(system(cmd), silent = TRUE)
if(class(res[1]) == "try-error") stop("Error extracting text.")

##################
# FILE MANUALLY EDITED TO MARK GLOSSARY TERMS FOR EXTRACTION
# Way too much noise to efficiently write a regex to separate the terms from
# their definitions.

# read in the text:
txt <- readLines("inst/extdata/NMFS_glossary_tagged.txt")
head(txt, 40)


empties <- grep(txt, pattern = "^$")
numlines <- grep(txt, pattern = "^[0-9,]+$")
dateline <- grep(txt, pattern = "JUNE 2006")
footers <- grep(txt, pattern = "NOAA FISHERIES GLOSSARY")
alphalines <- grep(txt, pattern = "^[A-Z]$")
removes <- c(1, empties, numlines, dateline, footers, alphalines)
clean <- txt[-removes]
head(clean, 30)

term_idx <- grep(clean, pattern = "^- ")
terms <- clean[term_idx]
terms <- gsub(terms, pattern = "^- ", replacement = "")
head(terms, 30)

defs_idx <- grep(clean, pattern = "^[^- ]")
defs <- c()
cur_def <- c()
for(i in 1:length(defs_idx)) {
  if(length(cur_def) == 0) {
    cur_def <- c(clean[defs_idx[i]])
  } else if(i > 1 & (defs_idx[i] - 1 == defs_idx[i-1])) {
    cur_def <- c(cur_def, clean[defs_idx[i]])
  } else {
    defs <- c(defs, paste(cur_def, collapse = " "))
    cur_def <- c(clean[defs_idx[i]])
  }
}
defs <- c(defs, paste(cur_def, collapse = " "))
head(defs)
length(defs)

NMFS_glossary <- tibble::tibble(term = terms,
                                definition = defs)
head(data.frame(NMFS_glossary))

devtools::use_data(NMFS_glossary)
