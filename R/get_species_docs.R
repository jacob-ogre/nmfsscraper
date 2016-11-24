get_all_species_pdfs <- function(urls) {
  res <- lapply(urls, get_species_pdfs)
  return(res)
}

get_species_pdfs <- function(url, domain = "") {
  if(domain == "") {
    domain <- paste(strsplit(url, "/")[[1]][1:3], collapse = "/")
  }
  url <- URLencode(url)
  page <- xml2::read_html(url)
  atag <- rvest::html_nodes(page, "a")
  href <- rvest::html_attr(atag, "href")
  pdfs <- href[grep(href, pattern = "pdf$|PDF$")]
  pdfs <- ifelse(grepl(pdfs, pattern = "^http"),
                 pdfs,
                 paste0(domain, pdfs))
  return(pdfs)
}