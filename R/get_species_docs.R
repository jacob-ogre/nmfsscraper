#' Get all PDF URLs from query URL
#'
#' Function description; defaults to title if left blank
#'
#' @param url The URL to be queried for PDF links
#' @importFrom  package function(s)
#' @export
#' @examples
#' \dontrun{
#'    url <- "http://www.nmfs.noaa.gov/pr/species/turtles/green.html"
#'    chemyd_pdfs <- get_species_pdf_urls(url)
#' }
get_species_pdf_urls <- function(url) {
  domain <- paste(strsplit(url, "/")[[1]][1:3], collapse = "/")
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

#' Download all PDF recovery documents from NMFS
#'
#' @details Uses \link{get_plan_pdf_urls} to fetch the vector of PDF URLs for
#' recovery plans maintained by the National Marine Fisheries Service (NMFS).
#' Filenames are the \link{basename} of the URL with spaces replaced by "_".
#' Uses \link[pdfdown]{pdfdown}, which returns a data.frame of results, to
#' do the scraping. Note that \link[pdfdown]{download_pdf} will not
#'
#' @param subd The directory (subdirectory) to which the PDFs are downloaded
#' @export
#' @examples
#' \dontrun{
#' dl_res <- get_species_pdfs("~/Downloads/NMFS_rec")
#' }
get_species_pdfs <- function(subd = "") {
  all_plan_pdfs <- get_species_pdf_urls()
  res <- lapply(all_plan_pdfs, pdfdown::download_pdf, subd = subd)
  res <- dplyr::bind_rows(res)
  return(res)
}

get_all_species_pdfs <- function(urls) {
  res <- lapply(urls, get_species_pdfs)
  return(res)
}
