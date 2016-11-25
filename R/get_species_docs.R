#' Get all PDF URLs from query URL
#'
#' Function description; defaults to title if left blank
#'
#' @param url The URL to be queried for PDF links
#' @importFrom  xml2 read_html
#' @importFrom  rvest html_nodes html_attr
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
  return(unique(pdfs))
}

#' Download all PDF documents from NMFS for a species
#'
#' @details Uses \link{get_species_pdf_urls} to fetch a vector of PDF URLs for
#' species documents maintained by the National Marine Fisheries Service (NMFS).
#' Filenames are the \link{basename} of the URL with spaces replaced by "_".
#' Uses \link[pdfdown]{pdfdown}, which returns a data.frame of results, to
#' do the scraping.
#'
#' @param url The URL to query for PDF links
#' @param subd The directory (subdirectory) to which the PDFs are downloaded
#' @return An augmented data.frame from \link[pdfdown]{pdfdown} with:
#'   \describe{
#'     \item{url}{Document URL}
#'     \item{dest}{Path to document}
#'     \item{success}{One of Success, Failed, Pre-exist}
#'     \item{pdfCheck}{TRUE if a real PDF, else FALSE}
#'     \item{taxon}{The taxon represented, from the URL}
#'   }
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' \dontrun{
#'   url <- "http://www.nmfs.noaa.gov/pr/species/turtles/green.html"
#'   dl_res <- get_species_pdfs(url, "~/Downloads/NMFS_rec")
#' }
download_species_pdfs <- function(url, subd = "") {
  all_species_pdfs <- get_species_pdf_urls(url)
  res <- lapply(all_species_pdfs, pdfdown::download_pdf, subd = subd)
  res <- dplyr::bind_rows(res)
  spp_pt <- strsplit(gsub(url,
                          pattern = "\\.htm$|\\.html$",
                          replacement = ""),
                     split="/")
  idx <- length(spp_pt[[1]])
  spp <- paste(spp_pt[[1]][(idx-1):idx], collapse=":")
  res$taxon <- rep(spp, length(res[[1]]))
  return(res)
}

#' Download PDFs of documents linked on NMFS species pages
#'
#' @note This function will only get documents linked from pages linked to
#'   NMFS's Protected Resources ESA-listed species page,
#'   \url{http://www.nmfs.noaa.gov/pr/species/esa/listed.htm}. In general this
#'   means recovery plans and many \emph{Federal Register} documents will not
#'   be gathered.
#' @importFrom dplyr bind_rows
#' @export
#' @examples
#' \dontrun{
#'   download_all_species_pdfs()
#' }
download_all_species_pdfs <- function() {
  urls <- get_species_pages_links()
  res <- lapply(urls, download_species_pdfs)
  res <- dplyr::bind_rows(res)
  return(res)
}
