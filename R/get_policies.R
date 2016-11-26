#' Return NMFS's main policies page as an xml2 object
#'
get_policies_page <- function() {
  page <- xml2::read_html("http://www.nmfs.noaa.gov/pr/laws/esa/policies.htm")
}

#' Return PDF urls linked directly to NMFS's main policies page
#' @export
get_policies_pdf_urls <- function() {
  domain <- "http://www.nmfs.noaa.gov"
  page <- get_policies_page()
  cntr <- rvest::html_nodes(page, xpath = '//*[@id="center"]')
  atag <- rvest::html_nodes(cntr, "a")
  href <- rvest::html_attr(atag, "href")
  pdfs <- href[grep(href, pattern = "pdf$|PDF$")]
  pdfs <- ifelse(grepl(pdfs, pattern = "^http"),
                 pdfs,
                 paste0(domain, pdfs))
  return(pdfs)
}

#' Download all PDF policies documents from NMFS
#'
#' @details Uses \link{get_status_review_pdf_urls} to fetch the vector of PDF
#' URLs for policies maintained by the National Marine Fisheries
#' Service (NMFS). Filenames are the \link{basename} of the URL with spaces
#' replaced by "_". Uses \link[pdfdown]{pdfdown}, which returns a data.frame of
#' results, to do the scraping.
#'
#' @note Two points to note: \enumerate{
#'   \item{
#'     \link[pdfdown]{download_pdf} checks for duplicate files by name and
#'       skips the download by default if the file exists;
#'   }
#'   \item{
#'       By extension, \link[pdfdown]{download_pdf} does not check for duplicate
#'       files based on, e.g., MD5 hashes. Yet.
#'   }
#' }
#'
#' @param subd The directory (subdirectory) to which the PDFs are downloaded
#' @export
#' @examples
#' \dontrun{
#'   dl_res <- download_policies("~/Downloads/NMFS_rec")
#' }
download_policies <- function(subd = "") {
  all_policy_pdfs <- get_policies_pdf_urls()
  res <- lapply(all_policy_pdfs, pdfdown::download_pdf, subd = subd)
  res <- dplyr::bind_rows(res)
  return(res)
}
