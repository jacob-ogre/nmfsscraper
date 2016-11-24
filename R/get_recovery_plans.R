#' Return NMFS's main recovery plan page as an xml2 object
#'
get_recovery_plans_page <- function() {
  page <- xml2::read_html("http://www.nmfs.noaa.gov/pr/recovery/plans.htm")
}

#' Return PDF urls linked directly to NMFS's main recovery page
#' @export
get_plan_urls_direct <- function(page, domain = "http://www.nmfs.noaa.gov") {
  atag <- rvest::html_nodes(page, "a")
  href <- rvest::html_attr(atag, "href")
  pdfs <- href[grep(href, pattern = "pdf$|PDF$")]
  pdfs <- ifelse(grepl(pdfs, pattern = "^http"),
                 pdfs,
                 paste0(domain, pdfs))
  return(pdfs)
}

#' Return PDF urls linked indirectly to NMFS's main recovery page
#' @export
get_plan_urls_indirect <- function(page) {
  domain <- function(x) paste(strsplit(x, "/")[[1]][1:3], collapse = "/")
  cntr <- rvest::html_nodes(page, xpath = '//*[@id="center"]')
  atag <- rvest::html_nodes(cntr, "a")
  href <- rvest::html_attr(atag, "href")
  outl <- href[grep(href, pattern = "htm[l]$")]
  pdfs <- lapply(outl,
                 FUN = function(x) {
                   pg <- xml2::read_html(x)
                   get_plan_urls_direct(pg, domain(x))
                 }
          )
  return(unique(unlist(pdfs)))
}

#' Return PDF urls for ESA recovery plans from NMFS
#'
#' @details The National Marine Fisheries Service (NMFS) provides recovery plan
#' materials (the plans, addendums, etc.) for Endangered Species Act-listed
#' (ESA) species under its jurisdiction at
#' \url{http://www.nmfs.noaa.gov/pr/recovery/plans.htm}. Except that not all
#' plans or plan materials are located at that site; plans for different
#' Chinook salmon populations are located elsewhere. This function returns a
#' vector of all PDF URLs on the main recovery plan page as well as PDFs on
#' pages linked from the main page. After getting this list, all of the PDFs
#' can be downloaded using a custom scraper, with \link[pdfdown]{pdfdown}, or
#' using \link{get_recovery_plans}.
#'
#' @return A vector of URLs for recovery plan PDFs
#'
#' @export
get_plan_pdf_urls <- function() {
  pg <- get_recovery_plans_page()
  direct <- get_plan_urls_direct(pg)
  indirect <- get_plan_urls_indirect(pg)
  pdfs <- c(direct, indirect)
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
#' dl_res <- get_recovery_plans("~/Downloads/NMFS_rec")
#' }
get_recovery_plans <- function(subd = "") {
  all_plan_pdfs <- get_plan_pdf_urls()
  res <- lapply(all_plan_pdfs, pdfdown::download_pdf, subd = subd)
  res <- dplyr::bind_rows(res)
  return(res)
}