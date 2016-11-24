#' Read NMFS's ESA listings page
#'
#' @importFrom xml2 read_html
#' @export
#' @examples
#' \dontrun{
#'    pg <- get_listings_page()
#' }
get_listings_page <- function() {
  page <- xml2::read_html("http://www.nmfs.noaa.gov/pr/species/esa/listed.htm")
  return(page)
}

#' Get URLs to NMFS's ESA species pages
#'
#' @importFrom xml2 read_html
#' @export
#' @examples
#' \dontrun{
#'    pg <- get_listings_page()
#' }
get_species_pages_links <- function(page) {
  cntr <- rvest::html_nodes(page, xpath = '//*[@id="center"]')
  atag <- rvest::html_nodes(cntr, "a")
  href <- rvest::html_attr(atag, "href")
  part <- unique(
            href[grep(href,
                      pattern = "/pr/species/(mammals|turtles|snakes|fish|invertebrates)/")]
          )
  sppg <- part[grep(part, pattern = "*.htm[l]?")]
  return(sppg)
}

get_species_tables <- function(page) {
  tables <- rvest::html_table(page)
  warning("Note that these tables are a mess; manual collation may be better.")
  return(tables)
}

get_mammals_table <- function(tabs) {
  mammals <- tabs[[1]]
  names(mammals) <- c("Species", "YearListed", "Status", "CH", "RecoveryPlan")
  return(mammals)
}

get_reptiles_table <- function(tabs) {
  reptiles <- tabs[[2]]
  names(reptiles) <- c("Species", "YearListed", "Status", "CH", "RecoveryPlan")
  return(reptiles)
}

get_fish_table <- function(tabs) {
  fish <- tabs[[3]]
  names(fish) <- c("Species", "YearListed", "Status", "CH", "RecoveryPlan")
  return(fish)
}

get_inverts_table <- function(tabs) {
  inverts <- tabs[[4]]
  names(inverts) <- c("Species", "YearListed", "Status", "CH", "RecoveryPlan")
  return(inverts)
}

get_plants_table <- function(tabs) {
  plants <- tabs[[5]]
  names(plants) <- c("Species", "YearListed", "Status", "CH", "RecoveryPlan")
  return(plants)
}
