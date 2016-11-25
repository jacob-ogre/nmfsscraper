get_glossary_page <- function() {
  pg <- try(xml2::read_html("http://www.nmfs.noaa.gov/pr/glossary.htm"))
  return(pg)
}

get_OPR_glossary <- function(pg) {
  # pg <- get_glossary_page()
  if(class(pg[1]) != "try-error") {
    cntr <- rvest::html_nodes(pg, xpath = '//*[@id="center"]')
    para <- rvest::xml_nodes(cntr, "p")
    text <- rvest::html_text(para)
    no_cont <- text[-endsWith(text, suffix = "--")]
    no_cont <- no_cont[-endsWith(no_cont, suffix = ":")]
    cont_dash <- text[endsWith(text, suffix = "--")]
    cont_colo <- text[endsWith(text, suffix = ":")]
    ols <- rvest::html_text(rvest::xml_nodes(cntr, "ol"))
    uls <- rvest::html_text(rvest::xml_nodes(cntr, "ul"))
  } else {
    stop("Error reading the NMFS glossary page.")
  }
  res <- list(text = text,
              no_cont = no_cont,
              cont_dash = cont_dash,
              cont_colo = cont_colo,
              ols = ols,
              uls = uls)
  return(res)
}