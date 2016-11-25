# nmfsscraper

Scrape PDFs From National Marine Fisheries Service Website

----

The National Marine Fisheries Service (NMFS), along with the U.S.
Fish and Wildlife Service (FWS), is responsible for implementing the
Endangered Species Act (ESA). There is a huge amount of information about
ESA-listed species and about how the ESA is implemented in tens of thousands
of PDFs that the Services host, but the Services don't make those documents
available from a single, central download location. This package is a (set
of) scrapers to search for PDFs on the [NMFS website](http://www.nmfs.noaa.gov)
and download them locally to facilitate analysis of the embedded information, 
e.g., using Natural Language Processing (NLP).

## Installation

Install `nmfsscraper` using [devtools](https://github.com/hadley/devtools):

```r
devtools::install_github("jacob-ogre/nmfsscraper")
```

## Usage

The functions in `nmfsscraper` each start with one of two verbs, `get` or 
`download`. The `get` functions return either an `xml2::read_html` object or a
vector of URLs for scraping. For example,

```r
listings <- get_listings_page()
```

returns the main [NMFS listings page](http://www.nmfs.noaa.gov/pr/species/esa/listed.htm), of which the main
content is five tables with links to species' documents.

The `download` functions download PDFs of a given type to the user's choice of
directory and return a data.frame with data about scraping success, e.g.,

```r
stat_rev <- download_status_reviews(subd = "~/Downloads/NMFS_5yr")
```

downloads all status reviews, such as [five-year reviews](http://www.nmfs.noaa.gov/pr/listing/reviews.htm).

## Help

Find a bug? [Submit an issue!](https://github.com/jacob-ogre/nmfsscraper/issues)

## Contributing

Want to contribute to the package? Fork the repository and submit a pull request!
Also feel free to get [in touch](mailto:esa@defenders.org).

