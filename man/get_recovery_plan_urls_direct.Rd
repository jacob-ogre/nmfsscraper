% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_recovery_plans.R
\name{get_recovery_plan_urls_direct}
\alias{get_recovery_plan_urls_direct}
\title{Return PDF urls linked directly to NMFS's main recovery page}
\usage{
get_recovery_plan_urls_direct(page, domain = "http://www.nmfs.noaa.gov")
}
\arguments{
\item{page}{An \link[xml2]{read_html} page}

\item{domain}{The base domain for pages, needed for relative URLs}
}
\description{
Return PDF urls linked directly to NMFS's main recovery page
}

