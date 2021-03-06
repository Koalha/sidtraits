% Generated by roxygen2 (4.0.1): do not edit by hand
\name{sidspecurl}
\alias{sidspecurl}
\title{Check URLs for binomial names in SID}
\usage{
sidspecurl(sciname, sepa = " ", single = FALSE)
}
\arguments{
\item{sciname}{A binomial name}

\item{sepa}{Character separating genus and species names. Defaults to " "}

\item{single}{Should the result be restricted to length one? Defaults to FALSE}
}
\description{
This function checks if Seed Information Database has entries for a given
binomial name, and returns the URL if an entry exists.
}

