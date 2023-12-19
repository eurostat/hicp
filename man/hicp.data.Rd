\name{hicp.data}

\alias{hicp.datasets}
\alias{hicp.datafilters}
\alias{hicp.dataimport}

\title{Download HICP data}

\author{Sebastian Weinand}

\description{
These functions are simple wrappers of functions in the \code{restatapi} package. Function \code{hicp.datasets()} lists all available HICP datasets in Eurostat's public database, while \code{hicp.datafilters()} gives the allowed values that can be used for filtering a dataset. \code{hicp.dataimport()} downloads a specific dataset with filtering on key parameters and time, if supplied.
}

\usage{
hicp.datasets()

hicp.datafilters(id)

hicp.dataimport(id, filters=list(), date.range=NULL, flags=FALSE)
}

\arguments{
  \item{id}{A dataset identifier, which can be obtained from \code{hicp.datasets()}.}
  \item{filters}{A named list of filters to be applied to the data request. Allowed values for filtering can be retrieved from \code{hicp.datafilters()}. For HICP data, typical filter variables are the index reference period (\code{unit: I96, I05, I15}), the country (\code{geo: EA, DE, FR,...}), or the COICOP code (\code{coicop: CP00, CP01, SERV, ...}).}
  \item{date.range}{A vector of start and end date used for filtering on time dimension. These must follow the pattern \code{YYYY(-MM)?}. An open interval can be defined by setting one date to \code{NA}.}
  \item{flags}{A logical indicating if data flags should be returned or not.}
}

\value{
A data.table.
}

\seealso{
restatapi: \code{\link{get_eurostat_toc}}, \code{\link{get_eurostat_dsd}}, \code{\link{get_eurostat_data}}
}

\source{
See Eurostat's public database at \url{https://ec.europa.eu/eurostat/web/main/data/database}.
}

\examples{
# view available datasets:
hicp.datasets()

# get allowed filters for item weights:
hicp.datafilters(id="prc_hicp_inw")

# download item weights for euro area from 2015 on:
hicp.dataimport(id="prc_hicp_inw", filters=list("geo"="EA"), date.range=c("2015", NA))
}
