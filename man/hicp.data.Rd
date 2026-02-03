\name{hicp.data}

\alias{datasets}
\alias{datafilters}
\alias{data}

\title{Download HICP data}

\author{Sebastian Weinand}

\description{
These functions are simple wrappers of functions in the \code{restatapi} package.

The function \code{datasets()} lists all available HICP data sets in Eurostat's public database, while the function \code{datafilters()} gives the allowed values that can be used for filtering a data set.
The function \code{data()} downloads a specific data set with filtering on key parameters and time, if supplied.
}

\usage{
datasets(pattern="^prc_hicp", ...)

datafilters(id, ...)

data(id, filters=list(), date.range=NULL, flags=FALSE, ...)
}

\arguments{
  \item{pattern}{character for pattern matching on data set identifier. 
  See also \code{\link[base]{grepl}}.}
  \item{id}{data set identifier, which can be obtained from \code{datasets()}.}
  \item{filters}{named list of filters to be applied to the data request. 
  Allowed values for filtering can be retrieved from \code{datafilters()}. 
  For HICP data, typical filter variables are the index reference period (\code{unit: I15, I25}), the country (\code{geo: EA, DE, FR,...}) or the COICOP code (\code{coicop18: CP01, CP02, SERV, ...}).}
  \item{date.range}{
  character vector of start and end date used for filtering on time dimension.
  These must follow the pattern \code{YYYY(-MM)?}.
  An open interval can be defined by setting one date to \code{NA}.}
  \item{flags}{logical indicating if data flags should be returned or not.}
  \item{...}{further arguments that can be passed to the functions:
  \itemize{
   \item \code{\link[restatapi]{get_eurostat_toc}} for \code{datasets()}
   \item \code{\link[restatapi]{get_eurostat_dsd}} for \code{datafilters()}
   \item \code{\link[restatapi]{get_eurostat_data}} for \code{data()}
   }}
}

\value{
A data.table.
}

\source{
See Eurostat's public database at \url{https://ec.europa.eu/eurostat/web/main/data/database}.
}

\examples{
\donttest{# set cores for testing on CRAN:
library(restatapi)
options(restatapi_cores=1)

# view available HICP data sets:
datasets()

# get allowed filters for item weights:
datafilters(id="prc_hicp_iw")

# download item weights since 2015 for euro area:
data(id="prc_hicp_iw", filters=list("geo"="EA"), date.range=c("2015", NA))
}}
