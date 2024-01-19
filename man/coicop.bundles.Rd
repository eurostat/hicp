\name{coicop.bundles}

\alias{unbundle}
\alias{is.bundle}
\alias{coicop.bundles}

\title{COICOP bundle codes}

\author{Sebastian Weinand}

\description{
HICP data follow the COICOP classification system. However, sometimes COICOP ids are merged into bundles, deviating from the usual structure of ids (e.g. 08X, 0531_2). Function \code{is.bundle()} flags if a coicop id is a bundle or not, while \code{unbundle()} splits the bundles into their original ids. Both functions make use of the bundle dictionary \code{coicop.bundles}.
}

\usage{
is.bundle(id)

unbundle(id)

# list of coicop bundles:
coicop.bundles
}

\arguments{
  \item{id}{character vector of coicop ids.}
}

\value{
For \code{is.bundle()}, a logical vector of the same length as \code{id}. For \code{unbundle()} a vector of ids with length greater or equal to the length of \code{id}.}

\examples{
ids <- c("011",NA,"08X","112","0531_2")
is.bundle(ids)
unbundle(ids)
}
