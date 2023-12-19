\name{coicop.bundles}

\alias{unbundle}
\alias{is.bundle}
\alias{keep.bundle}
\alias{coicop.bundles}

\title{COICOP bundle codes}

\author{Sebastian Weinand}

\description{
HICP data follow the COICOP classification system. However, sometimes COICOP ids are merged into bundles, deviating from the usual structure of ids (e.g. 08X, 0531_2). Function \code{is.bundle()} flags if a coicop id is a bundle or not, while \code{unbundle()} splits the bundles into their original ids. Both functions make use of the bundle dictionary \code{coicop.bundles}. Function \code{keep.bundle()} checks if either the bundle id or its component ids shall be kept, if both are present.
}

\usage{
is.bundle(id)

unbundle(id)

keep.bundle(id)

coicop.bundles # R-object
}

\arguments{
  \item{id}{character vector of coicop ids.}
}

\value{
For \code{is.bundle()} and \code{keep.bundle()}, a logical vector of the same length as \code{id}. For \code{unbundle()} a vector of ids with length greater or equal to the length of \code{id}.}

\details{
\code{keep.bundle()} checks for a bunde id if all its component ids are present. If this is the case, the bundle id can be dropped. Otherwise, if only some or none component ids are present, these can be dropped while the bundle id is kept.
}

\examples{
ids <- c("011",NA,"08X","112","0531_2")
is.bundle(ids)
unbundle(ids)

keep.bundle(id=c("08X","082","1212","1213","1212_3"))
# 081 is missing, that's why 08X is kept while 082 is dropped
# both 1212 and 1213 are present, which is why 1212_3 is dropped
}
