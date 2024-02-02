\name{coicop.relatives}

\alias{is.coicop}
\alias{level}
\alias{child}
\alias{parent}

\title{Derive or flag COICOP relatives}

\author{Sebastian Weinand}

\description{
Function \code{is.coicop()} checks if the input is a valid coicop code while \code{level()} returns the coicop level (e.g. division or subclass). Function \code{parent()} derives the parent of a coicop code if available in the data supplied, while \code{child()} does the same for children. Coicop codes must (roughly) follow the pattern \code{[0-9]\{1,5\}}, or be bundle codes defined in \code{\link{coicop.bundles}}.
}

\usage{
is.coicop(id, unbundle=TRUE)

level(id, unbundle=TRUE, label=FALSE)

child(id, flag=TRUE, unbundle=TRUE, direct=FALSE)

parent(id, flag=TRUE, unbundle=TRUE, direct=FALSE)
}

\arguments{
  \item{id}{character vector of coicop ids.}
  \item{label}{logical indicating if digits or labels should be used for the COICOP level.}
  \item{flag}{for \code{flag=TRUE}, the function flags by a logical if parent or child codes are available in the data. Otherwise, it returns the parent or child codes.}
  \item{unbundle}{logical indicating if coicop bundles (e.g. 08X, 0531_2) should be taken into account or not.}
  \item{direct}{logical indicating if only direct relatives should be flagged as TRUE (e.g. \code{03->031}) or also indirect relatives (e.g. \code{03->0311}) if direct relatives in between are missing.}
}

\details{
If \code{unbundle=TRUE}, coicop bundle codes are resolved into their component ids and processed in that way. By contrast, if \code{unbundle=FALSE}, coicop bundle codes are internally set to \code{NA}. Consequently, they can't be a parent or a child of some other coicop code.
}

\value{
Function \code{is.coicop()} returns a logical vector and function \code{level()} a numeric vector. If argument \code{flag=TRUE}, functions \code{parent()} and \code{child()} both return a logical vector. If \code{flag=FALSE}, \code{parent()} gives a character vector, while \code{child()} returns a list. In any case, all function outputs have the same length as \code{id}.
}

\seealso{
\code{\link{unbundle}}, \code{\link{tree}}
}

\examples{
require(data.table)

### EXAMPLE 1

# validity of coicop id:
is.coicop(id=c("00","CP00","13","08X"), unbundle=TRUE)
is.coicop(id=c("00","CP00","13","08X"), unbundle=FALSE)

# coicop level:
level(id=c("00","05","053","0531_2"))
level(id=c("00","05","053","0531_2"), label=TRUE)

# check for children in data:
child(id=c("0111"), flag=FALSE) # false, no child found
child(id=c("0111", "01"), flag=FALSE, direct=TRUE) # still false
child(id=c("0111", "01"), flag=FALSE, direct=FALSE) # now TRUE

# check for parent in data, including coicop bundles:
ids <- c("053","0531_2","05311","05321")
parent(id=ids, flag=FALSE, unbundle=FALSE, direct=TRUE)
parent(id=ids, flag=FALSE, unbundle=TRUE, direct=TRUE)

### EXAMPLE 2

\donttest{

# load hicp item weights:
coicops <- hicp.dataimport(id="prc_hicp_inw", filter=list(geo="EA"))
coicops <- coicops[grepl("^CP", coicop),]
coicops[, "coicop":=gsub("^CP", "", coicop)]

# get frequency of coicop levels:
coicops[, .N, by=list(time, "lvl"=level(coicop))]

# get coicop parent from the data:
coicops[, "parent":=parent(id=coicop, flag=FALSE), by="time"]

# flag if coicop has child available in the data:
coicops[, "has_child":=child(id=coicop, flag=TRUE), by="time"]
coicops[has_child==FALSE, sum(values, na.rm=TRUE), by="time"]
# coicop bundles and their component ids are both taken into
# account. this double counting explains some differences
}
}
