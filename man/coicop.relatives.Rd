\name{coicop}

\alias{is.coicop}
\alias{level}
\alias{child}
\alias{parent}
\alias{coicop}

\title{Working with COICOP codes}

\author{Sebastian Weinand}

\description{
Function \code{is.coicop()} checks if the input is a valid COICOP code while \code{level()} returns the COICOP level (e.g. division or subclass). Function \code{parent()} derives the higher-level parent of a COICOP code if available in the data supplied. The function \code{child()} does the same for lower-level children.
}

\usage{
is.coicop(id, settings=list())

level(id, label=FALSE, settings=list())

child(id, flag=TRUE, direct=FALSE, settings=list())

parent(id, flag=TRUE, direct=FALSE, settings=list())
}

\arguments{
  \item{id}{character vector of COICOP ids.}
  \item{label}{logical indicating if the number of digits or the labels (e.g., division, subclass) should be returned.}
  \item{flag}{for \code{flag=TRUE}, parent or child codes are available in the data are flagged by a logical. Otherwise, the parent or child codes are returned.}
  \item{direct}{logical indicating if only direct relatives should be flagged as \code{TRUE} (e.g. \code{03->031}) or also indirect relatives (e.g. \code{03->0311}) if direct relatives in between are missing.}
  \item{settings}{a list of control settings to be used. The following settings are supported:
  \itemize{
    \item \code{coicop.version} : character specifying the COICOP version to be used when checking for valid COICOP codes. See details for the allowed values. The default is \code{getOption("hicp.coicop.version")}.
    \item \code{unbundle} : logical indicating if COICOP bundles (e.g. 08X, 0531_2) as defined in \code{\link{coicop.bundles}} should be taken into account or not. The default is \code{getOption("hicp.unbundle")}.
    \item \code{all.items.code} : character specifying the code internally used for the all-items index. The default is taken from \code{getOption("hicp.all.items.code")}. Not used by \code{is.coicop()}.
  }
  }
}

\details{
The following COICOP versions are supported:
\itemize{
  \item Classification of Individual Consumption According to Purpose (\href{https://unstats.un.org/unsd/classifications/Econ/Structure}{COICOP-1999}): \code{coicop1999}
  \item European COICOP (\href{https://op.europa.eu/de/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/ecoicop}{ECOICOP}): \code{ecoicop}
  \item ECOICOP adopted to the needs of the HICP (\href{https://op.europa.eu/de/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/ecoicop-hicp}{ECOICOP-HICP}): \code{ecoicop-hicp}
  \item \href{https://unstats.un.org/unsd/classifications/Econ}{COICOP-2018}: \code{coicop2018}
}
None of the COICOP versions include a code for the all-items index. The internal package code for the all-items index is globally defined by \code{options(hicp.all.items.code="00")} but can be changed by the user. The \code{level()} is always 1.

If \code{settings$unbundle=TRUE}, COICOP bundle codes are resolved into their component ids and processed in that way. By contrast, if \code{settings$unbundle=FALSE}, COICOP bundle codes are internally set to \code{NA}. Consequently, they can't be a parent or a child of some other COICOP code.
}

\value{
Function \code{is.coicop()} returns a logical vector and function \code{level()} a numeric vector. If argument \code{flag=TRUE}, functions \code{parent()} and \code{child()} both return a logical vector. If \code{flag=FALSE}, \code{parent()} gives a character vector, while \code{child()} returns a list. In any case, all function outputs have the same length as \code{id}.
}

\seealso{
\code{\link{unbundle}}, \code{\link{tree}}
}

\examples{
### EXAMPLE 1

# validity of coicop id:
is.coicop(id=c("00","CP00","13","08X"), settings=list(unbundle=TRUE))
is.coicop(id=c("00","CP00","13","08X"), settings=list(unbundle=FALSE))

# coicop level:
level(id=c("00","05","053","0531_2"))
level(id=c("00","05","053","0531_2"), label=TRUE)

# check for children in data:
child(id=c("0111"), flag=FALSE) # false, no child found
child(id=c("0111", "01"), flag=FALSE, direct=TRUE) # still false
child(id=c("0111", "01"), flag=FALSE, direct=FALSE) # now TRUE

# check for parent in data, including coicop bundles:
ids <- c("053","0531_2","05311","05321")
parent(id=ids, flag=FALSE, direct=TRUE, settings=list(unbundle=FALSE))
parent(id=ids, flag=FALSE, direct=TRUE, settings=list(unbundle=TRUE))

### EXAMPLE 2
\donttest{
# set cores for testing on CRAN:
library(restatapi)
options(restatapi_cores=1)
library(data.table)

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
}}
