\name{coicop}

\alias{is.coicop}
\alias{level}
\alias{unbundle}
\alias{is.bundle}
\alias{child}
\alias{parent}
\alias{coicop}

\title{COICOP codes, bundles and relatives}

\author{Sebastian Weinand}

\description{
Function \code{is.coicop()} checks if the input is a valid COICOP code while \code{level()} returns the level (e.g. division or subclass). 

For HICP data, COICOP codes are sometimes merged into bundles (e.g. \code{08X}, \code{0531_2}), deviating from the usual code structure. Function \code{is.bundle()} flags if a COICOP code is a bundle or not, while \code{unbundle()} resolves the bundles into the underlying valid codes.

Functions \code{parent()} and \code{child()} derive the higher-level parents or lower-level children of a COICOP code.
}

\usage{
is.coicop(id, settings=list())

level(id, label=FALSE, settings=list())

is.bundle(id, settings=list())

unbundle(id, settings=list())

child(id, usedict=TRUE, closest=TRUE, k=1, settings=list())

parent(id, usedict=TRUE, closest=TRUE, k=1, settings=list())
}

\arguments{
  \item{id}{character vector of COICOP codes.}
  \item{label}{logical indicating if the number of digits or the labels (e.g., division, subclass) should be returned.}
  \item{usedict}{logical indicating if parents or children should be derived from the full code dictionary defined by \code{settings$coicop.version} (if set to \code{TRUE}) or only from the codes present in \code{id}.}
  \item{closest}{logical indicating if the closest parents or children should be derived or the \emph{k}-th ones defined by \code{k}. For example, if set to \code{TRUE}, the closest parent could be the direct parent for one code (e.g. \code{031->03}) and the grandparent for another (e.g. \code{0321->03}).}
  \item{k}{integer specifying the \emph{k}-th relative (e.g., \code{1} for direct parents or children, \code{2} for grandparents and grandchildren, ...). Multiple values allowed, e.g., \code{k=c(1,2)}. Only relevant if \code{closest=FALSE}.}
  \item{settings}{list of control settings to be used. The following settings are supported:
  \itemize{
    \item \code{coicop.version} : character specifying the COICOP version to be used by \code{is.coicop()}, \code{level()}, \code{child()}, and \code{parent()} for flagging valid COICOP codes. See details for the allowed values. The default is \code{getOption("hicp.coicop.version")}.
    \item \code{all.items.code} : character specifying the code internally used by \code{level()}, \code{child()}, and \code{parent()} for the all-items index. The default is taken from \code{getOption("hicp.all.items.code")}.
    \item \code{coicop.bundles} : named list specifying the COICOP bundle code dictionary used for unbundling any bundle codes in \code{id}. The default is \code{getOption("hicp.coicop.bundles")}.
    \item \code{simplify} : logical indicating if the output of \code{child()}, \code{parent()} or \code{unbundle()} should be simplified into a vector if possible. The default is \code{FALSE}.
    \itemize{
      \item For \code{child()} and \code{parent()}: If both a COICOP bundle code and the underlying codes are the parent or child, only the latter codes are kept (e.g., \code{c(08X,082)->082}). Note that simplification usually only works for \code{parent()}.
      \item For \code{unbundle()}: All codes underlying the bundle code are kept, meaning that the resulting vector length can exceed the length of \code{id}.
    }
  }
  }
}

\details{
The following COICOP versions are supported:
\itemize{
  \item Classification of Individual Consumption According to Purpose (\href{https://unstats.un.org/unsd/classifications/Econ/Structure}{COICOP-1999}): \code{coicop1999}
  \item European COICOP (version 1, \href{https://op.europa.eu/de/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/ecoicop}{ECOICOP}): \code{ecoicop}
  \item ECOICOP adopted to the needs of the HICP (version 1, \href{https://op.europa.eu/de/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/ecoicop-hicp}{ECOICOP-HICP}): \code{ecoicop.hicp}
  \item \href{https://unstats.un.org/unsd/classifications/Econ}{COICOP-2018}: \code{coicop2018}
  \item ECOICOP (version 2, \href{https://op.europa.eu/de/web/eu-vocabularies/dataset/-/resource?uri=http://publications.europa.eu/resource/dataset/ecoicop2}{ECOICOP 2}): \code{ecoicop2}
}
The COICOP version can be set temporarily in the function settings or globally via \code{options(hicp.coicop.version)}. The package default is \code{ecoicop.hicp}.

None of the COICOP versions include a code for the all-items index. By default, the internal package code for the all-items index is defined by \code{options(hicp.all.items.code="00")} but can be changed by the user. The level is always 1.

Although bundle codes (e.g. \code{08X}, \code{0531_2}) are no valid COICOP codes, they are internally resolved into their underlying codes and processed in that way if they can be found in the bundle code dictionary (see \code{getOption("hicp.coicop.bundles")}). If bundle codes should not be processed, the dictionary can be cleared by \code{options("hicp.coicop.bundles"=list())}.
}

\value{
Functions \code{is.coicop()} and \code{is.bundle()} return a logical vector, function \code{level()} an integer vector, and functions \code{child()}, \code{parent()}, and \code{unbundle()} a list. All function outputs have the same length as \code{id}.
}

\seealso{
\code{\link{tree}}
}

\examples{
### EXAMPLE 1

# check if coicop codes are valid:
is.coicop(id=c("00","CP00","01","011","13","08X"))

# get the coicop level or label:
level(id=c("00","05","053","0531_2"))
level(id=c("00","05","053","0531_2"), label=TRUE)

# derive children and parents

# no children of 01 present in ids:
child(id=c("01"), usedict=FALSE) 

# still no direct child present:
child(id=c("01","0111"), usedict=FALSE, closest=FALSE, k=1) 

# but a grandchild of 01 is found:
child(id=c("01","0111"), usedict=FALSE, closest=TRUE) 

# derive the children from the code dictionary:
child(id=c("01"), usedict=TRUE) 

# two parents found for 05311 due to presence of bundle code:
parent(id=c("0531","0531_2","05311","05321"), usedict=FALSE) 

# simplification removes bundle code:
parent(id=c("0531","0531_2","05311","05321"), usedict=FALSE, settings=list(simplify=TRUE))

### EXAMPLE 2: Working with published HICP data
\donttest{
library(data.table)
library(restatapi)
options(restatapi_cores=1) # set cores for testing on CRAN
options(hicp.chatty=FALSE) # suppress package messages and warnings

# load hicp item weights of euro area:
coicops <- hicp::data(id="prc_hicp_inw", filters=list(geo="EA"))
coicops <- coicops[grepl("^CP", coicop),]
coicops[, "coicop":=gsub("^CP", "", coicop)]

# show frequency of coicop levels over time:
coicops[, .N, by=list(time, "lvl"=level(coicop))]

# get coicop parent from the data:
coicops[, "parent":=parent(id=coicop, usedict=FALSE, settings=list(simplify=TRUE)), by="time"]

# flag if coicop has child available in the data:
coicops[, "has_child":=lengths(child(id=coicop, usedict=FALSE))>0, by="time"]
coicops[has_child==FALSE, sum(values, na.rm=TRUE), by="time"]
# coicop bundles and their component ids are both taken into
# account. this double counting explains some differences
}}
