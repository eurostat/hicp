\name{coicop}

\alias{is.coicop}
\alias{is.bundle}
\alias{is.spec.agg}
\alias{label}
\alias{level}
\alias{spec.agg}
\alias{coicop}

\title{COICOP codes and special aggregates}

\author{Sebastian Weinand}

\description{
Valid COICOP codes can be flagged by \code{is.coicop()}.
The function \code{level()} returns their level (e.g. 2-digit division or 5-digit subclass level). 

For HICP data, special aggregates like food or energy have their own codes, which can be flagged by \code{is.spec.agg()}.
The function \code{spec.agg()} provides the composition of special aggregates.

The function \code{label()} translates the codes into their descriptions.
}

\usage{
# flag COICOP, bundle and special aggregate codes:
is.coicop(id, settings=list())
is.bundle(id, settings=list())
is.spec.agg(id, settings=list())

# derive the level of COICOP codes:
level(id, settings=list())

# label codes:
label(id, settings=list())

# get the composition of a special aggregate:
spec.agg(id=NULL, settings=list())
}

\arguments{
  \item{id}{character vector of COICOP or special aggregate codes.}
  \item{settings}{list of control settings to be used. The following settings are supported:
  \itemize{
    \item \code{coicop.version} : character specifying the COICOP version to be used.
    See details for the allowed values. 
    The default is \code{getOption("hicp.coicop.version")}.
    \item \code{coicop.prefix} : character specifying a prefix for the COICOP codes.
    The default is \code{getOption("hicp.coicop.prefix")}.
    \item \code{all.items.code} : character specifying the code internally used for the all-items index.
    The default is taken from \code{getOption("hicp.all.items.code")}.
    If the character is named, the function \code{label()} uses the name as the label of the all-items code.
    }
  }
}

\details{
The following COICOP versions are supported:
\itemize{
  \item Classification of Individual Consumption According to Purpose (\href{https://unstats.un.org/unsd/classifications/Econ/Structure}{COICOP-1999}): \code{coicop1999}
  \item European COICOP (version 1, \href{https://op.europa.eu/web/eu-vocabularies/concept-scheme/-/resource?uri=http://data.europa.eu/ed1/ecoicop/ecoicop}{ECOICOP}): \code{ecoicop1}
  \item ECOICOP adapted to the needs of the HICP (version 1, \href{https://op.europa.eu/web/eu-vocabularies/concept-scheme/-/resource?uri=http://data.europa.eu/ed1/ecoicop/ecoicop-hicp}{ECOICOP-HICP}): \code{ecoicop1.hicp}
  \item \href{https://unstats.un.org/unsd/classifications/Econ}{COICOP-2018} (including the voluntary 6-digit codes): \code{coicop2018}
  \item ECOICOP (version 2, \href{https://op.europa.eu/web/eu-vocabularies/concept-scheme/-/resource?uri=http://data.europa.eu/ed1/ecoicop2/ecoicop2}{ECOICOP 2}): \code{ecoicop2}
  \item ECOICOP adapted to the needs of the HICP (version 2, \href{https://op.europa.eu/web/eu-vocabularies/concept-scheme/-/resource?uri=http://data.europa.eu/ed1/ecoicop2/ecoicop2-hicp}{ECOICOP 2 HICP}): \code{ecoicop2.hicp}
}
The COICOP version can be set temporarily in the function settings or globally via \code{options("hicp.coicop.version")}.
For \code{ecoicop1.hicp}, some COICOP codes are merged into bundles (e.g. \code{08X}, \code{0531_2}), deviating from the expected code structure.
Although such bundle codes are no valid COICOP codes, they are internally resolved into their underlying codes and processed in that way if they can be found in the package's internal bundle code dictionary.

None of the COICOP versions include a code for the all-items index.
The internal package code for the all-items index is defined by \code{options("hicp.all.items.code")}.
Its level is always 1 and the label \code{All-items} by default.

For HICP data from Eurostat's database, COICOP codes are usually prefixed by ``CP''.
Whether such a prefix is expected can be defined in \code{options("hicp.coicop.prefix")}.
}

\value{
The functions \code{is.coicop()}, \code{is.bundle()} and \code{is.spec.agg()} return a logical vector, the function \code{level()} an integer vector and the function \code{label()} a character vector.
All function outputs have the same length as \code{id}.

The function \code{spec.agg()} returns a named list with the special aggregate composition. 
}

\references{
European Commission, Eurostat, \emph{Harmonised Index of Consumer Prices (HICP) - Methodological Manual - 2024 edition}, Publications Office of the European Union, 2024, Annex 11.1, \doi{10.2785/055028}.
}

\seealso{
\code{\link{child}}, \code{\link{parent}}, \code{\link{tree}}
}

\examples{
# example codes:
ids <- c("TOTAL","CP01","CP011","CP99","FOOD","NRG")
ids[is.coicop(ids)] # all-items is no valid COICOP code
ids[is.spec.agg(ids)]
level(ids) # special aggregates without level
label(ids) 
}
