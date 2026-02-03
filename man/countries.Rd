\name{countries}

\alias{countries}
\alias{is.ea}
\alias{is.eu}
\alias{is.eea}

\title{European aggregates and countries}

\author{Sebastian Weinand}

\description{
The function \code{countries()} lists all countries with available HICP data in their protocol order.

The functions \code{is.ea()}, \code{is.eu()} and \code{is.eea()} indicate if a country belongs to one of the European aggregate at a specific date.
}

\usage{
countries(group="All", t=Sys.Date())

is.ea(id, t=Sys.Date())

is.eu(id, t=Sys.Date())

is.eea(id, t=Sys.Date())
}

\arguments{
  \item{group}{character specifying the scope. Allowed values are \code{All} for all countries, \code{EA} for euro area countries, \code{EU} for the Member States of the European Union and \code{EEA} for members of the European Economic Area.}
  \item{id}{character vector of country codes.}
  \item{t}{date vector in format \code{YYYY-MM-DD}. Must be of length one for \code{countries()}.}
}

\details{
The functions \code{is.ea()}, \code{is.eu()} and \code{is.eea()} can be used to flag the evolving composition of European aggregates by letting \code{t} advance in time.
By contrast, for the fixed composition of European aggregates at a specific date, \code{t} can be fixed at this date.
}

\value{
For \code{countries()}, a named vector of countries.

For \code{is.ea()}, \code{is.eu()} and \code{is.eea()}, a logical vector of the same length as \code{id}.
}

\references{
European Commission, Eurostat, \emph{Harmonised Index of Consumer Prices (HICP) - Methodological Manual - 2024 edition}, Publications Office of the European Union, 2024, Annex 11.2, \doi{10.2785/055028}.
}

\examples{
# EU member states in 2025:
countries(group="EU", t=as.Date("2025-01-01"))

# check which of the following countries belong to euro area in 2025:
is.ea(id=c("HR","BG"), t=as.Date("2025-01-01"))

# check over time:
is.ea(id="BG", t=as.Date(c("2024-01-01","2025-01-01","2026-01-01")))
}
