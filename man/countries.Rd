\name{countries}

\alias{countries}

\title{EU and EFTA country metadata}

\author{Sebastian Weinand}

\docType{data}

\description{
This dataset contains metadata on all (former) EU and EFTA countries that submit HICP data on a regular basis (e.g. number of index decimals used for dissemination).
}

\format{
Data for European Union (EU), euro area (EA) and the individual EU and EFTA countries (excluding Liechtenstein).
\itemize{
  \item \code{code}: the country code
  \item \code{name}: the country name
  \item \code{protocol_order}: the official protocol order of countries
  \item \code{group}: a character distinguishing between country aggregates, EU countries and EFTA countries
  \item \code{eu_entry} and \code{eu_exit}: date of joining or leaving the European Union
  \item \code{euro_since}: the date of introduction of the euro as the official currency
  \item \code{decimals}: the number of index decimals used for dissemination
}
}

\usage{
countries
}

\examples{
# EU:
countries[group=="eu",]

# individual EA countries:
countries[group=="eu" & !is.na(euro_since),]
}
