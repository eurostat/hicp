\name{countries}

\alias{countries}

\title{Country metadata}

\author{Sebastian Weinand}

\docType{data}

\description{
This data set contains metadata for the euro area, EU, EFTA, and candidate countries that submit(ted) HICP data on a regular basis.
}

\format{
A data.table with metadata on the individual euro area (EA), EU, EFTA, and candidate countries producing the HICP.
\itemize{
  \item \code{code}: the country code
  \item \code{name_[en|fr|de]}: the country name in English, French, and German
  \item \code{protocol_order}: the official protocol order of countries
  \item \code{is_eu, is_ea, is_efta, is_candidate}: a logical indicating if a country belongs to the EU, the euro area, or if it's an EFTA or candidate country, respectively
  \item \code{eu_since, eu_until}: date of joining and leaving the European Union
  \item \code{ea_since}: the date of introduction of the euro as the official currency
  \item \code{index_decimals}: the number of index decimals used for dissemination
}

}

\usage{
# country metadata:
countries
}

\examples{
# subset to euro area countries:
countries[is_ea==TRUE, ]
}
