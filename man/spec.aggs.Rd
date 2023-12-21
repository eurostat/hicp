\name{spec.aggs}

\alias{spec.aggs}

\title{Special aggregates}

\author{Sebastian Weinand}

\docType{data}

\description{
This dataset contains the special aggregates and their composition of COICOP codes valid since 2017.
}

\format{
A data.table with the following variables.
\itemize{
    \item \code{code}: the special aggregate code
    \item \code{name_[en|fr|de]}: the special aggregate description in English, French, and German
    \item \code{composition}: a list of the COICOP product codes forming the special aggregate
}

}

\usage{
# special aggregates:
spec.aggs
}

\examples{
# subset to services:
spec.aggs[code=="SERV", composition[[1]]]
}
