\name{chaining}

\alias{unchain}
\alias{chain}
\alias{rebase}
\alias{convert}

\title{Chain-linking, rebasing and index conversion}

\author{Sebastian Weinand}

\description{
The function \code{unchain()} unchains an annually chain-linked index series.
The unchained indices can be aggregated into higher-level indices, which can be chain-linked using the function \code{chain()} to obtain a long-term index series.
The function \code{rebase()} sets the reference period of the chain-linked index.

The function \code{convert()} allows to convert monthly indices into quarterly and yearly indices or 12-month moving averages.
}

\usage{
unchain(x, t, by=12, settings=list())

chain(x, t, by=12, settings=list())

rebase(x, t, t.ref="first", settings=list())

convert(x, t, type="year", settings=list())
}

\arguments{
  \item{x}{numeric vector of index values.}
  \item{t}{date vector of monthly (i.e., one observation per month), quarterly or yearly frequency in format \code{YYYY-MM-DD}.}
  \item{by}{for one-month or one-quarter overlap a single integer between 1 and 12 specifying the price reference period; for annual overlap using a full calendar year \code{NULL}.}
  \item{t.ref}{character specifying the index reference period either in format \code{YYYY} for a calendar year or \code{YYYY-MM} for a specific month or quarter. Can also be \code{first} or \code{last} to use the first or last available period. If \code{t.ref} contains multiple entries, these are processed in the order provided, and the first match is used for the rebasing.}
  \item{type}{type of converted index. Either \code{year} (for annual average), \code{quarter} (for quarterly average), or \code{12mavg} (for a 12-month moving average).}
  \item{settings}{list of control settings to be used. The following settings are supported:
  \itemize{
    \item \code{chatty} : logical indicating if package-specific warnings and info messages should be printed or not. The default is \code{getOption("hicp.chatty")}.
    \item \code{freq} : character specifying the frequency of \code{t}. Allowed values are \code{month}, \code{quarter}, \code{year}, and \code{auto} (the default). For \code{auto}, the frequency is internally derived from \code{t}.
    \item \code{na.rm} : logical indicating if averages for calendar years should also be computed when there are \code{NA}s and less than 12 months (or 4 quarters) present (for \code{na.rm=TRUE}). For the 12-month moving average in \code{convert()}, the calculations are always based on the last 12 months (or 4 quarters), meaning that only \code{NA}s are excluded. The default is \code{na.rm=FALSE}.
    }}
}

\value{
The functions \code{unchain()}, \code{chain()}, \code{rebase()}, and \code{convert(..., type="12mavg")} return numeric values of the same length as \code{x}.

For \code{type="year"} and \code{type="quarter"}, the function \code{convert()} returns a named numeric vector of the length of quarters or years available in \code{t}, where the names correspond to the last month of the year or quarter.
}

\details{
The function \code{unchain()} sets the value of the first price reference period to \code{NA} although the value could be set to 100 (if \code{by} is not \code{NULL}) or 100 divided by the average of the year (if \code{by=NULL}).
This is wanted to avoid aggregation of these values.
The function \code{chain()} finally sets the values back to 100.
}

\seealso{
\code{\link{aggregate}}
}

\references{
European Commission, Eurostat, \emph{Harmonised Index of Consumer Prices (HICP) - Methodological Manual - 2024 edition}, Publications Office of the European Union, 2024, \doi{10.2785/055028}.
}

\examples{
### EXAMPLE 1

# sample monthly price index:
t <- seq.Date(from=as.Date("2022-12-01"), to=as.Date("2025-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

# rebase index to new reference period:
rebase(x=p, t=t, t.ref=c("1996","2023")) # 1996 not present so 2023 is used
rebase(x=p, t=t, t.ref=c("1996","first")) # 1996 not present so first period is used

# convert into quarterly index:
convert(x=p, t=t, type="q") # first quarter is not complete so NA

# unchaining and chaining gives initial results:
100*p/p[1]
chain(unchain(p, t, by=12), t, by=12)

# use annual overlap:
100*p/mean(p[1:12])
(res <- chain(unchain(p, t, by=NULL), t, by=NULL))
# note that for backwards compability, each month in the first
# year receives an index value of 100. this allows the same
# computation again:
chain(unchain(res, t, by=NULL), t, by=NULL)

### EXAMPLE 2: Working with published HICP data
\donttest{
library(data.table)
library(restatapi)
options(restatapi_cores=1) # set cores for testing on CRAN
options(hicp.chatty=FALSE) # suppress package messages and warnings

# import monthly price indices for euro area:
dtm <- hicp::data(id="prc_hicp_minr", filters=list(unit="I25", geo="EA"))
dtm[, "time":=as.Date(paste0(time, "-01"))]
setkeyv(x=dtm, cols=c("unit","coicop18","time"))

# unchain, chain, and rebase all euro area indices by COICOP:
dtm[, "dec_ratio" := unchain(x=values, t=time), by="coicop18"]
dtm[, "chained_index" := chain(x=dec_ratio, t=time), by="coicop18"]
dtm[, "index_own" := rebase(x=chained_index, t=time, t.ref="2025"), by="coicop18"]

# convert all monthly indices into annual averages:
dta <- dtm[, as.data.table(
              x=convert(x=values, t=time, type="year"), 
              keep.rownames=TRUE), by="coicop18"]
setnames(x=dta, c("coicop18","time","index"))
plot(index~as.Date(time), data=dta[coicop18=="TOTAL",], type="l")
}}
