\name{rates}

\alias{rate}
\alias{rates}
\alias{contribution}
\alias{contributions}

\title{Change rates and contributions}

\author{Sebastian Weinand}

\description{
The function \code{rate()} derives the rate of change for a specific time period.

The function \code{contribution()} calculates the contribution of a subcomponent (e.g., food, energy) to the change rate of the overall index within a specific time period. 

The functions \code{rates()} and \code{contributions()} are vectorized versions for monthly, quarterly and annual (average) change rates.
}

\usage{
rate(x, t, start, end, settings=list())

rates(x, t, type="year", settings=list())

contribution(x, x0=NULL, w, xagg, xagg0=NULL, wagg, t, start, end, settings=list())

contributions(x, x0=NULL, w, xagg, xagg0=NULL, wagg, t, type="year", settings=list())
}

\arguments{
  \item{x, xagg}{numeric vector of index values of the subcomponent (\code{x}) and the overall index (\code{xagg}).}
  \item{x0, xagg0}{numeric vector of index values of the subcomponent (\code{x0}) and the overall index (\code{xagg0}) in the price reference period. 
  Only required for quarterly and annual index averages (see also details).}
  \item{w, wagg}{numeric vector of weights of the subcomponent (\code{w}) and the overall index (\code{wagg}).}
  \item{t}{date vector of monthly (i.e., one observation per month), quarterly or yearly frequency in format \code{YYYY-MM-DD}.}
  \item{start,end}{start and end dates of the considered time period in format \code{YYYY-MM-DD}.}
  \item{type}{character specifying the type of change rate. Allowed values are \code{month} (for monthly change rates), \code{quarter} (for quarterly change rates), and \code{year} (for annual change rates).
  See also details.}
  \item{settings}{list of control settings to be used. The following settings are supported:
  \itemize{
    \item \code{chatty} : logical indicating if package-specific warnings and info messages should be printed or not. The default is \code{getOption("hicp.chatty")}.
    \item \code{freq} : character specifying the frequency of \code{t}. Allowed values are \code{month}, \code{quarter}, \code{year}, and \code{auto} (the default). For \code{auto}, the frequency is internally derived from \code{t}.
    \item \code{details} : logical indicating if the annual terms underlying the contribution should be returned. Default is \code{FALSE}. Only relevant for \code{contribution()}.
    }}
}

\details{
For monthly index series, the change rates show the percentage change of \code{x} in the current month compared to the previous month (monthly change rates, \emph{m-1}), compared to three months ago (quarterly change rates, \emph{m-3}), or compared to the same month one year before (annual change rates, \emph{m-12}).

For quarterly index series, the change rates show the percentage change of \code{x} in the current quarter compared to the previous quarter (quarterly change rates, \emph{q-1}) or compared to the same quarter one year before (annual change rates, \emph{q-4}).

For yearly index series, the change rates show the percentage change of \code{x} in the current year compared to the previous year (annual change rates, \emph{y-1}).
If \code{x} is an annual index produced by \code{convert()}, the annual change rates correspond to annual average change rates.

The calculation of contributions to the overall change rate(s) applies to annually chain-linked Laspeyres-type indices with price reference period December of the previous year.
For quarterly and annual index averages, the initial monthly index values of the price reference period are required for a proper calculation of the contributions.
They can be provided via \code{x0} and \code{xagg0}.
}

\value{
A numeric vector of length one for \code{rate()} and \code{contribution()} and of the same length as \code{x} for \code{rates()} and \code{contributions()}.
}

\references{
Brunetti, A. (2010), 'The decomposition of the chained price index rate of change: Generalization and interpretative effectiveness', In: \emph{Rivista di statistica ufficiale}, 2010 (1), pp. 17–34.

European Commission, Eurostat, \emph{Harmonised Index of Consumer Prices (HICP) - Methodological Manual - 2024 edition}, Publications Office of the European Union, 2024, \doi{10.2785/055028}.

Schwind, P. and Weinand, S. (2026). 'Contributions to euro area inflation over arbitrary time periods', paper presented at the 19th meeting of the Ottawa Group on Price Indices, Warsaw, Poland, 13-15 May 2026.
}

\examples{
### EXAMPLE 1

# sample monthly price index:
t <- seq.Date(from=as.Date("2022-12-01"), to=as.Date("2025-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

# change rate over entire period:
rate(x=p, t=t, start=min(t), end=max(t)) 

# annual change rates:
rates(x=p, t=t, type="year")

# annual average change rates:
pa <- convert(x=p, t=t, type="year")
rates(x=pa, t=as.Date(names(pa)), type="year")

# 12-month average rate of change:
pmvg <- convert(x=p, t=t, type="12mavg")
rates(x=pmvg, t=t, type="year")
\donttest{
### EXAMPLE 2: Ribe contributions using published HICP data

library(data.table)
library(restatapi)
options(restatapi_cores=1)  # set cores for testing on CRAN
options(hicp.chatty=FALSE)  # suppress package messages and warnings

# import monthly price indices for euro area:
dtp <- hicp::data(id="prc_hicp_minr", 
                  date.range=c("2019-12", NA), 
                  filters=list(unit="I25", geo="EA"))
                  
# import euro area item weights:
dtw <- hicp::data(id="prc_hicp_iw", date.range=c("2020",NA), filters=list(geo="EA"))
dtw[, "year":=as.integer(time)]

# combine data:   
dtall <- CJ("coicop18"=unique(dtw$coicop18), "time"=unique(dtp$time))
dtall[dtp, "index":=i.values, on=c("coicop18","time")]
dtall[, "time":=as.Date(paste0(time, "-01"))]
dtall[, "year":=year(time)]
dtall[dtw, "weight":=i.values, on=c("coicop18","year")]   
dtall <- dtall[year <= year(Sys.Date())-1,] # not necessarily needed
setcolorder(dtall, c("coicop18","year","time","index","weight"))
setkeyv(dtall, cols=c("coicop18","time"))

# add all-items HICP:
dtall[dtall[coicop18=="TOTAL", ], 
      c("index_all","weight_all"):=list(i.index,i.weight), 
      on="time"]

# calculate Ribe contributions by COICOP:
dtall[, "ribe" := contributions(x=index, w=weight, 
                                xagg=index_all, wagg=weight_all,
                                t=time, type="year"), by="coicop18"]

# plot annual change rates over time:
plot(rates(x=index, t=time, type="year")~time,
     data=dtall[coicop18=="TOTAL",],
     type="l", ylim=c(-2,12))

# add contributions of energy to plot:
lines(ribe~time, data=dtall[coicop18=="NRG"], col="red")

# "cumulative" contribution of energy to overall price change until 2025:
dtall[coicop18=="NRG", 
      contribution(x=index, w=weight, xagg=index_all, wagg=weight_all,
                   t=time, start=as.Date("2019-12-01"), end=as.Date("2025-12-01")), 
      by="coicop18"]
}}
