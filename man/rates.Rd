\name{rates}

\alias{rates}
\alias{contrib}

\title{Change rates and contributions}

\author{Sebastian Weinand}

\description{
The function \code{rates()} derives monthly, quarterly and annual rates of change from an index series. 

The function \code{contrib()} computes the contributions of a subcomponent (e.g., food, energy) to the change rate of the overall index (for chained indices with price reference period December of the previous year).
}

\usage{
rates(x, t, type="year", settings=list())

contrib(x, w, t, x.all, w.all, type="year", settings=list())
}

\arguments{
  \item{x, x.all}{numeric vector of index values of the subcomponent (\code{x}) and the overall index (\code{x.all}).}
  \item{w, w.all}{numeric vector of weights of the subcomponent (\code{w}) and the overall index (\code{w.all}).}
  \item{t}{date vector of monthly (i.e., one observation per month), quarterly or yearly frequency in format \code{YYYY-MM-DD}.}
  \item{type}{character specifying the type of change rate. Allowed values are \code{month} for monthly change rates, \code{quarter} for quarterly change rates, and \code{year} for annual change rates. See also details.}
  \item{settings}{list of control settings to be used. The following settings are supported:
  \itemize{
    \item \code{chatty} : logical indicating if package-specific warnings and info messages should be printed or not. The default is \code{getOption("hicp.chatty")}.
    \item \code{freq} : character specifying the frequency of \code{t}. Allowed values are \code{month}, \code{quarter}, \code{year}, and \code{auto} (the default). For \code{auto}, the frequency is internally derived from \code{t}.
    \item \code{method} : character specifying the method for decomposing the change rates. Allowed values are \code{ribe} (the default) and \code{kirchner}.
    }}
}

\details{
For monthly frequency, the change rates show the percentage change of \code{x} in the current month compared to the previous month (monthly change rates, \emph{m-1}), compared to three months ago (quarterly change rates, \emph{m-3}), or compared to the same month one year before (annual change rates, \emph{m-12}).

For quarterly frequency, the change rates show the percentage change of \code{x} in the current quarter compared to the previous quarter (quarterly change rates, \emph{q-1}) or compared to the same quarter one year before (annual change rates, \emph{q-4}).

For yearly frequency, the change rates show the percentage change of \code{x} in the current year compared to the previous year (annual change rates, \emph{y-1}).
If \code{x} is an annual index produced by \code{convert()}, the annual change rates correspond to annual average change rates.
}

\value{
A numeric vector of the same length as \code{x}.
}

\references{
European Commission, Eurostat, \emph{Harmonised Index of Consumer Prices (HICP) - Methodological Manual - 2024 edition}, Publications Office of the European Union, 2024, \doi{10.2785/055028}.
}

\examples{
### EXAMPLE 1

# sample monthly price index:
t <- seq.Date(from=as.Date("2022-12-01"), to=as.Date("2025-12-01"), by="1 month")
p <- rnorm(n=length(t), mean=100, sd=5)

# compute change rates:
rates(x=p, t=t, type="month") # one month to the previous month
rates(x=p, t=t, type="year")  # month to the same month of previous year

# compute annual average rate of change:
pa <- convert(x=p, t=t, type="y") # now annual frequency
rates(x=pa, t=as.Date(names(pa)), type="year")

# compute 12-month average rate of change:
pmvg <- convert(x=p, t=t, type="12mavg") # still monthly frequency
rates(x=pmvg, t=t, type="year")
\donttest{
### EXAMPLE 2: Ribe contributions using published HICP data

library(data.table)
library(restatapi)
options(restatapi_cores=1)  # set cores for testing on CRAN
options(hicp.chatty=FALSE)  # suppress package messages and warnings

# import monthly price indices:
dtp <- hicp::data(id="prc_hicp_minr", filters=list(unit="I25", geo="EA"))
dtp[, "time":=as.Date(paste0(time, "-01"))]
dtp[, "year":=as.integer(format(time, "\%Y"))]
setnames(x=dtp, old="values", new="index")

# import item weights:
dtw <- hicp::data(id="prc_hicp_iw", filters=list(geo="EA"))
dtw[, "time":=as.integer(time)]
setnames(x=dtw, old=c("time","values"), new=c("year","weight"))

# merge price indices and item weights:
dtall <- merge(x=dtp, y=dtw, by=c("geo","coicop18","year"), all.x=TRUE)

# add all-items hicp:
dtall <- merge(x=dtall,
               y=dtall[coicop18=="TOTAL", list(geo,time,index,weight)],
               by=c("geo","time"), all.x=TRUE, suffixes=c("","_all"))

# Ribe contributions by COICOP:
dtall[, "ribe" := contrib(x=index, w=weight, t=time, 
                          x.all=index_all, w.all=weight_all,
                          type="year", settings=list(method="ribe")), by="coicop18"]

# plot annual change rates over time:
plot(rates(x=index, t=time, type="year")~time,
     data=dtall[coicop18=="TOTAL",],
     type="l", ylim=c(-2,12))

# add contribution of energy to plot:
lines(ribe~time, data=dtall[coicop18=="NRG"], col="red")
}}
