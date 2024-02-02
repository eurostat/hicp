\name{rates}

\alias{rates}
\alias{contrib}

\title{Change rates and contributions}

\author{Sebastian Weinand}

\description{
Function \code{rates()} computes monthly, annual and annual average rates of change for an index series. Function \code{contrib()} computes the contributions of a subcomponent to the annual change rate of the overall index.
}

\usage{
rates(x, t=NULL, type="monthly")

contrib(x, w, t, x.all, w.all, method="ribe")
}

\arguments{
  \item{x, x.all}{numeric vector of index values.}
  \item{w, w.all}{numeric vector of weights of the subcomponent (\code{w}) and the overall index (\code{w.all}).}
  \item{t}{date vector.}
  \item{type}{character specifying the type of change rate. Allowed values are \code{monthly} for monthly change rates, \code{annual} for annual change rates, and \code{annual-average} for annual average change rates.}
  \item{method}{character specifying the method used for the calculations. Allowed values are \code{ribe} and \code{kirchner}.}
}

\value{
For \code{rates()}, a numeric vector of the same length as \code{x} if \code{type='monthly'} or \code{type='annual'}. If \code{type='annual-average'}, same length as years available.

For \code{contrib()}, a numeric vector of the same length as \code{x}.
}

\references{
Eurostat (2018), \emph{Harmonised Index of Consumer Prices (HICP) Methodological Manual}, online available at: \url{https://ec.europa.eu/eurostat/web/products-manuals-and-guidelines/-/ks-gq-17-015}.
}

\examples{
require(data.table)

### EXAMPLE 1

P <- rnorm(n=25,mean=100,sd=5)
t <- seq.Date(from=as.Date("2021-01-01"), by="1 month", length.out=length(P))

rates(x=P, type="monthly")
rates(x=P, type="annual")
rates(x=P, type="annual-average")
rates(x=P, t=t, type="annual-average")


### EXAMPLE 2: Contributions using published HICP data

\donttest{

# import monthly price indices:
prc <- hicp.dataimport(id="prc_hicp_midx", filter=list(unit="I15", geo="EA"))
prc[, "time":=as.Date(paste0(time, "-01"))]
prc[, "year":=as.integer(format(time, "\%Y"))]
setnames(x=prc, old="values", new="index")

# import item weights:
inw <- hicp.dataimport(id="prc_hicp_inw", filter=list(geo="EA"))
inw[, "time":=as.integer(time)]
setnames(x=inw, old=c("time","values"), new=c("year","weight"))

# merge price indices and item weights:
hicp.data <- merge(x=prc, y=inw, by=c("geo","coicop","year"), all.x=TRUE)

# add all-items hicp:
hicp.data <- merge(x=hicp.data,
                   y=hicp.data[coicop=="CP00", list(geo,time,index,weight)],
                   by=c("geo","time"), all.x=TRUE, suffixes=c("","_all"))

# ribe decomposition:
hicp.data[, "ribe" := contrib(x=index, w=weight, t=time, 
                              x.all=index_all, w.all=weight_all), by="coicop"]

# annual change rates over time:
plot(rates(x=index, t=time, type="annual")~time,
     data=hicp.data[coicop=="CP00",],
     type="l", ylim=c(-2,12))

# add contribution of energy:
lines(ribe~time, data=hicp.data[coicop=="NRG"], col="red")

# compare to published contributions:
hicp.ctrb <- hicp.dataimport(id="prc_hicp_ctrb")
hicp.ctrb[, "time":=as.Date(paste0(time, "-01"))]
dt.comp <- merge(x=hicp.ctrb,
                 y=hicp.data[, list(coicop, time, ribe)],
                 by=c("coicop","time"),
                 all=TRUE)
head(dt.comp[!is.na(values) & abs(values-ribe)>0.1, ]) # should be empty
}
}
