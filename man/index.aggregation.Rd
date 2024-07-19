\name{index.aggregation}

\alias{jevons}
\alias{carli}
\alias{harmonic}
\alias{laspeyres}
\alias{paasche}
\alias{fisher}
\alias{walsh}
\alias{toernqvist}
\alias{aggregate}

\title{Index number functions and aggregation}

\author{Sebastian Weinand}

\description{
Lower-level price relatives or price indices can be aggregated into higher-level indices in a single step using one of the bilateral index number methods listed below. Function \code{aggregate()} uses these bilateral indices (or others defined by the user) for step-wise aggregation of lower-level subindices into the overall index following the COICOP hierarchy.}

\usage{
# bilateral price indices:
jevons(x, w0=NULL, wt=NULL)
carli(x, w0=NULL, wt=NULL)
harmonic(x, w0=NULL, wt=NULL)
laspeyres(x, w0, wt=NULL)
paasche(x, w0=NULL, wt)
fisher(x, w0, wt)
toernqvist(x, w0, wt)
walsh(x, w0, wt)

# step-wise index aggregation:
aggregate(x, w0, wt, grp, index=laspeyres, add=list(), settings=list())
}

\arguments{
  \item{x}{numeric vector of price relatives obtained by \code{unchain()}ing some HICP index series.}
  \item{w0, wt}{numeric vector of weights in the base period \code{w0} (e.g., for the Laspeyres index) or current period \code{wt} (e.g., for the Paasche index), respectively.}
  \item{grp}{grouping variable to be used. These must be valid COICOP codes according to \code{is.coicop()}.}
  \item{index}{a function or named list of functions specifying the index formula used for aggregation. Each function must have arguments \code{x}, \code{w0} and \code{wt}, even if \code{w0} and/or \code{wt} are not used (this can be indicated by setting this argument to \code{NULL}). Each function must return a scalar. The default is \code{index=laspeyres} since the HICP is calculated as a Laspeyres-type index.}
  \item{add}{a named list of user-defined aggregates to be calculated. Each list element is a vector of ids that can be found in \code{grp}. See \code{settings$add.exact} for further specification of this argument.}
  \item{settings}{A list of control settings to be used. The following settings are supported:
  \itemize{
    \item \code{keep.lowest} : logical indicating if the lowest-level indices that form the base of all aggregation steps should be kept in the function output. The default is \code{TRUE}.
    \item \code{add.exact} : logical indicating if the ids in \code{add} must \bold{all} be present in \code{grp} for aggregation or not. If \code{FALSE}, aggregation is carried out using the available ids in \code{add}. If \code{TRUE} and some ids are missing in \code{add}, \code{NA} is returned. The default is \code{TRUE}.
    \item \code{coicop.version} : the COICOP version to be used when checking for valid COICOP codes. See \code{\link{coicop}} for the allowed values.  The default is \code{getOption("hicp.coicop.version")}.
    \item \code{unbundle} : logical indicating if COICOP bundles (e.g. 08X, 0531_2) as defined in \code{\link{coicop.bundles}} should be taken into account or not. The default is \code{getOption("hicp.unbundle")}.
    \item \code{all.items.code} : character specifying the code internally used for the all-items index. The default is taken from \code{getOption("hicp.all.items.code")}.
  }}
}

\details{
The price indices currently available use price relatives \code{x}. The Dutot index is therefore not implemented.

The functions \code{jevons()}, \code{carli()}, and \code{harmonic()} do not make use of any weights in the calculations. However, they are implemented in a way such that the weights \code{w0} are considered, that is, elements in \code{x} where the weight \code{w0} is \code{NA} are excluded from the calculations. This mimics the behaviour of the weighted index functions like \code{laspeyres()} and can be useful in situations where indices are present but the weight is missing. If, for example, subindices are newly introduced, the index in December is usually set to 100 while the weight of this subindex is not available. The subindex's value in December can thus be excluded by using the weights \code{w0} also in the unweighted price indices.
}

\value{
Functions \code{jevons()}, \code{carli()}, \code{harmonic()}, \code{laspeyres()}, \code{paasche()}, \code{fisher()}, \code{toernqvist()}, and \code{walsh()} return a single (aggregated) value.

Function \code{aggregate()} returns a \code{data.table} of aggregated values at each \code{grp}-level with the following variables:
\tabular{rlll}{
\tab grp \tab \emph{character} \tab the grouping variable \cr
\tab is_aggregated  \tab \emph{logical} \tab is the value an aggregate (\code{TRUE}) or not (\code{FALSE}); column available if \code{settings$keep.lowest=TRUE} \cr
\tab w0, wt  \tab \emph{numeric} \tab sum of weights \code{w0} and \code{wt}; columns available if weights were provided \cr
\tab index  \tab \emph{numeric} \tab aggregates for each \code{index} function
}
}

\seealso{
\code{\link{unchain}}, \code{\link{chain}}, \code{\link{rebase}}
}

\references{
European Commission, Eurostat, \emph{Harmonised Index of Consumer Prices (HICP) - Methodological Manual - 2024 edition}, Publications Office of the European Union, 2024, \url{https://data.europa.eu/doi/10.2785/055028}.
}

\examples{
library(data.table)

### EXAMPLE 1

# data for two times periods:
dt <- data.table(
  "time"=rep(1:2, each=5),
  "coicop"=rep(c("01111","01112","0112","0113","021"), times=2),
  "price"=c(105,103,102,99,120, 105,104,110,98,125),
  "weight"=rep(c(0.05,0.15,0.3,0.2,0.3), times=2),
  "weight_lag"=rep(c(0.03,0.12,0.33,0.2,0.32), times=2))

# aggregate directly to overall index:
dt[, laspeyres(x=price, w0=weight), by="time"]

# gives identical results at top level as with stepwise
# aggregation through all coicop levels:
dt[, aggregate(x=price, w0=weight, grp=coicop, index=laspeyres), by="time"]

# this is no longer the case for the superlative indices as shown
# here for the walsh index:
dt[, walsh(x=price, w0=weight, wt=weight_lag), by="time"]
dt[, aggregate(x=price, w0=weight, wt=weight_lag, grp=coicop, index=walsh), by="time"]

# see also for example Auer and Wengenroth (2017, p. 2)

# apply user-defined function:
dt[, aggregate(x=price, w0=weight, grp=coicop,
               index=list("carli"=function(x,w0=NULL,wt=NULL) mean(x))),
     by="time"]

# add additional, user-defined aggregates (e.g. special aggregates):
dt[, aggregate(x=price, w0=weight, grp=coicop,
               add=list("FOOD"=c("01111","021"), "MISS"=c("021","09"))),
     by="time"]
     
# aggregate 'MISS' is computed if settings$add.exact=FALSE:
dt[, aggregate(x=price, w0=weight, grp=coicop,
               add=list("FOOD"=c("01111","021"), "MISS"=c("021","09")),
               settings=list("add.exact"=FALSE)),
     by="time"]     

### EXAMPLE 2: Index aggregation using published HICP data
\donttest{
# set cores for testing on CRAN:
library(restatapi)
options(restatapi_cores=1)

# import monthly price indices:
prc <- hicp.dataimport(id="prc_hicp_midx", filter=list(unit="I15", geo="EA"))
prc[, "time":=as.Date(paste0(time, "-01"))]
prc[, "year":=as.integer(format(time, "\%Y"))]
setnames(x=prc, old="values", new="index")

# unchaining indices:
prc[, "dec_ratio" := unchain(x=index, t=time), by="coicop"]

# import item weights:
inw <- hicp.dataimport(id="prc_hicp_inw", filter=list(geo="EA"))
inw[, "time":=as.integer(time)]
setnames(x=inw, old=c("time","values"), new=c("year","weight"))

# derive coicop tree:
inw[grepl("^CP",coicop),
    "tree":=tree(id=gsub("^CP","",coicop), w=weight, settings=list(w.tol=0.1)),
    by=c("geo","year")]

# except for rounding, we receive total weight of 1000 in each period:
inw[tree==TRUE, sum(weight), by="year"]

# merge price indices and item weights:
hicp.data <- merge(x=prc, y=inw, by=c("geo","coicop","year"), all.x=TRUE)
hicp.data <- hicp.data[year <= year(Sys.Date())-1 & grepl("^CP\\\\d+", coicop),]
hicp.data[, "coicop" := gsub(pattern="^CP", replacement="", x=coicop)]

# compute all-items HICP:
hicp.own <- hicp.data[tree==TRUE,
                      list("laspey"=laspeyres(x=dec_ratio, w0=weight)),
                      by="time"]
setorderv(x=hicp.own, cols="time")
hicp.own[, "chain_laspey" := chain(x=laspey, t=time, by=12)]
hicp.own[, "chain_laspey_15" := rebase(x=chain_laspey, t=time, t.ref="2015")]

# add published all-items HICP for comparison:
hicp.own <- merge(
            x=hicp.own,
            y=hicp.data[coicop=="00", list(time, index)],
            by="time",
            all.x=TRUE)
plot(index-chain_laspey_15~time, data=hicp.own, type="l")
head(hicp.own[abs(index-chain_laspey_15)>0.1,])

# compute all-items HICP stepwise through all higher-levels:
hicp.own.all <- hicp.data[, aggregate(x=dec_ratio, w0=weight, grp=coicop, index=laspeyres),
                            by="time"]
setorderv(x=hicp.own.all, cols="time")
hicp.own.all[, "chain_laspey" := chain(x=laspeyres, t=time, by=12), by="grp"]
hicp.own.all[, "chain_laspey_15" := rebase(x=chain_laspey, t=time, t.ref="2015"), by="grp"]

# add published indices for compariosn:
hicp.own.all <- merge(
                x=hicp.own.all,
                y=hicp.data[, list(time,"grp"=coicop,index,weight)],
                by=c("time","grp"),
                all.x=TRUE)
hicp.own.all[, "diff" := index-chain_laspey_15]
head(hicp.own.all[abs(diff)>0.1,])
head(hicp.own.all[abs(w0-weight)>0.1,])

# compare all-items HICP from direct and step-wise aggregation:
agg.comp <- merge(
        x=hicp.own.all[grp=="00", list(time, "index_stpwse"=chain_laspey_15)],
        y=hicp.own[, list(time, "index_direct"=chain_laspey_15)],
        by="time")

# no differences -> consistent in aggregation:
head(agg.comp[abs(index_stpwse-index_direct)>1e-4,])
}}
