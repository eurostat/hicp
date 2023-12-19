\name{index.aggregation}

\alias{laspey}
\alias{paasche}
\alias{fisher}
\alias{walsh}
\alias{toernq}
\alias{aggregate}

\title{Index number functions and aggregation}

\author{Sebastian Weinand}

\description{
Currently, the following index number methods are implemented: \code{laspey()} for the Laspeyres index, \code{paasche()} for the Paasche index, \code{toernq()} for the Toernqvist index, \code{fisher()} for the Fisher index, and \code{walsh()} for the Walsh index. Function \code{aggregate()} uses these functions (or other functions provided by the user) for step-wise aggregation of lower-level subindices into the overall index following the COICOP hierarchy.}

\usage{
laspey(x, w0, wt=NULL)
paasche(x, w0=NULL, wt)
fisher(x, w0, wt)
toernq(x, w0, wt)
walsh(x, w0, wt)

aggregate(x, w0, wt, grp, index=laspey, add=list(), keep.lowest=TRUE)
}

\arguments{
  \item{x}{numeric vector of price relatives obtained by \code{unchain}ing some HICP index series.}
  \item{w0, wt}{numeric vector of weights in the base period \code{w0} (e.g., for the Laspeyres index) or current period \code{wt} (e.g., for the Paasche index), respectively.}
  \item{grp}{grouping variable to be used. These should be valid COICOP codes or other hierarchical codes.}
  \item{index}{a function or named list of functions specifying the index formula used for aggregation. Each function must have arguments \code{x}, \code{w0} and \code{wt}, even if the attributes are not used (this can be indicated by setting this argument to \code{NULL}). Each function must return a scalar. Default is \code{index=laspey}.}
  \item{add}{a named list of user-defined aggregates to be calculated. Each list element is a vector of ids that can be found in \code{grp}. Only if all ids are present in \code{grp}, aggregation is done; otherwise \code{NA}.}
  \item{keep.lowest}{logical indicating if the lowest-level indices that form the base of all aggregation steps should be kept in the output.}
}

\value{
Functions \code{laspey()}, \code{paasche()}, \code{fisher()}, \code{toernq()}, and \code{walsh()} return a single (aggregated) value.

Function \code{aggregate()} returns a \code{data.table} of aggregated values at each \code{grp}-level with the following variables:
\tabular{rlll}{
\tab grp \tab \emph{character} \tab the grouping variable \cr
\tab is_aggregated  \tab \emph{logical} \tab is the value an aggregate (\code{TRUE}) or not; column available if \code{keep.lowest=TRUE} \cr
\tab w0, wt  \tab \emph{numeric} \tab sum of weights \code{w0} and \code{wt} \cr
\tab index  \tab \emph{numeric} \tab aggregates for each \code{index} function
}
}

\seealso{
\code{\link{unchain}}, \code{\link{chain}}, \code{\link{rebase}}
}

\references{
Eurostat (2018), \emph{Harmonised Index of Consumer Prices (HICP) Methodological Manual}, online available at: \url{https://ec.europa.eu/eurostat/web/products-manuals-and-guidelines/-/ks-gq-17-015}.
}

\examples{
require(data.table)

### EXAMPLE 1

# data for two times periods:
dt <- data.table(
  "time"=rep(1:2, each=5),
  "coicop"=rep(c("01111","01112","0112","0113","021"), times=2),
  "price"=c(105,103,102,99,120, 105,104,110,98,125),
  "weight"=rep(c(0.05,0.15,0.3,0.2,0.3), times=2),
  "weight_lag"=rep(c(0.03,0.12,0.33,0.2,0.32), times=2))

# aggregate directly to overall index:
dt[, laspey(x=price, w0=weight), by="time"]

# gives identical results at top level as with stepwise
# aggregation through all coicop levels:
dt[, aggregate(x=price, w0=weight, grp=coicop, index=laspey), by="time"]

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


### EXAMPLE 2: Index aggregation using published HICP data

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
    "tree":=tree(id=gsub("^CP","",coicop), w=weight, w.tol=0.1),
    by=c("geo","year")]

# except for rounding, we receive total weight of 1000 in each period:
inw[tree==TRUE, sum(weight), by="year"]

# merge price indices and item weights:
hicp.data <- merge(x=prc, y=inw, by=c("geo","coicop","year"), all.x=TRUE)
hicp.data <- hicp.data[year <= year(Sys.Date())-1 & grepl("^CP\\\\d+", coicop),]
hicp.data[, "coicop" := gsub(pattern="^CP", replacement="", x=coicop)]

# compute all-items HICP:
hicp.own <- hicp.data[tree==TRUE,
                      list("laspey"=laspey(x=dec_ratio, w0=weight)),
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
hicp.own[abs(index-chain_laspey_15)>0.1,]

# compute all-items HICP stepwise through all higher-levels:
hicp.own.all <- hicp.data[, aggregate(x=dec_ratio, w0=weight, grp=coicop, index=laspey),
                            by="time"]
setorderv(x=hicp.own.all, cols="time")
hicp.own.all[, "chain_laspey" := chain(x=laspey, t=time, by=12), by="grp"]
hicp.own.all[, "chain_laspey_15" := rebase(x=chain_laspey, t=time, t.ref="2015"), by="grp"]

# add published indices for compariosn:
hicp.own.all <- merge(
                x=hicp.own.all,
                y=hicp.data[, list(time,"grp"=coicop,index,weight)],
                by=c("time","grp"),
                all.x=TRUE)
hicp.own.all[, "diff" := index-chain_laspey_15]
View(hicp.own.all[abs(diff)>0.1,])
View(hicp.own.all[abs(w0-weight)>0.1,])

# compare all-items HICP from direct and step-wise aggregation:
agg.comp <- merge(
        x=hicp.own.all[grp=="00", list(time, "index_stpwse"=chain_laspey_15)],
        y=hicp.own[, list(time, "index_direct"=chain_laspey_15)],
        by="time")

# no differences -> consistent in aggregation:
agg.comp[abs(index_stpwse-index_direct)>1e-4,]
}
