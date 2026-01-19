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
\alias{disaggregate}
\alias{aggregate.tree}

\title{Index number functions and aggregation}

\author{Sebastian Weinand}

\description{
Lower-level price indices can be aggregated into higher-level indices in a single step using the bilateral index formulas below or gradually following the COICOP tree with the function \code{aggregate.tree()}.

The functions \code{aggregate()} and \code{disaggregate()} can be used for the calculation of user-defined aggregates (e.g., HICP special aggregates).
For \code{aggregate()}, lower-level indices are aggregated into the respective total.
For \code{disaggregate()}, they are deducted from the total to receive a subaggregate.
}

\usage{
# bilateral price index formulas:
jevons(x)
carli(x)
harmonic(x)
laspeyres(x, w0)
paasche(x, wt)
fisher(x, w0, wt)
toernqvist(x, w0, wt)
walsh(x, w0, wt)

# aggregation into user-defined aggregates:
aggregate(x, w0, wt, id, formula=laspeyres, agg=list(), settings=list())

# disaggregation into user-defined aggregates:
disaggregate(x, w0, id, agg=list(), settings=list())

# gradual aggregation following the COICOP tree:
aggregate.tree(x, w0, wt, id, formula=laspeyres, settings=list())
}

\arguments{
  \item{x}{numeric vector of price relatives between two periods, typically obtained by unchaining some HICP index series.}
  \item{w0, wt}{numeric vector of weights in the base period \code{w0} (e.g., for the Laspeyres index) or current period \code{wt} (e.g., for the Paasche index).}
  \item{id}{character vector of aggregate codes. For \code{aggregate.tree()}, only valid COICOP codes or bundle codes are processed.}
  \item{formula}{a function or named list of functions specifying the index formula(s) used for aggregation. Each function must return a scalar and have the argument \code{x}. For weighted index formulas, the arguments \code{w0} and/or \code{wt} must be available as well.}
  \item{agg}{list of user-defined aggregates to be calculated. For \code{disaggregate()}, the list must have names specifying the aggregate from which indices are deducted. Each list element is a vector of codes that can be found in \code{id}. See \code{settings$exact} for further specification of this argument.}
  \item{settings}{list of control settings to be used. The following settings are supported:
  \itemize{
    \item \code{chatty} : logical indicating if package-specific warnings and info messages should be printed or not. The default is \code{getOption("hicp.chatty")}.
    \item \code{coicop.version} : character specifying the COICOP version to be used for flagging valid COICOP codes. See \code{\link{coicop}} for the allowed values. The default is \code{getOption("hicp.coicop.version")}.
    \item \code{all.items.code} : character specifying the code internally used for the all-items index. The default is taken from \code{getOption("hicp.all.items.code")}.
    \item \code{coicop.bundles} : named list specifying the COICOP bundle code dictionary used for unbundling any bundle codes in \code{id}. The default is \code{getOption("hicp.coicop.bundles")}.
    \item \code{exact} : logical indicating if the codes in \code{agg} must all be present in \code{id} for aggregation or not. If \code{FALSE}, aggregation is carried out using the codes present in \code{agg}. If \code{TRUE} and some codes cannot be found in \code{id}, \code{NA} is returned. The default is \code{TRUE}.
    \item \code{names} : character of names for the aggregates in \code{agg}. If not supplied, the aggregates are numbered.
  }}
}

\details{
The bilateral index formulas currently available are intended for the aggregation of (unchained) price relatives \code{x}. The Dutot index is therefore not implemented.
}

\value{
The functions \code{jevons()}, \code{carli()}, \code{harmonic()}, \code{laspeyres()}, \code{paasche()}, \code{fisher()}, \code{toernqvist()}, and \code{walsh()} return a single aggregated value.

The functions \code{aggregate()}, \code{disaggregate()} and \code{aggregate.tree()} return a \code{data.table} with the sum of weights \code{w0} and \code{wt} (if supplied) and the computed aggregates for each index formula specified by \code{formula}.
}

\seealso{
\code{\link{unchain}}, \code{\link{chain}}, \code{\link{rebase}}
}

\references{
European Commission, Eurostat, \emph{Harmonised Index of Consumer Prices (HICP) - Methodological Manual - 2024 edition}, Publications Office of the European Union, 2024, \doi{10.2785/055028}.
}

\examples{
library(data.table)

### EXAMPLE 1

# example data with unchained prices and weights:
dt <- data.table("coicop"=c("0111","0112","012","021","022"),
                 "price"=c(102,105,99,109,115),
                 "weight"=c(0.2,0.15,0.4,0.2,0.05))

# aggregate directly into overall index:
dt[, laspeyres(x=price, w0=weight)]

# same result at top level with gradual aggregation:
(dtagg <- dt[, aggregate.tree(x=price, w0=weight, id=coicop)])

# compute user-defined aggregates by disaggregation:
dtagg[, disaggregate(x=laspeyres, w0=w0, id=id,
                     agg=list("00"=c("01"), "00"=c("022")),
                     settings=list(names=c("A","B")))]

# which can be similarly derived by aggregation:
dtagg[, aggregate(x=laspeyres, w0=w0, id=id,
                  agg=list(c("021","022"), c("011","012","021")),
                  settings=list(names=c("A","B")))]

# same aggregates by several index formulas:
dtagg[, aggregate(x=laspeyres, w0=w0, id=id,
                  agg=list(c("021","022"), c("011","012","021")),
                  formula=list("lasp"=laspeyres, "jev"=jevons, "mean"=mean),
                  settings=list(names=c("A","B")))]

# no aggregation if one index is missing:
dtagg[, aggregate(x=laspeyres, w0=w0, id=id, 
                  agg=list(c("01","02","03")),
                  settings=list(exact=TRUE))]

# or just use the available ones:
dtagg[, aggregate(x=laspeyres, w0=w0, id=id, 
                  agg=list(c("01","02","03")), 
                  settings=list(exact=FALSE))]

### EXAMPLE 2: Index aggregation using published HICP data
\donttest{
library(restatapi)
options(restatapi_cores=1)  # set cores for testing on CRAN
options(hicp.chatty=FALSE)  # suppress package messages and warnings

# import monthly price indices:
prc <- hicp::data(id="prc_hicp_midx", filters=list(unit="I15", geo="EA"))
prc[, "time":=as.Date(paste0(time, "-01"))]
prc[, "year":=as.integer(format(time, "\%Y"))]
setnames(x=prc, old="values", new="index")

# unchaining indices:
prc[, "dec_ratio" := unchain(x=index, t=time), by="coicop"]

# import item weights:
inw <- hicp::data(id="prc_hicp_inw", filters=list(geo="EA"))
inw[, "time":=as.integer(time)]
setnames(x=inw, old=c("time","values"), new=c("year","weight"))

# derive coicop tree at lowest possible level:
inw[grepl("^CP",coicop),
    "tree":=tree(id=gsub("^CP","",coicop), w=weight, flag=TRUE, settings=list(w.tol=0.1)),
    by=c("geo","year")]

# except for rounding, we receive total weight of 1000 in each period:
inw[tree==TRUE, sum(weight), by="year"]

# merge price indices and item weights:
hicp.data <- merge(x=prc, y=inw, by=c("geo","coicop","year"), all.x=TRUE)
hicp.data <- hicp.data[year <= year(Sys.Date())-1 & grepl("^CP\\\\d+", coicop),]
hicp.data[, "coicop" := gsub(pattern="^CP", replacement="", x=coicop)]

# compute all-items HICP in one step using only lowest-level indices:
hicp.own <- hicp.data[tree==TRUE,
                      list("laspey"=laspeyres(x=dec_ratio, w0=weight)),
                      by="time"]
setorderv(x=hicp.own, cols="time")
hicp.own[, "chain_laspey" := chain(x=laspey, t=time, by=12)]
hicp.own[, "chain_laspey_15" := rebase(x=chain_laspey, t=time, t.ref="2015")]

# compute all-items HICP gradually through all higher-levels:
hicp.own.all <- hicp.data[, aggregate.tree(x=dec_ratio, w0=weight, id=coicop), by="time"]
setorderv(x=hicp.own.all, cols="time")
hicp.own.all[, "chain_laspey" := chain(x=laspeyres, t=time, by=12), by="id"]
hicp.own.all[, "chain_laspey_15" := rebase(x=chain_laspey, t=time, t.ref="2015"), by="id"]

# compare all-items HICP from direct and gradual aggregation:
agg.comp <- merge(x=hicp.own.all[id=="00", list(time, "index_stpwse"=chain_laspey_15)],
                  y=hicp.own[, list(time, "index_direct"=chain_laspey_15)],
                  by="time")

# no differences -> consistent in aggregation:
head(agg.comp[abs(index_stpwse-index_direct)>1e-4,])
}}
