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

\title{Index number formulas and aggregation}

\author{Sebastian Weinand}

\description{
Lower-level price indices can be aggregated into higher-level indices in a single step using the bilateral index formulas below or gradually following the COICOP tree structure with the function \code{aggregate.tree()}.

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
  \item{x}{numeric vector of price relatives between two periods, typically obtained from \code{unchain()}.}
  \item{w0, wt}{numeric vector of weights in the base period \code{w0} (e.g., for the Laspeyres index) or current period \code{wt} (e.g., for the Paasche index).}
  \item{id}{character vector of aggregate codes. For \code{aggregate.tree()}, only valid COICOP (bundle) codes are processed.}
  \item{formula}{function or named list of functions specifying the index formula(s) used for aggregation. Each function must return a scalar and have the argument \code{x}. For weighted index formulas, the arguments \code{w0} and/or \code{wt} must be available as well.}
  \item{agg}{list of user-defined aggregates to be calculated. For \code{disaggregate()}, the list must have names specifying the aggregate from which indices are deducted. Each list element is a vector of codes that can be found in \code{id}. If any code cannot be found in \code{id}, \code{NA} is returned.}
  \item{settings}{list of control settings to be used. The following settings are supported:
  \itemize{
    \item \code{chatty} : logical indicating if package-specific warnings and info messages should be printed or not.
    The default is \code{getOption("hicp.chatty")}.
    \item \code{coicop.version} : character specifying the COICOP version to be used.
    See \code{\link{coicop}} for the allowed values.
    The default is \code{getOption("hicp.coicop.version")}.
    \item \code{coicop.prefix} : character specifying a prefix for the COICOP codes.
    The default is \code{getOption("hicp.coicop.prefix")}.
    \item \code{all.items.code} : character specifying the code internally used for the all-items index.
    The default is taken from \code{getOption("hicp.all.items.code")}.
    \item \code{names} : character of names for the aggregates in \code{agg}.
    If not supplied, the aggregates are numbered.
  }}
}

\details{
The bilateral index formulas currently available are intended for the aggregation of (unchained) price relatives \code{x}.
}

\value{
The functions \code{jevons()}, \code{carli()}, \code{harmonic()}, \code{laspeyres()}, \code{paasche()}, \code{fisher()}, \code{toernqvist()}, and \code{walsh()} return a single aggregated value.

The functions \code{aggregate()}, \code{disaggregate()} and \code{aggregate.tree()} return a \code{data.table} with the sum of weights \code{w0} and \code{wt} (if supplied) and the index aggregates for each \code{formula}.
}

\seealso{
\code{\link{unchain}}, \code{\link{tree}}, \code{\link{special.agg}}
}

\references{
European Commission, Eurostat, \emph{Harmonised Index of Consumer Prices (HICP) - Methodological Manual - 2024 edition}, Publications Office of the European Union, 2024, \doi{10.2785/055028}.
}

\examples{
library(data.table)

### EXAMPLE 1

# example data with unchained prices and weights:
dt <- data.table("coicop"=c("CP0111","CP0112","CP012","CP021","CP022"),
                 "price"=c(102,105,99,109,115)/100,
                 "weight"=c(0.2,0.15,0.4,0.2,0.05))

# aggregate directly into overall index:
dt[, laspeyres(x=price, w0=weight)]

# same result at top level with gradual aggregation:
(dtagg <- dt[, aggregate.tree(x=price, w0=weight, id=coicop)])

# compute user-defined aggregates by disaggregation:
dtagg[, disaggregate(x=laspeyres, w0=w0, id=id,
                     agg=list("TOTAL"=c("CP01"), "TOTAL"=c("CP022")),
                     settings=list(names=c("A","B")))]

# which can be similarly derived by aggregation:
dtagg[, aggregate(x=laspeyres, w0=w0, id=id,
                  agg=list(c("CP021","CP022"), c("CP011","CP012","CP021")),
                  settings=list(names=c("A","B")))]

# same aggregates by several index formulas:
dtagg[, aggregate(x=laspeyres, w0=w0, id=id,
                  agg=list(c("CP021","CP022"), c("CP011","CP012","CP021")),
                  formula=list("lasp"=laspeyres, "jev"=jevons, "mean"=mean),
                  settings=list(names=c("A","B")))]

# no aggregation if one index is missing:
dtagg[, aggregate(x=laspeyres, w0=w0, id=id, 
                  agg=list(c("CP01","CP02","CP03")),
                  settings=list(exact=TRUE))]


### EXAMPLE 2: Index aggregation using published HICP data
\donttest{
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

# unchain all indices for aggregation:
dtall[, "dec_ratio" := unchain(x=index, t=time), by="coicop18"]

# derive COICOP tree at lowest possible level:
dtall[weight>0 & !is.na(dec_ratio),
      "tree":=tree(id=coicop18, w=weight, flag=TRUE, settings=list(w.tol=0.1)),
      by="time"]

# except for rounding, we receive total weight of 1000 in each period:
dtall[tree==TRUE, sum(weight), by="time"]

# (1) compute all-items HICP in one step using only lowest-level indices:
dthicp <- dtall[tree==TRUE,
                list("laspeyres"=laspeyres(x=dec_ratio, w0=weight)),
                by="time"]
setorderv(x=dthicp, cols="time")
dthicp[, "index" := chain(x=laspeyres, t=time, by=12)]
dthicp[, "index25" := rebase(x=index, t=time, t.ref="2025")]

# (2) compute all-items HICP gradually through all higher-levels:
dthicp.all <- dtall[level(coicop18)<5L,
                    aggregate.tree(x=dec_ratio, w0=weight, id=coicop18),
                    by="time"]
setorderv(x=dthicp.all, cols="time")
dthicp.all[, "index" := chain(x=laspeyres, t=time, by=12), by="id"]
dthicp.all[, "index25" := rebase(x=index, t=time, t.ref="2025"), by="id"]
# -> all-items HICP identical to the one in hicp.own due to consistency in aggregation
}}
