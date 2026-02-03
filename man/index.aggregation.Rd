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
  \item{agg}{list of user-defined aggregates to be calculated. For \code{disaggregate()}, the list must have names specifying the aggregate from which indices are deducted. Each list element is a vector of codes that can be found in \code{id}. See \code{settings$exact} for further specification of this argument.}
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
    \item \code{exact} : logical indicating if the codes in \code{agg} must all be present in \code{id} for aggregation or not.
    If \code{FALSE}, aggregation is carried out using the codes present in \code{agg}.
    If \code{TRUE} and some codes cannot be found in \code{id}, \code{NA} is returned.
    The default is \code{TRUE}.
    \item \code{names} : character of names for the aggregates in \code{agg}.
    If not supplied, the aggregates are numbered.
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
\code{\link{unchain}}, \code{\link{tree}}, \code{\link{spec.agg}}
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

# or just use the available ones:
dtagg[, aggregate(x=laspeyres, w0=w0, id=id, 
                  agg=list(c("CP01","CP02","CP03")), 
                  settings=list(exact=FALSE))]

### EXAMPLE 2: Index aggregation using published HICP data
\donttest{
library(restatapi)
options(restatapi_cores=1)  # set cores for testing on CRAN
options(hicp.chatty=FALSE)  # suppress package messages and warnings

# import monthly price indices for euro area since 2014:
dtp <- hicp::data(id="prc_hicp_minr", 
                  date.range=c("2014-12", NA), 
                  filters=list(unit="I25", geo="EA"))
dtp[, "time":=as.Date(paste0(time, "-01"))]
dtp[, "year":=as.integer(format(time, "\%Y"))]
setnames(x=dtp, old="values", new="index")

# unchain all indices for aggregation:
dtp[, "dec_ratio" := unchain(x=index, t=time), by="coicop18"]

# import euro area item weights since 2014:
dtw <- hicp::data(id="prc_hicp_iw", 
                  date.range=c("2014",NA),
                  filters=list(geo="EA"))
dtw[, "time":=as.integer(time)]
setnames(x=dtw, old=c("time","values"), new=c("year","weight"))

# merge price indices and item weights:
dtall <- merge(x=dtp, y=dtw, by=c("geo","coicop18","year"), all.x=TRUE)
dtall <- dtall[year <= year(Sys.Date())-1,]

# derive COICOP tree at lowest possible level:
dtall[weight>0 & !is.na(dec_ratio),
      "tree":=tree(id=coicop18, w=weight, flag=TRUE, settings=list(w.tol=0.1)),
      by="time"]

# except for rounding, we receive total weight of 1000 in each period:
dtall[tree==TRUE, sum(weight), by="time"]

# (1) compute all-items HICP in one step using only lowest-level indices:
hicp.own <- dtall[tree==TRUE,
                  list("laspey"=laspeyres(x=dec_ratio, w0=weight)),
                  by="time"]
setorderv(x=hicp.own, cols="time")
hicp.own[, "chain_laspey" := chain(x=laspey, t=time, by=12)]
hicp.own[, "chain_laspey_25" := rebase(x=chain_laspey, t=time, t.ref="2025")]

# (2) compute all-items HICP gradually through all higher-levels:
hicp.own.all <- dtall[weight>0 & !is.na(dec_ratio), 
                      aggregate.tree(x=dec_ratio, w0=weight, id=coicop18), 
                      by="time"]
setorderv(x=hicp.own.all, cols="time")
hicp.own.all[, "chain_laspey" := chain(x=laspeyres, t=time, by=12), by="id"]
hicp.own.all[, "chain_laspey_25" := rebase(x=chain_laspey, t=time, t.ref="2025"), by="id"]

# (3) compare all-items HICP from direct and gradual aggregation:
all(abs(hicp.own.all[id=="TOTAL", chain_laspey_25]-hicp.own$chain_laspey_25)<0.1)
# no differences -> consistent in aggregation
}}
