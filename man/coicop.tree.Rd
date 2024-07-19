\name{coicop.tree}

\alias{tree}

\title{Derive and fix COICOP tree}

\author{Sebastian Weinand}

\description{
Function \code{tree()} derives the COICOP tree at the lowest possible level. In HICP data, this can be done separately for each reporting month and country. Consequently, the COICOP tree can differ across space and time. If needed, specifying argument \code{by} in \code{tree()} allows to merge the COICOP trees at the lowest possible level, e.g. to obtain a unique composition of COICOP codes over time.
}

\usage{
tree(id, by=NULL, w=NULL, max.lvl=NULL, settings=list())
}

\arguments{
  \item{id}{character vector of COICOP ids}
  \item{by}{vector specifying the variable to be used for merging the tree, e.g. vector of dates for merging over time or a vector of countries for merging across space. Can be \code{NULL} if no merging is required.}
  \item{w}{numeric weight of \code{id}. If supplied, it is checked that the weight of children add up to the corresponding weight of the parent (allowing for tolerance \code{w.tol}). If \code{w=NULL} (the default), no checking of weight aggregation is performed.}
  \item{max.lvl}{integer specifying the maximum depth or deepest COICOP level allowed. If \code{NULL} (the default), the deepest level found in \code{id} is used.}
  \item{settings}{a list of control settings to be used. The following settings are supported:
  \itemize{
    \item \code{coicop.version} : the COICOP version to be used when checking for valid COICOP codes. See \code{\link{coicop}} for the allowed values.  The default is \code{getOption("hicp.coicop.version")}.
    \item \code{unbundle} : logical indicating if COICOP bundles (e.g. 08X, 0531_2) as defined in \code{\link{coicop.bundles}} should be taken into account or not. The default is \code{getOption("hicp.unbundle")}.
    \item \code{all.items.code} : character specifying the code internally used for the all-items index. The default is taken from \code{getOption("hicp.all.items.code")}.
    \item \code{w.tol} : numeric tolerance for checking of weights. Only relevant in case \code{w} is not \code{NULL}. The default is 1/100.
  }
  }
}

\value{
A logical vector of the same length as \code{id}.
}

\seealso{
\code{\link{unbundle}}, \code{\link{child}}
}

\examples{
### EXAMPLE 1

# flag lowest possible level to be used as COICOP tree:
tree(id=c("01","011","012"), w=NULL) # true
tree(id=c("01","011","012"), w=c(0.2,0.08,0.12)) # true, weights add up
tree(id=c("01","011","012"), w=c(0.2,0.08,0.10)) # false, weights do not add up

# set maximum (or deepest) coicop level to 3:
tree(id=c("01","011","012","0111","0112","01121"),
     w=c(0.2,0.08,0.12,0.02,0.06,0.06),
     max.lvl=3)

# maximum level=3, but weights do not add up:
tree(id=c("01","011","012","0111","0112","01121"),
     w=c(0.2,0.08,0.07,0.02,0.06,0.06),
     max.lvl=3)

# coicop bundles:
tree(id=c("08","081","082_083"), w=c(0.25,0.05,0.2))
tree(id=c("08","081","082_083"), w=c(0.25,0.05,0.2), settings=list(unbundle=FALSE))

# merge (or fix) coicop tree over time:
tree(id=c("08","081","082","08"), by=c(1,1,1,2))

### EXAMPLE 2
\donttest{
# set cores for testing on CRAN:
library(restatapi)
options(restatapi_cores=1)
library(data.table)

# load hicp item weights:
coicops <- hicp.dataimport(
              id="prc_hicp_inw",
              filter=list(geo=c("EA","DE","FR")),
              date.range=c("2005", NA))
coicops <- coicops[grepl("^CP", coicop),]
coicops[, "coicop":=gsub("^CP", "", coicop)]

# derive seperate trees for each time period and country:
coicops[, "t1" := tree(id=coicop, w=values, settings=list(w.tol=0.1)), by=c("geo","time")]
coicops[t1==TRUE,
        list("n"=uniqueN(coicop),           # varying coicops over time and space
             "w"=sum(values, na.rm=TRUE)),  # weight sums should equal 1000
        by=c("geo","time")]

# derive merged trees over time, but not across countries:
coicops[, "t2" := tree(id=coicop, by=time, w=values, settings=list(w.tol=0.1)), by="geo"]
coicops[t2==TRUE,
        list("n"=uniqueN(coicop),           # same selection over time in a country
             "w"=sum(values, na.rm=TRUE)),  # weight sums should equal 1000
        by=c("geo","time")]

# derive merged trees over countries and time:
coicops[, "t3" := tree(id=coicop, by=paste(geo,time), w=values, settings=list(w.tol=0.1))]
coicops[t3==TRUE,
        list("n"=uniqueN(coicop),           # same selection over time and across countries
             "w"=sum(values, na.rm=TRUE)),  # weight sums should equal 1000
        by=c("geo","time")]
}}
