\name{tree}

\alias{tree}

\title{Derive and fix COICOP tree}

\author{Sebastian Weinand}

\description{
Function \code{tree()} derives the COICOP tree at the lowest possible level. In HICP data, this can be done separately for each country and year. Consequently, the COICOP tree can differ across space and time. If needed, however, specifying the argument \code{by} in \code{tree()} allows to merge the COICOP trees at the lowest possible level, e.g. to obtain a unique composition of COICOP codes over time.
}

\usage{
tree(id, by=NULL, w=NULL, flag=FALSE, settings=list())
}

\arguments{
  \item{id}{character vector of COICOP codes.}
  \item{by}{vector specifying the variable to be used for merging the tree, e.g. vector of dates for merging over time or a vector of countries for merging across space. If \code{by=NULL} (the default), no merging is performed.}
  \item{w}{numeric weight of \code{id}. If supplied, it is checked that the weights of children add up to the weight of their parent (allowing for tolerance \code{w.tol}). If \code{w=NULL} (the default), no checking of weight aggregation is performed.}
  \item{flag}{logical specifying the function output. For \code{FALSE} (the default), a list with the codes defining the COICOP tree at each level. For \code{TRUE}, a logical vector of the same length as \code{id} indicating which elements in \code{id} define the lowest level of the COICOP tree.}
  \item{settings}{list of control settings to be used. The following settings are supported:
  \itemize{
    \item \code{chatty} : logical indicating if package-specific warnings and info messages should be printed or not. The default is \code{getOption("hicp.chatty")}.
    \item \code{coicop.version} : character specifying the COICOP version to be used for flagging valid COICOP codes. See \code{\link{coicop}} for the allowed values. The default is \code{getOption("hicp.coicop.version")}.
    \item \code{all.items.code} : character specifying the code internally used for the all-items index. The default is taken from \code{getOption("hicp.all.items.code")}.
    \item \code{coicop.bundles} : named list specifying the COICOP bundle code dictionary used for unbundling any bundle codes in \code{id}. The default is \code{getOption("hicp.coicop.bundles")}.
    \item \code{max.lvl} : integer specifying the maximum depth or deepest COICOP level allowed. If \code{NULL} (the default), the deepest level found in \code{id} is used.
    \item \code{w.tol} : numeric tolerance for checking of weights. Only relevant if \code{w} is not \code{NULL}. The default is 1/100.
  }
  }
}

\value{
Either a list (for \code{flag=FALSE}) or a logical vector of the same length as \code{id} (for \code{flag=TRUE}).
}

\details{
The derivation of the COICOP tree follows a top-down-approach. Starting from the top level (usually the all-items code), it is checked if
\enumerate{
  \item the code in \code{id} has children,
  \item the children's weights correctly add up to the weight of the parent (if \code{w} provided),
  \item all children can be found in all the groups in \code{by} (if \code{by} provided).
}
Only if all three conditions are met, the children are stored and further processed. Otherwise, the parent is kept and the processing stops in the respective node. This process is followed until the lowest level of all codes is reached.

If \code{by} is provided, function \code{tree()} first subsets all codes in \code{id} to the intersecting levels. 
This ensures that the derivation of the COICOP tree does not directly stops if, for example, the all-items code is missing in one of the groups in \code{by}. 
For example, assume the codes\code{(00,01,02,011,012,021)} for \code{by=1} and \code{(01,011,012,021)} for \code{by=2}. 
In this case, the code \code{00} would be dropped internally first because its level is not available for \code{by=2}. 
The other codes would be processed since their levels intersect across \code{by}. 
However, since \code{(01,02)} do not fulfill the third check, the derivation would stop and no merged tree would be available though codes \code{(011,012,021)} seem to be a solution.
}

\seealso{
\code{\link{unbundle}}, \code{\link{parent}}
}

\examples{
### EXAMPLE 1

# derive COICOP tree from top to bottom:
tree(id=c("01","011","012","0111","0112")) # (0111,0112,012) at lowest level

# or just flag lowest level of COICOP tree:
tree(id=c("01","011","012","0111","0112"), flag=TRUE) 

# still same tree because weights add up:
tree(id=c("01","011","012","0111","0112"), w=c(0.2,0.08,0.12,0.05,0.03)) 

# now (011,012) because weights do not correctly add up at lower levels:
tree(id=c("01","011","012","0111","0112"), w=c(0.2,0.08,0.12,0.05,0.01)) 

# again (011,012) because maximum (or deepest) coicop level to 3 digits:
tree(id=c("01","011","012","0111","0112","01121"),
     w=c(0.2,0.08,0.12,0.02,0.06,0.06),
     settings=list(max.lvl=3)) 

# coicop bundles are used if their underlying codes are not all present:
tree(id=c("08","081","082","082_083"), w=c(0.25,0.05,0.15,0.2))
# (081,082_083) where 082 is dropped because 083 is missing

# merge (or fix) coicop tree over groups:
tree(id=c("00","01","011","012", "00","01","011"), by=c(1,1,1,1,2,2,2))
# 01 is present in both by=(1,2) while 012 is missing in by=2

### EXAMPLE 2: Working with published HICP data
\donttest{
library(data.table)
library(restatapi)
options(restatapi_cores=1) # set cores for testing on CRAN
options(hicp.chatty=FALSE) # suppress package messages and warnings

# load HICP item weights:
coicops <- hicp::data(id="prc_hicp_inw",
                      filter=list(geo=c("EA","DE","FR")), 
                      date.range=c("2005", NA))
coicops <- coicops[grepl("^CP", coicop),]
coicops[, "coicop":=gsub("^CP", "", coicop)]

# derive seperate trees for each time period and country:
coicops[, "t1" := tree(id=coicop, w=values, 
                       flag=TRUE, settings=list(w.tol=0.1)), by=c("geo","time")]
coicops[t1==TRUE,
        list("n"=uniqueN(coicop),           # varying coicops over time and space
             "w"=sum(values, na.rm=TRUE)),  # weight sums should equal 1000
        by=c("geo","time")]

# derive merged trees over time, but not across countries:
coicops[, "t2" := tree(id=coicop, by=time, w=values, 
                       flag=TRUE, settings=list(w.tol=0.1)), by="geo"]
coicops[t2==TRUE,
        list("n"=uniqueN(coicop),           # same selection over time in a country
             "w"=sum(values, na.rm=TRUE)),  # weight sums should equal 1000
        by=c("geo","time")]

# derive merged trees over countries and time:
coicops[, "t3" := tree(id=coicop, by=paste(geo,time), w=values, 
                       flag=TRUE, settings=list(w.tol=0.1))]
coicops[t3==TRUE,
        list("n"=uniqueN(coicop),           # same selection over time and across countries
             "w"=sum(values, na.rm=TRUE)),  # weight sums should equal 1000
        by=c("geo","time")]
}}
