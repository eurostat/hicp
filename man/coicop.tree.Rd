\name{tree}

\alias{tree}

\title{COICOP tree}

\author{Sebastian Weinand}

\description{
Following the tree structure of COICOP, the function \code{tree()} derives from a given set of COICOP codes those at the lowest possible level.
This can be particularly useful for aggregating from the lowest to the highest level in a single step.
}

\usage{
tree(id, by=NULL, w=NULL, flag=FALSE, settings=list())
}

\arguments{
  \item{id}{character vector of COICOP codes.}
  \item{by}{vector specifying the variable to be used for merging the derived COICOP codes, e.g., a vector of dates to obtain the same composition of COICOP codes over time. 
  If \code{by=NULL}, no merging is performed.}
  \item{w}{numeric weight of \code{id}. 
  If supplied, it is checked that the weights of children add up to the weight of their parent (allowing for tolerance \code{settings$w.tol}).
  If \code{w=NULL}, no checking of weight aggregation is performed.}
  \item{flag}{logical specifying the function output. 
  For \code{FALSE}, a list with the COICOP codes at each level.
  For \code{TRUE}, a logical vector of the same length as \code{id} indicating which COICOP codes in \code{id} define the lowest level.}
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
    \item \code{max.lvl} : integer specifying the maximum (or deepest) COICOP level allowed.
    If \code{NULL} (the default), the maximum level found in \code{id} is used.
    \item \code{w.tol} : numeric tolerance for checking of weights.
    Only relevant if \code{w} is not \code{NULL}.
    The default is 1/100.
  }
  }
}

\value{
Either a list (for \code{flag=FALSE}) or a logical vector of the same length as \code{id} (for \code{flag=TRUE}).
}

\details{
The derivation of COICOP codes at the lowest level follows a top-down-approach.
Starting from the top level of the COICOP tree (usually the all-items code), it is checked if
\enumerate{
  \item the code has children in \code{id},
  \item the children's weights correctly add up to the weight of the parent (if \code{w} provided),
  \item all children can be found in all the groups in \code{by} (if \code{by} provided).
}
Only if all three conditions are met, the children are stored and further processed following the same three steps.
Otherwise, the parent is kept and the processing stops in the respective node.
This process is followed until the lowest level of all codes is reached.

If \code{by} is provided, the function \code{tree()} first subsets all codes in \code{id} to the intersecting levels. 
This ensures that the derivation of the COICOP codes does not directly stops if, for example, the all-items code is missing in one of the groups in \code{by}. 
For example, assume the codes\code{(00,01,02,011,012,021)} for \code{by=1} and \code{(01,011,012,021)} for \code{by=2}. 
In this case, the code \code{00} would be dropped internally first because its level is not available for \code{by=2}. 
The other codes would be processed since their levels intersect across \code{by}. 
However, since \code{(01,02)} do not fulfill the third check, the derivation would stop and no merged tree would be available though codes \code{(011,012,021)} seem to be a solution.
}

\seealso{
\code{\link{child}}, \code{\link{coicop}}, \code{\link{parent}}
}

\examples{
# example codes:
ids <- c("CP01","CP011","CP012","CP0111","CP0112")

# derive lowest level of COICOP tree from top to bottom:
tree(ids) # (CP0111,CP0112,CP012) at lowest level

# or just flag lowest level:
tree(ids, flag=TRUE) 

# still same codes because weights add up:
tree(id=ids, w=c(0.2,0.08,0.12,0.05,0.03)) 

# now (CP011,CP012) because weights do not correctly add up at lower levels:
tree(id=ids, w=c(0.2,0.08,0.12,0.05,0.01)) 

# again (CP011,CP012) because maximum COICOP level limited to 3 digits:
tree(id=c(ids,"01121"),
     w=c(0.2,0.08,0.12,0.02,0.06,0.06),
     settings=list(max.lvl=3)) 

# merge (or fix) COICOP tree over groups:
tree(id=c("TOTAL","CP01","CP02","CP011","CP012", 
          "TOTAL","CP01","CP02","CP011"), 
     by=c(1,1,1,1,1, 2,2,2,2),
     w=c(1,0.3,0.7,0.12,0.18, 1,0.32,0.68,0.15))
# for by=1, the lowest level would be (CP011,CP012,CP02).
# however, CP012 is missing for by=2. therefore, the merged 
# COICOP tree consists of (CP01,CP02) at the lowest level.
}
