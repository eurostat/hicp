\name{coicop.relatives}

\alias{child}
\alias{parent}

\title{COICOP relatives}

\author{Sebastian Weinand}

\description{
The functions \code{parent()} and \code{child()} derive the higher-level parents or lower-level children of a COICOP code.
}

\usage{
child(id, usedict=TRUE, closest=TRUE, k=1, settings=list())

parent(id, usedict=TRUE, closest=TRUE, k=1, settings=list())
}

\arguments{
  \item{id}{character vector of COICOP codes.}
  \item{usedict}{logical indicating if parents or children should be derived from the full code dictionary defined by \code{settings$coicop.version} (if set to \code{TRUE}) or only from the codes present in \code{id}.}
  \item{closest}{logical indicating if the closest parents or children should be derived or the \emph{k}-th ones defined by \code{k}. For example, if set to \code{TRUE}, the closest parent could be the direct parent for one code (e.g. \code{031->03}) and the grandparent for another (e.g. \code{0321->03}).}
  \item{k}{integer specifying the \emph{k}-th relative (e.g., \code{1} for direct parents or children, \code{2} for grandparents and grandchildren, ...). Multiple values allowed, e.g., \code{k=c(1,2)}. Only relevant if \code{closest=FALSE}.}
  \item{settings}{list of control settings to be used. The following settings are supported:
  \itemize{
    \item \code{coicop.version} : character specifying the COICOP version to be used.
    See \code{\link{coicop}} for the allowed values.
    The default is \code{getOption("hicp.coicop.version")}.
    \item \code{coicop.prefix} : character specifying a prefix for the COICOP codes.
    The default is \code{getOption("hicp.coicop.prefix")}.
    \item \code{all.items.code} : character specifying the code internally used for the all-items index. 
    The default is taken from \code{getOption("hicp.all.items.code")}.
    \item \code{simplify} : logical indicating if the output should be simplified into a vector if possible.
    The default is \code{FALSE}.
    If both a COICOP bundle code and the underlying codes are the parent or child, only the latter codes are kept (e.g., \code{c(08X,082)->082}). Note that simplification usually only works for \code{parent()}.
  }
  }
}

\value{
A list with the same length as \code{id}.
}

\seealso{
\code{\link{coicop}}, \code{\link{tree}}
}

\examples{
# no children of CP01 present in id:
child(id="CP01", usedict=FALSE) 

# still no direct child present:
child(id=c("CP01","CP0111"), usedict=FALSE, closest=FALSE, k=1) 

# but a grandchild of CP01 is found:
child(id=c("CP01","CP0111"), usedict=FALSE, closest=TRUE) 

# now, get the children directly from the code dictionary:
child(id=c("CP01","CP0111"), usedict=TRUE) 

# which works analogously for parents:
parent(id=c("CP01","CP0111"), usedict=TRUE)
}
