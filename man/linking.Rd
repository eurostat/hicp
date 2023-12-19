\name{linking}

\alias{link}
\alias{lsf}

\title{Linking-in new index series}

\author{Sebastian Weinand}

\description{
Function \code{link()} links a new index series to an existing one by an overlap period supplied. In the resulting linked index series, the new index series starts after the existing one. Function \code{lsf()} computes the level-shift factors for linking via the overlap periods in \code{t.overlap}. The level-shift factors can be applied to an index series that has already been linked by the standard HICP one-month overlap method using December of year \code{t-1}.}

\usage{
link(x, x.new, t, t.overlap=NULL)

lsf(x, x.new, t, t.overlap=NULL)
}

\arguments{
  \item{x, x.new}{numeric vector of index values. \code{NA}-values in the vectors indicate when the index series discontinues (for \code{x}) or starts (for \code{x.new}).}
  \item{t}{date vector}
  \item{t.overlap}{character specifying the overlap period to be used. Could be a whole year (\code{YYYY}) or a single year-month (\code{YYYY-MM}). Multiple periods can be provided. If \code{NULL}, all available overlap periods are considered.}
}

\value{
Function \code{link()} returns a numeric vector of the same length as \code{t}, while \code{lsf()} provides a numeric vector of the same length as \code{t.overlap}.
}

\seealso{
\code{\link{chain}}
}

\examples{
require(data.table)

# input data:
set.seed(1)
t <- seq.Date(from=as.Date("2015-01-01"), to=as.Date("2024-05-01"), by="1 month")
x.new <- rnorm(n=length(t), mean=100, sd=5)
x.new <- rebase(x=x.new, t=t, t.ref="2019-12")
x.old <- x.new + rnorm(n=length(x.new), sd=5)
x.old <- rebase(x=x.old, t=t, t.ref="2015")
x.old[t>as.Date("2021-12-01")] <- NA # current index discontinues in 2021
x.new[t<as.Date("2020-01-01")] <- NA # new index starts in 2019-12

# linking in new index in different periods:
plot(x=t, y=link(x=x.old, x.new=x.new, t=t, t.overlap="2021-12"),
     col="red", type="l", xlab=NA, ylab="Index", ylim=c(80,120))
lines(x=t, y=link(x=x.old, x.new=x.new, t=t, t.overlap="2020"), col="blue")
lines(x=t, y=link(x=x.old, x.new=x.new, t=t, t.overlap="2021"), col="green")
lines(x=t, y=x.old, col="black")
abline(v=as.Date("2021-12-01"), lty="dashed")
legend(x="topleft",
       legend=c("One-month overlap using December 2021",
                "Annual overlap using 2021",
                "Annual overlap using 2020"),
       fill=c("red","green","blue"), bty = "n")

# compute level-shift factors:
lsf(x=x.old, x.new=x.new, t=t, t.overlap=c("2020","2021"))

# level-shift factors can be applied to already chain-linked index series
# to obtain linked series using another overlap period:
x.new.chained <- link(x=x.old, x.new=x.new, t=t, t.overlap="2021-12")

# level-shift adjustment:
x.new.adj <- ifelse(test=t>as.Date("2021-12-01"),
                yes=x.new.chained*lsf(x=x.old, x.new=x.new, t=t, t.overlap="2020"),
                no=x.new.chained)

# compare:
all.equal(x.new.adj, link(x=x.old, x.new=x.new, t=t, t.overlap="2020"))
}
