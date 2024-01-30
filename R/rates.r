# START

# Title:  Change rates and contributions
# Author: Sebastian Weinand
# Date:   19 January 2024

# compute change rates:
rates <- function(x, t=NULL, type="monthly"){

  # input checks:
  check.num(x=x)
  check.date(x=t, null.ok=TRUE, na.ok=FALSE, chronological=FALSE)
  check.lengths(x=x, y=t)
  check.char(x=type, min.len=1, max.len=1, na.ok=FALSE)

  # match type:
  type <- match.arg(arg=type, choices=c("monthly","annual","annual-average"))

  # check if time period is available or not:
  t.null <- is.null(t)

  if(t.null){

    # make assumption on time period for calculations,
    # only needed for type='annual-average':
    t <- t0 <- seq.Date(from=as.Date("2000-01-01"), by="1 month", length.out=length(x))

  }else{

    # ensure chronological order:
    t <- as.Date(format(t, "%Y-%m-01"))
    t0 <- seq.Date(from=min(t, na.rm=TRUE), to=max(t, na.rm=TRUE), by="1 month")
    x <- x[match(x=t0, table=t)]

  }

  # monthly change rate:
  if(type=="monthly") res <- x/shift(x, n=1)

  # annual change rate:
  if(type=="annual") res <- x/shift(x, n=12)

  # annual average change rate:
  if(type=="annual-average"){

    y <- format(t0, "%Y")
    res <- tapply(X=x, INDEX=y, FUN=mean, na.rm=TRUE)
    res[tapply(X=x, INDEX=y, FUN=length)<12] <- NA
    res <- c(res/shift(as.vector(res), n=1))
    if(t.null) names(res) <- NULL # drop names

  }else{

    # reorder to initial ordering:
    res <- res[match(x=t, table=t0)]

  }

  # transform into percentage change:
  res <- 100*(res-1)

  # print output to console:
  return(res)

}

# compute contributions to annual change rate:
contrib <- function(x, w, t, x.all, w.all, method="ribe"){

  # input checks:
  check.num(x=x)
  check.num(x=w)
  check.date(x=t, na.ok=FALSE)
  check.num(x=x.all)
  check.num(x=w.all)
  check.lengths(x=x, y=w)
  check.lengths(x=x, y=t)
  check.lengths(x=x, y=x.all)
  check.lengths(x=x, y=w.all)
  check.char(x=method, min.len=1, max.len=1, na.ok=FALSE)

  # match argument:
  method <- match.arg(arg=method, choices=c("ribe","kirchner"))

  # unchain indices:
  x.unchained <- unchain(x=x, t=t, by=12)
  x.all.unchained <- unchain(x=x.all, t=t, by=12)
  # # set total values if missing; this leads to an outcome
  # # that coincides with the annual rates of change of 'x':
  # if(is.null(x.all) | is.null(w.all)){
  #   w.all <- w
  #   x.all.unchained <- x.unchained
  #   x.all <- x
  # }else{
  #   x.all.unchained <- unchain(x=x.all, t=t, by=12)
  # }

  # rescale indices:
  t.dec <- format(t, "%m")==12L
  x.dec <- x.unchained[t.dec]
  x.rescaled <- x.dec[match(x=format(t, "%Y"), table=format(t[t.dec], "%Y"))]/x.unchained
  x.all.dec <- x.all.unchained[t.dec]
  x.all.rescaled <- x.all.dec[match(x=format(t, "%Y"), table=format(t[t.dec], "%Y"))]/x.all.unchained

  # rescale item weights:
  w.rescaled <- w*x.unchained/x.all.unchained
  w.all.rescaled <- w.all*x.all.unchained/x.all.unchained

  # ribe decompositon:
  if(method=="ribe"){
    # compute this-year term and last-year term:
    ty <- 100*shift(x=x.all.rescaled, n=12)*(x.unchained-1)*(w/w.all)
    ly <- shift(x=100*(x.rescaled-1)*(w.rescaled/w.all.rescaled), n=12)
    if(any(abs(ly[t.dec])>0.0001, na.rm=TRUE)){
      warning("Last year term in December deviates from 0. Something might be wrong here.", call.=FALSE)
    }
    res <- ty+ly
  }

  if(method=="kirchner"){
    ty <- 100*shift(x=x.all.rescaled, n=12)*(x.unchained-1)*(w/w.all)
    ly1 <- 100*x.all.unchained*shift(x=(x.rescaled-1)*(w.rescaled/w.all.rescaled), n=12)
    ly2 <- 100*(w/w.all)*(x.unchained-1)*shift(x=(x.rescaled-1)*(w.rescaled/w.all.rescaled), n=12)
    res <- ty+ly1-ly2
  }

  # print output to console:
  return(res)

}

# END
