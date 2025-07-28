# START

# Title:  Linking-in new index series
# Author: Sebastian Weinand
# Date:   24 February 2025

# link-in new index series:
link <- function(x, x.new, t, t.overlap=NULL, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  if(is.null(settings$na.rm)) settings$na.rm <- FALSE
  if(is.null(settings$freq)) settings$freq <- "auto"
  
  # input checks:
  check.num(x=x)
  check.num(x=x.new)
  check.date(x=t, na.ok=FALSE)
  check.lengths(x=x, y=t)
  check.lengths(x=x, y=x.new)
  check.char(x=t.overlap, null.ok=TRUE, na.ok=FALSE)
  check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$na.rm, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$freq, min.len=1, max.len=1, na.ok=FALSE)
  
  # match input:
  freq <- match.arg(arg=settings$freq, choices=c("auto","month","quarter","year"))
  
  # frequency/number of periods per year:
  if(freq=="auto"){
    k <- nperiods(t)
  }else{
    if(freq=="month") k <- 12L
    if(freq=="quarter") k <- 4L
    if(freq=="year") k <- 1L
  }
  
  # check chronological ordering, duplicates and gaps:
  check.dateseries(x=t, freq=k, chatty=settings$chatty)
  
  # manipulate dates:
  t <- as.Date(t, format="%Y-%m-%d") # set date format
  y <- data.table::year(t) # get years
  m <- data.table::month(t) # get months
  ym <- yearmonth(y=y, m=m) # paste to year-month
  # no use of pin.month() necessary; a user would set t.overlap 
  # based on the dates/months present in t, irrespective if t is
  # monthly, quarterly, or annual. if we would use pin.month(), 
  # we would have to adjust t.overlap similarly to ensure matching
  # between t and t.overlap
  
  # check:
  if(is.null(t.overlap)){
    check <- TRUE
  }else{
    check <- FALSE
  }

  # last value of current index defining the period
  # when linking is carried out:
  t.last <- max(t[!is.na(x)])
  
  # compute annual averages for current and new index:
  xavg <- navg(x=x, g=y, n=k, na.rm=settings$na.rm)
  xavg.new <- navg(x=x.new, g=y, n=k, na.rm=settings$na.rm)
  # keep in mind that the the averages xavg and xavg.new can be
  # based on different sets if settings$na.rm=TRUE, e.g., if there
  # are some NAs in x but not in x.new
  
  # compute linking factors:
  lf <- c(
    xavg/xavg.new, # annual overlap
    stats::setNames(x/x.new, ym) # one-month or one-quarter overlap
  )
  
  # drop NAs:
  lf <- lf[!is.na(lf)]
  
  # set overlap period if not provided:
  if(is.null(t.overlap) & length(lf)>0){
    t.overlap <- names(lf)
  }
  
  # check if overlap periods are available:
  keep <- t.overlap%in%names(lf)
  
  # print messages:
  if(settings$chatty){
    if(all(!keep)){
      message("No overlap period(s) found.")
    }
    if(!all(!keep) && any(!keep)){
      message("Some overlap period(s) in 't.overlap' not found.")
    }
  } 
  
  # match linking factors to overlap periods:
  lf <- lf[match(x=t.overlap, table=names(lf))]
  
  # output container:
  out <- matrix(data=x, byrow=FALSE, 
                ncol=max(1, length(t.overlap)), nrow=length(t), 
                dimnames=list(NULL, t.overlap))
  
  # link old and new index series using all overlap periods:
  for(j in seq_along(t.overlap)[keep]){
    out[,j] <- c(x[t<=t.last], (x.new*lf[j])[t>t.last])
  }
  
  # match to initial ordering if dates not chronological:
  out[, keep] <- out[match(x=ym, table=c(ym[t<=t.last], ym[t>t.last])), keep, drop=FALSE]
  
  # coerce to vector or keep matrix:
  if(ncol(out)<=1L){
    if(settings$chatty && !all(!keep) && check){
      message("Overlap period ", t.overlap, " used.")
    }
    res <- as.vector(out[,1])
  }else{
    res <- out
  }
  
  # return output:
  return(res)
  
}

# compute level-shift factor:
lsf <- function(x, x.new, t, t.overlap=NULL, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  if(is.null(settings$na.rm)) settings$na.rm <- FALSE
  if(is.null(settings$freq)) settings$freq <- "auto"
  
  # input checks:
  check.num(x=x)
  check.num(x=x.new)
  check.date(x=t, na.ok=FALSE)
  check.lengths(x=x, y=t)
  check.lengths(x=x, y=x.new)
  check.char(x=t.overlap, null.ok=TRUE, na.ok=FALSE)
  check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$na.rm, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$freq, min.len=1, max.len=1, na.ok=FALSE)
  
  # match input:
  freq <- match.arg(arg=settings$freq, choices=c("auto","month","quarter","year"))
  
  # frequency/number of periods per year:
  if(freq=="auto"){
    k <- nperiods(t)
  }else{
    if(freq=="month") k <- 12L
    if(freq=="quarter") k <- 4L
    if(freq=="year") k <- 1L
  }
  
  # check chronological ordering, duplicates and gaps:
  check.dateseries(x=t, freq=k, chatty=settings$chatty)
  
  # manipulate dates:
  t <- as.Date(t, format="%Y-%m-%d") # set date format
  y <- data.table::year(t) # get years
  m <- data.table::month(t) # get months
  ym <- yearmonth(y=y, m=m) # paste to year-month

  # index value available in both index series that would be
  # used by the one-month or one-quarter overlap method. if the
  # frequency is not monthly, pin.month() ensures that the month
  # in the last quarter is set to december and, thus, matched:
  m12 <- pin.month(t, freq=k)
  t12 <- !is.na(x) & !is.na(x.new) & m12==12L
  if(any(t12, na.rm=TRUE)) t12 <- max(y[t12], na.rm=TRUE)
  idx <- y==t12 & m12==12L
  
  # compute annual averages for current and new index:
  xavg <- navg(x=x, g=y, n=k, na.rm=settings$na.rm)
  xavg.new <- navg(x=x.new, g=y, n=k, na.rm=settings$na.rm)
  # keep in mind that the the averages xavg and xavg.new can be
  # based on different sets if settings$na.rm=TRUE, e.g., if there
  # are some NAs in x but not in x.new
  
  # compute level-shift factors:
  lf <- c(
    (x.new[idx]/xavg.new) / (x[idx]/xavg), # annual overlap
    (x.new[idx]/x.new) / (x[idx]/x) # one-month or one-quarter overlap
  )
  
  # set names:
  if(length(lf)>0) names(lf) <- c(names(xavg), ym)
  
  # drop NAs:
  lf <- lf[!is.na(lf)]
  
  # set overlap period if not provided:
  if(is.null(t.overlap) & length(lf)>0){
    t.overlap <- names(lf)
  }
  
  # subset to requested overlap periods:
  if(length(t.overlap)>0){
    out <- lf[match(x=t.overlap, table=names(lf))]
  }else{
    out <- NA_real_
  }
  names(out) <- t.overlap
  
  # return output to console:
  return(out)
  
}

# END
