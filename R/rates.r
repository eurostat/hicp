# START

# Title:  Change rates and contributions
# Author: Sebastian Weinand
# Date:   26 January 2026

# compute change rates:
rates <- function(x, t, type="year", settings=list()){

  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  if(is.null(settings$freq)) settings$freq <- "auto"
  
  # input checks:
  check.num(x=x)
  check.date(x=t, na.ok=FALSE)
  check.lengths(x=x, y=t)
  check.char(x=type, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$freq, min.len=1, max.len=1, na.ok=FALSE)
  
  # match inputs:
  type <- match.arg(arg=type, choices=c("month","quarter","year"))
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
  m <- pin.month(t, freq=k) # pin months to cut points
  ym <- yearmonth(y=y, m=m) # paste to year-month

  # monthly change rate:
  if(type=="month") res <- x/x[match(x=lag.yearmonth(y, m, n=1), table=ym)]
  
  # quarterly change rate:
  if(type=="quarter") res <- x/x[match(x=lag.yearmonth(y, m, n=3), table=ym)]

  # annual change rate:
  if(type=="year") res <- x/x[match(x=lag.yearmonth(y, m, n=12), table=ym)]

  # transform into percentage change:
  res <- as.vector(100*(res-1))

  # print output to console:
  return(res)

}

# compute contributions to change rate:
contrib <- function(x, w, t, x.all, w.all, type="year", settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  if(is.null(settings$freq)) settings$freq <- "auto"
  if(is.null(settings$method)) settings$method <- "ribe"
  
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
  check.char(x=type, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$method, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$freq, min.len=1, max.len=1, na.ok=FALSE)
  
  # match inputs:
  type <- match.arg(arg=type, choices=c("month","quarter","year"))
  method <- match.arg(arg=settings$method, choices=c("ribe","kirchner"))
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
  
  # translate type into integer:
  if(type=="month") type <- 1L
  if(type=="quarter") type <- 3L
  if(type=="year") type <- 12L
  # this must be static, i.e. irrespective of
  # the frequency. similar to rates().
  
  # manipulate dates:
  t <- as.Date(t, format="%Y-%m-%d") # set date format
  y <- data.table::year(t) # get years
  m <- pin.month(t, freq=k) # pin months to cut points
  ym <- yearmonth(y=y, m=m) # paste to year-month
  
  # unchain indices:
  x0 <- hicp::unchain(x=x, t=t, by=12)
  x.all0 <- hicp::unchain(x=x.all, t=t, by=12)
  
  # contributions to change rates within the same year:
  within <- function(x, x.all, w, w.all, y, m, ym, type){
    
    # this function follows Section 8.6.1 of the HICP Manual
    # but ignores the simplification of the formula in January
    # each year
    
    # lag by number of months:
    idx <- match(x=lag.yearmonth(y, m, n=type), table=ym)
    
    # rescale indices and weights within calendar year:
    x.rescaled <- x/x[idx]
    w.rescaled <- w*(x/x.all)[idx]
    w.all.rescaled <- w.all*(x.all/x.all)[idx]
    
    # contributions to change rates:
    res <- (x.rescaled-1)*(w.rescaled/w.all.rescaled)
    
    return(res)
    
  }
  
  # contributions to change rates between years:
  between <- function(x, x.all, w, w.all, y, m, ym, method, type, chatty){
    
    # this function follows Section 8.6.3 of the HICP Manual
    
    # lag by number of months:
    idx <- match(x=lag.yearmonth(y, m, n=type), table=ym)
    
    # rescale indices and weights between calendar years:
    t12 <- m==12L
    x12 <- x[t12]
    x.rescaled <- x12[match(x=y, table=y[t12])]/x
    x.all12 <- x.all[t12]
    x.all.rescaled <- x.all12[match(x=y, table=y[t12])]/x.all
    
    # rescale item weights:
    w.rescaled <- w*x/x.all
    w.all.rescaled <- w.all*x.all/x.all
    
    # ribe decompositon:
    if(method=="ribe"){
      # compute this-year term and last-year term:
      ty <- x.all.rescaled[idx]*(x-1)*(w/w.all)
      ly <- ((x.rescaled-1)*(w.rescaled/w.all.rescaled))[idx]
      if(settings$chatty && any(abs(ly[t12])>0.0001 & m[t12]<type, na.rm=TRUE)){
        warning("Last year term in December deviates from 0. Something might be wrong here.", call.=FALSE)
      }
      res <- ty+ly
    }
    
    # kirchner decomposition:
    if(method=="kirchner"){
      ty <- x.all.rescaled[idx]*(x-1)*(w/w.all)
      ly1 <- x.all*((x.rescaled-1)*(w.rescaled/w.all.rescaled))[idx]
      ly2 <- (w/w.all)*(x-1)*((x.rescaled-1)*(w.rescaled/w.all.rescaled))[idx]
      res <- ty+ly1-ly2
    }
    
    return(res)
    
  }
  
  # compute contributions:
  res <- 100*data.table::fcase(
    m>type, within(x=x0, x.all=x.all0, w=w, w.all=w.all, y=y, m=m, ym=ym, type=type),
    m==type, (x0-1)*(w/w.all), 
    m<type, between(x=x0, x.all=x.all0, w=w, w.all=w.all, y=y, m=m, ym=ym, type=type, method=method, chatty=settings$chatty)
  )
  
  # print output to console:
  return(res)
  
}

# END
