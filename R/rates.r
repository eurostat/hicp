# START

# Title:  Change rates and contributions
# Author: Sebastian Weinand
# Date:   21 October 2026

# change rate between specific period:
rate <- function(x, t, start, end, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  if(is.null(settings$freq)) settings$freq <- "auto"
  
  # input checks:
  check.num(x=x)
  check.date(x=t, na.ok=FALSE)
  check.lengths(x=x, y=t)
  check.date(x=start, max.len=1, na.ok=FALSE)
  check.date(x=end, max.len=1, na.ok=FALSE)
  check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$freq, min.len=1, max.len=1, na.ok=FALSE)
  
  # match inputs:
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
  
  # check that start date lies before end date:
  if(end<start){
    stop("Date in 'start' must lie before the date in 'end'", call.=FALSE)
  }
  
  # manipulate dates:
  t <- as.Date(t, format="%Y-%m-%d") # set date format
  y <- data.table::year(t) # get years
  m <- pin.month(t, freq=k) # pin months to cut points
  
  # years and months belonging to current and base year:
  yb <- data.table::year(start)
  mb <- pin.month(start, freq=k)
  yt <- data.table::year(end)
  mt <- pin.month(end, freq=k)
  
  # change rate:
  res <- as.vector(100*(x[yt==y&mt==m]/x[yb==y&mb==m]-1))
  if(length(res)<1L) res <- NA_real_
  return(res)
  
}

# contribution to change rate over specific period:
contribution <- function(x, x0=NULL, w, xagg, xagg0=NULL, wagg, t, start, end, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$details)) settings$details <- FALSE
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  if(is.null(settings$freq)) settings$freq <- "auto"
  
  # input checks:
  check.num(x=x)
  check.num(x=x0, null.ok=TRUE)
  check.num(x=w)
  check.num(x=xagg)
  check.num(x=xagg0, null.ok=TRUE)
  check.num(x=wagg)
  check.date(x=t, na.ok=FALSE)
  check.date(x=start, max.len=1, na.ok=FALSE)
  check.date(x=end, max.len=1, na.ok=FALSE)
  check.lengths(x=x, y=w)
  check.lengths(x=x, y=t)
  check.lengths(x=x, y=xagg)
  check.lengths(x=x, y=wagg)
  check.log(x=settings$details, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$freq, min.len=1, max.len=1, na.ok=FALSE)
  
  # match inputs:
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
  
  # check that start date lies before end date:
  if(end<start){
    stop("Date in 'start' must lie before the date in 'end'", call.=FALSE)
  }
  
  # manipulate dates:
  t <- as.Date(t, format="%Y-%m-%d") # set date format
  y <- data.table::year(t) # get years
  m <- pin.month(t, freq=k) # pin months to cut points
  
  # match indices of price reference period to current year:
  if(is.null(x0) || is.null(xagg0)){
    t0 <- m==12L
    idx0 <- match(x=y, table=(y+1)[t0])
    if(is.null(x0)) x0 <- x[t0][idx0]
    if(is.null(xagg0)) xagg0 <- xagg[t0][idx0]
  }
  
  # years and months belonging to this year (t), base year (b) and mid years:
  yb <- data.table::year(start)
  mb <- pin.month(start, freq=k)
  yt <- data.table::year(end)
  mt <- pin.month(end, freq=k)
  ymid <- yb:yt
  ymid <- ymid[-c(1,length(ymid))]
  
  # match current period and base period to time:
  idxb <- yb==y&mb==m
  idxt <- yt==y&mt==m
  
  # rescale item weights:
  w.rescaled <- w[idxb]*(x[idxb]/x0[idxb])/(xagg[idxb]/xagg0[idxb])
  wagg.rescaled <- wagg[idxb]
  
  # contribution to change rate spanning two or more calendar year:
  if((yt-yb)>0L){
    
    # base-year term:
    byt <- (w.rescaled/wagg.rescaled) * (x0[(yb+1)==y&mb==m]/x[idxb]-1)
    if(length(byt)<1L) byt <- NA_real_
    byt[w[idxb]==0 | (x0[(yb+1)==y&mb==m]/x[idxb]-1)==0] <- 0
    
    # this-year term:
    tyt <- (w[idxt]/wagg[idxt]) * (xagg0[idxt]/xagg[idxb]) * (x[idxt]/x0[idxt]-1)
    if(length(tyt)<1L) tyt <- NA_real_
    tyt[w[idxt]==0 | (x[idxt]/x0[idxt]-1)==0] <- 0
    
    # # set term to zero:
    # if(!is.na(tyt)&is.na(byt)&(any(idxb)&&(is.na(w[idxb]|w[idxb]<1e-9)))) byt <- 0
    # if(!is.na(byt)&is.na(tyt)&(any(idxt)&&(is.na(w[idxt]|w[idxt]<1e-9)))) tyt <- 0
    
    # contribution to change rate spanning more than two calendar years:
    if((yt-yb)>1L){
      idxm <- match(x=ymid, table=y)
      myt <- (w[idxm]/wagg[idxm]) * (xagg0[idxm]/xagg[idxb]) * (x0[match(x=ymid+1, table=y)]/x0[idxm]-1)
      if(length(myt)<1L){
        myt <- rep(x=NA_real_, times=length(ymid))
      }else{
        #myt <- ifelse(test=!is.na(tyt)&!is.na(byt)&is.na(myt)&is.na(w[idxm]), yes=0, no=myt)
        myt[w[idxm]==0 | (x0[match(x=ymid+1, table=y)]/x0[idxm]-1)==0] <- 0
      }
    }else{
      myt <- numeric()
    }
    
    # combine terms:
    res <- c(byt, myt, tyt)
    res <- c(res, sum(res, na.rm=FALSE))
    nms <- c(yb, ymid, yt, "total")
    
  }else{
    
    # contribution to change rate within a calendar year:
    res <- (w.rescaled/wagg.rescaled)*(x[idxt]/x[idxb]-1)
    if(length(res)<1L) res <- NA_real_
    res[w[idxb]==0 | (x[idxt]/x[idxb]-1)==0] <- 0
    nms <- "total"
    
  }
  
  # create named list:
  res <- stats::setNames(as.numeric(100*res), nms)
  
  if(!any(idxb)){
    if(settings$chatty){
      warning("Start date not found in 't' -> no calculation of total contribution", call.=FALSE)
    }
    res[["total"]] <- NA_real_
  }
  
  if(!any(idxt)){
    if(settings$chatty){
      warning("End date not found in 't' -> no calculation of total contribution", call.=FALSE)
    }
    res[["total"]] <- NA_real_
  }
  
  # return output to console:
  if(!settings$details) res <- res[["total"]]
  return(res)
  
}

# monthly, quarterly or annual change rates:
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

# contributions to monthly, quarterly or annual change rates:
contributions <- function(x, x0=NULL, w, xagg, xagg0=NULL, wagg, t, type="year", settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  if(is.null(settings$freq)) settings$freq <- "auto"
  
  # input checks:
  check.num(x=x)
  check.num(x=x0, null.ok=TRUE)
  check.num(x=w)
  check.num(x=xagg)
  check.num(x=xagg0, null.ok=TRUE)
  check.num(x=wagg)
  check.date(x=t, na.ok=FALSE)
  check.lengths(x=x, y=w)
  check.lengths(x=x, y=t)
  check.lengths(x=x, y=xagg)
  check.lengths(x=x, y=wagg)
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
  
  # match indices of price reference period to current year:
  if(is.null(x0) || is.null(xagg0)){
    t0 <- m==12L
    idx0 <- match(x=y, table=(y+1)[t0])
    if(is.null(x0)) x0 <- x[t0][idx0]
    if(is.null(xagg0)) xagg0 <- xagg[t0][idx0]
  }
  
  # contributions to change rates within the same year:
  within <- function(x, x0, w, xagg, xagg0, wagg, y, m, ym, type){
    
    # this function follows Section 8.6.1 of the HICP Manual
    # but ignores the simplification of the formula in January
    # each year
    
    # lag by number of months:
    idx <- match(x=lag.yearmonth(y, m, n=type), table=ym)
    
    # rescale item weights:
    w.rescaled <- w*(x/x0)/(xagg/xagg0)
    wagg.rescaled <- wagg*(xagg/xagg0)/(xagg/xagg0)
    
    # contributions to change rates:
    res <- (x/x[idx]-1)*(w.rescaled/wagg.rescaled)[idx]
    res[w==0 | (x/x[idx]-1)==0] <- 0
    # if the price change or the weight is zero,
    # the contribution is also zero
    res <- as.numeric(res)
    return(res)
    
  }
  
  # contributions to change rates between years:
  between <- function(x, x0, w, xagg, xagg0, wagg, y, m, ym, type){
    
    # this function follows Section 8.6.3 of the HICP Manual
    
    # lag by number of months:
    idx <- match(x=lag.yearmonth(y, m, n=type), table=ym)
    
    # rescale item weights:
    w.rescaled <- w*(x/x0)/(xagg/xagg0)
    wagg.rescaled <- wagg*(xagg/xagg0)/(xagg/xagg0)
    
    # compute this-year and last-year terms:
    ty <- (w/wagg)*(xagg0/xagg[idx])*(x/x0-1)
    ly <- (w.rescaled/wagg.rescaled)[idx]*(x0/x[idx]-1)
    # print no warning if last-year term deviates from zero in price reference 
    # period as this is ok for quarterly or annual averages
    # # res <- ifelse(test=is.na(ty)&(is.na(w)|w<1e-9), yes=0, no=ty)+ifelse(test=is.na(ly)&(is.na(w[idx])|w[idx]<1e-9), yes=0, no=ly)
    # # res[is.na(ly) & is.na(ty)] <- NA_real_
    res <- ifelse(test=w==0 | (x/x0-1)==0, yes=0, no=ty) + ifelse(test=w[idx]==0 | (x0/x[idx]-1)==0, yes=0, no=ly)
    # if the weight in the respective year is zero, the whole term is zero, 
    # irrespective of the price change being NA or not. this might be the case
    # for disappearing and newly introduced products:
    # - the this-year term should be treated as zero for disappearing products
    #   whose total contribution in the first year they disappeared is just 
    #   defined by the last-year term. 
    # - the last-year term should be treated as zero only for newly appearing 
    #   products whose total contribution in the first year of appearance
    #   is defined by the this-year term.
    # similarly, if the price change is zero, the whole term is zero, even
    # if the weight is NA or greater than zero
    res <- as.numeric(res)
    return(res)
    
  }
  
  # compute contributions:
  res <- 100*as.numeric(data.table::fcase(
    m>type, within(x=x, x0=x0, w=w, xagg=xagg, xagg0=xagg0, wagg=wagg, y=y, m=m, ym=ym, type=type),
    m<=type, between(x=x, x0=x0, w=w, xagg=xagg, xagg0=xagg0, wagg=wagg, y=y, m=m, ym=ym, type=type)
  ))
  
  # print output to console:
  return(res)
  
}

# END
