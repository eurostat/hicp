# START

# Title:  Chain-linking, rebasing and index frequency conversion
# Author: Sebastian Weinand
# Date:   7 May 2025

# unchain monthly time series:
unchain <- function(x, t, by=12, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  if(is.null(settings$na.rm)) settings$na.rm <- FALSE
  if(is.null(settings$freq)) settings$freq <- "auto"
  
  # input checks:
  check.num(x=x)
  check.date(x=t, na.ok=FALSE)
  check.lengths(x=x, y=t)
  check.num(x=by, min.len=1, max.len=1, null.ok=TRUE, int=c(1,12))
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
  m <- pin.month(t, freq=k) # pin months to cut points

  if(is.null(by)){
    
    # overwrite for checking in last step: 
    by <- 12L
    
    # derive reference year:
    y.ref <- y-1
    
    # compute shifted annual average using full calendar years:
    x.ref <- navg(x=x, g=y, n=k, na.rm=settings$na.rm) 
    
    # match index values of reference period to time periods:
    x.ref.shifted <- x.ref[match(x=y.ref, table=as.integer(names(x.ref)))]
    
    
  }else{
    
    # align to pinned months:
    by <- pin.month.int(m=by, freq=k)
    
    # derive reference year:
    y.ref <- y-1+ifelse(m>by,1,0)
    
    # index value in price reference period:
    x.ref <- x[m==by]
    
    # match index values of reference period to time periods:
    x.ref.shifted <- x.ref[match(x=y.ref, table=(y.ref[m==by]+1))]
    
  }
  
  # detect time series breaks for monthly and quarterly frequency:
  if(k>1){ 
    
    # a break in the time series is present if the full calendar year
    # is missing/empty or only one index value is present in December
    # (for by=NULL) or in the month specified in by. in such cases, 
    # the index value is usually set to 100 but must be set to NA 
    # in the nominator (x) to avoid that this "artificial" index value
    # is unchained. however, this index value is still needed in the
    # denominator (x.ref.shifted) for unchaining later periods.
    
    # set index series/nominator meeting this condition to NA:
    tadj <- tapply(X=x, INDEX=y.ref+1, FUN=function(z) length(z[!is.na(z)]))
    tadj <- as.integer(names(which(tadj==1L)))
    x[y%in%tadj & m==by] <- NA_real_
    
  }
  
  # unchain index:
  x.unchained <- as.vector(x/x.ref.shifted)
  
  # return output:
  return(x.unchained)
  
}

# chain monthly time series:
chain <- function(x, t, by=12, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  if(is.null(settings$na.rm)) settings$na.rm <- FALSE
  if(is.null(settings$freq)) settings$freq <- "auto"
  
  # input checks:
  check.num(x=x)
  check.date(x=t, na.ok=FALSE)
  check.lengths(x=x, y=t)
  check.num(x=by, min.len=1, max.len=1, null.ok=TRUE, int=c(1,12))
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
  m <- pin.month(t, freq=k) # pin months to cut points
  
  if(is.null(by)){
    
    # overwrite for checking in last step:
    by <- m
    
    # align years to price reference periods:
    y.ref <- y-1
    
    # compute annual average using full calendar years:
    x.ref <- navg(x=x, g=y, n=k, na.rm=settings$na.rm) 
    
    # match annual averages to years:
    x.ref <- x.ref[match(x=unique(y.ref), table=as.integer(names(x.ref)))]
    
  }else{
    
    # align to pinned months:
    by <- pin.month.int(m=by, freq=k)
    
    # align years to price reference month:
    y.ref <- y-1+ifelse(m>by, 1, 0)
    
    # index value in price reference period:
    x.ref <- x[m==by]
    
    # match price reference indices to years:
    x.ref <- x.ref[match(x=unique(y.ref), table=(y.ref[m==by]+1))]
    
  }
  
  # overwrite names:
  names(x.ref) <- unique(y.ref)
  
  # years that contain only NAs indicate break in time series.
  # cumulative growth rates should start from 1 again after
  # each break. thus, time groups are derived:
  y.ref.grp <- cumsum(tapply(X=x, INDEX=y.ref, FUN=function(z) all(is.na(z))))
  
  # cumulative growth rates per time group:
  x.ref <- x.ref[match(x=names(y.ref.grp), table=names(x.ref))]
  x.ref.cum <- tapply(
    X=ifelse(is.na(x.ref), 1, x.ref), 
    INDEX=y.ref.grp, 
    FUN=function(z){z <- cumprod(z); z <- z/z[1]})
  
  # derive base years where index value is 100:
  y.base <- sapply(X=x.ref.cum[lengths(x.ref.cum)>1], FUN=function(z) names(z)[2])
  
  # unlist and set names:
  x.ref.cum <- unlist(x=x.ref.cum, use.names=FALSE)
  names(x.ref.cum) <- names(y.ref.grp)
  
  # match cumulative growth rates to months:
  x.ref.cum.shifted <- x.ref.cum[match(x=y.ref, table=names(x.ref.cum))]
  
  # chain index series:
  x.chained <- 100*as.vector(x*x.ref.cum.shifted)
  
  # set index start value to 100:
  x.chained[y%in%y.base & m==by & is.na(x.chained)] <- 100
  
  # return output:
  return(x.chained)
  
}

# rebase index series:
rebase <- function(x, t, t.ref="first", settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  if(is.null(settings$na.rm)) settings$na.rm <- FALSE
  if(is.null(settings$freq)) settings$freq <- "auto"
  
  # input checks:
  check.num(x=x)
  check.date(x=t, na.ok=FALSE)
  check.lengths(x=x, y=t)
  check.char(x=t.ref, min.len=1, max.len=Inf, null.ok=FALSE, na.ok=FALSE)
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

  # store for later checking:
  if(length(t.ref)>1 || any(c("first","last")%in%t.ref)){
    print.msg <- TRUE
  }else{
    print.msg <- FALSE
  }
  
  # manipulate dates:
  t <- as.Date(t, format="%Y-%m-%d") # set date format
  y <- data.table::year(t) # get years
  m <-  data.table::month(t) # get months
  ym <- yearmonth(y=y, m=m) # paste to year-month
  # no use of pin.month() necessary; a user would set t.ref based on
  # dates/months present in t, irrespective if t is monthly, quarterly,
  # or annual. if we would use pin.date(), we would have to adjust 
  # t.ref similarly to ensure matching between dates and t.ref
 
  # first and last index periods with non-NA values:
  if(any(!is.na(x))){
    t.ref[t.ref=="first"] <- ym[t==min(t[!is.na(x)])]
    t.ref[t.ref=="last"] <- ym[t==max(t[!is.na(x)])]
  }
  
  # set default value for rebasing:
  x.ref <- NA_real_
  
  # subset to matches:
  t.ref <- t.ref[t.ref%in%y | t.ref%in%ym]
  
  # loop over t.ref:
  for(j in seq_along(t.ref)){
    
    # match index reference period to underlying years and months:
    y.ref <- y==t.ref[j]
    m.ref <- ym==t.ref[j]
    
    # check if any reference year match was found:
    if(any(y.ref)){
      
      # compute the index value of the new base month:
      x.ref <- navg(x=x[y.ref], g=y[y.ref], n=k, na.rm=settings$na.rm)
      
      # be chatty:
      if(settings$chatty & print.msg & !is.na(x.ref)){
        message(paste0("New index reference period is ", t.ref[j]))
      }
      
    }
    
    # check if any base month match was found:
    if(any(m.ref)){
      
      # select the index value of the new base month:
      x.ref <- x[m.ref]
      
      # be chatty:
      if(settings$chatty & print.msg &  !is.na(x.ref)){
        message(paste0("New index reference period is ", t.ref[j]))
      }
      
    }
    
    # break loop as soon as index value is found:
    if(!is.na(x.ref)) break
    
  }
  
  # check if neither base year nor base month was found:
  if((!any(t.ref%in%ym) & !any(t.ref%in%y)) | is.na(x.ref)){
    
    if(settings$chatty){
      if(!any(t.ref%in%ym) & !any(t.ref%in%y)){
        warning("Index reference period(s) 't.ref' not found in 't' -> no rebasement of 'x' performed", call.=FALSE)
      }else{
        warning("Index reference period(s) 't.ref' found in 't' but NA -> no rebasement of 'x' performed", call.=FALSE)
      }
    }
    
    # set new base value to 100 (-> no changes):
    x.ref <- 100
    
  }
  
  # rebase index series:
  out <- as.vector(100*x/x.ref)
  
  # return output:
  return(out)
  
}

# convert monthly into annual or quarterly index:
convert <- function(x, t, type="year", settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  if(is.null(settings$na.rm)) settings$na.rm <- FALSE
  if(is.null(settings$freq)) settings$freq <- "auto"
  
  # input checks:
  check.num(x=x)
  check.date(x=t, na.ok=FALSE)
  check.lengths(x=x, y=t)
  check.char(x=type, min.len=1, max.len=1, miss.ok=TRUE, null.ok=FALSE, na.ok=FALSE)
  check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$na.rm, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$freq, min.len=1, max.len=1, na.ok=FALSE)
  
  # match inputs:
  type <- match.arg(arg=type, choices=c("year","quarter","12mavg"))
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

  if(type=="12mavg"){
    
    if(k>1){
      
      # manipulate dates:
      t <- as.Date(t, format="%Y-%m-%d") # set date format
      y <- data.table::year(t) # get years
      m <-  pin.month(t, freq=k) # pin months to cut points
      ym <- yearmonth(y=y, m=m) # paste to year-month
      # pin months makes even sense for monthly frequency here 
      # because the day is set to 01. if the day would be greater
      # than 28, the february would not be created in the complete 
      # date sequence t0 below
      
      # create full date sequence to ensure chronological order and no gaps:
      t0 <- as.Date(paste(ym, "01", sep="-"), format="%Y-%m-%d") # convert into dates
      mns <- paste((12/k), "months") # define monthly or quarterly interval
      t0 <- seq.Date(from=min(t0), to=max(t0), by=mns) # full date sequence
      ym0 <- yearmonth(y=data.table::year(t0), m=data.table::month(t0)) # year-month for matching
      x <- x[match(x=ym0, table=ym)] # days in dates have no impact

      # compute 12-month rolling mean:
      out <- data.table::frollmean(x=x, n=k, fill=NA, algo="exact", 
                                   align="right", na.rm=settings$na.rm)
      
      # replace NaNs which might occur if na.rm=T and all values are NA:
      out[is.nan(out)] <- NA_real_
      
      # reorder to initial ordering:
      out <- out[match(x=ym, table=ym0)]
      
    }else{
      
      # for annual frequency the rolling mean corresponds
      # to the annual index:
      out <- x
      
    }
    
  }else{
    
    if(type=="year"){
      
      # the conversion into an annual index requires k=12 observations
      # for monthly frequency, k=4 observations for quarterly frequency
      # and k=1 observation for annual frequency
      
      # compute average index of full calendar years:
      g <- pin.date(t=t, freq=1L, format=NULL)
      out <- navg(x=x, g=g, n=k, na.rm=settings$na.rm)
      
    }else{
      
      # the conversion into a quarterly index requires (k/4)=(12/4)=3 
      # observations for monthly frequency and (k/4)=(4/4)=1 observation 
      # for quarterly frequency. no conversion is possible for annual
      # frequency
      
      # compute average index of full quarters:
      g <- pin.date(t=t, freq=4L, format=NULL)
      if(k>1){
        out <- navg(x=x, g=g, n=(k/4), na.rm=settings$na.rm)
      }else{
        out <- stats::setNames(rep(NA_real_, length(g)), g)
      }
      
    }
    
  }
  
  # return output:
  return(out)
  
}

# END
