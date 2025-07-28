# START

# Title:  Global helper functions (non exported)
# Author: Sebastian Weinand
# Date:   22 July 2025

# set coicop dictionary:
set.coicop <- function(x){
  
  # match arguments:
  ver <- match.arg(arg=x, choices=c("ecoicop","ecoicop.hicp","ecoicop2","coicop1999","coicop2018"))
  
  # select the coicop version:
  switch(EXPR = x, 
         "ecoicop"=coicop[["ecoicop"]], 
         "ecoicop.hicp"=coicop[["ecoicop.hicp"]],
         "ecoicop2"=coicop[["ecoicop2"]],
         "coicop1999"=coicop[["coicop1999"]],
         "coicop2018"=coicop[["coicop2018"]])
  
}

# keep bundle code or its components:
keep.bundle <- function(id, by=NULL, settings=list()){
  
  # @description:
  # if a coicop bundle code is present in id but its 
  # underlying valid coicop codes are not all present 
  # in id, the bundle codes are flagged with a TRUE and
  # the valid codes with a FALSE, meaning that the 
  # valid codes could be dropped from id. in all other
  # cases, the bundle codes would be flagged with FALSE
  # and the valid codes with a TRUE.
  
  # @args:
  # id        character, codes
  # by        factor, grouping variable 
  # settings  see is.bundle() and unbundle()
  
  # overwrite:
  settings$simplify <- FALSE
  
  # define logical:
  res <- rep(x=TRUE, times=length(id))
  
  # flag bundles:
  bdl.flag <- hicp::is.bundle(id=id, settings=settings)
  
  # if any bundles present:
  if(any(bdl.flag, na.rm=TRUE)){
    
    # get intersecting codes:
    if(is.null(by)){
      idtab <- unique(id)
    }else{
      idtab <- Reduce(f=intersect, x=split(id, by))
    } 
    
    # unique bundle codes:
    bdls <- unique(id[bdl.flag])
    
    # check if bundle codes are present in intersecting ids:
    bdls.idx <- bdls%in%idtab
    
    # unbundle bundle codes:
    bdls.clean <- hicp::unbundle(id=bdls, settings=settings)
    
    # loop over bundles:
    for(j in seq_along(bdls)){
      
      if(!all(bdls.clean[[j]]%in%idtab) & bdls.idx[j]){
        res[id%in%bdls.clean[[j]]] <- FALSE
      }else{
        res[id==bdls[j]] <- FALSE
      }
      
    }
    
  }
  
  # return output to console:
  return(res)
  
}

# number of periods per year defining the frequency of t:
nperiods <- function(t, tol=1e-6){
  
  # @args:
  # t     date vector
  # tol   the tolerance to differentiate between frequencies
  
  # start from monthly as we assume to be working with monthly frequency:
  res <- 12L
  
  # check for quarterly and yearly frequency of time series:
  if(length(t)>1L){
    
    # more than 2 observations:
    if(length(t)>2L){
      
      # get quarterly and yearly difference:
      qdiff <- diff(sort(data.table::yearqtr(t)))
      ydiff <- diff(sort(data.table::year(t)))
      if(all(qdiff>=(1/4-tol)) & !all(ydiff>=(1-tol))) res <- 4L
      if(all(ydiff>=(1-tol))) res <- 1L
      
    }else{
      
      # this difference is needed for c("2020-12-01","2021-01-01"),
      # for example, which would be always yearly using the code above
      
      # get monthly difference:
      mdiff <- diff(sort(data.table::yearmon(t)))
      if(all(mdiff>=(1/4-tol)) & !all(mdiff>=(1-tol))) res <- 4L
      if(all(mdiff>=(1-tol))) res <- 1L
      
    }
    
  }
  
  # return output:
  return(res)
  
}

# anchor months to monthly, quarterly or yearly cut points:
pin.month <- function(t, freq=12L){
  
  # @args:
  # t       date vector
  # freq    frequency of date vector [12=month, 4=quarter, 1=year]
  
  # get months:
  if(freq==12L) out <- data.table::month(t) 
  
  # set months to last month of quarter:
  if(freq==4L) out <- data.table::quarter(t)*3 
  
  # set month to last month of year:
  if(freq==1L) out <- rep(12L, length(t)) 
  
  # coerce into integer:
  out <- as.integer(out)
  
  return(out)
  
}

# same as pin.month() but for integer input:
pin.month.int <- function(m, freq=12L){
  
  # @args:
  # m       integer for months [1:12]
  # freq    frequency of date vector [12=month, 4=quarter, 1=year]
  
  if(freq==12L) out <- m
  if(freq==4L) out <- rep(c(3,6,9,12), each=3)[match(x=m, table=1:12)]
  if(freq==1L) out <- rep(12, length(m))
  out <- as.integer(out)
  
  return(out)
  
}

# using pin.month() to bring into date format:
pin.date <- function(t, freq=12L, format="%Y-%m-%d"){
  
  # similar to cut(as.Date(YYYY-MM-DD), "quarter")
  
  # @args:
  # t       date vector
  # freq    frequency of date vector [12=month, 4=quarter, 1=year]
  # format  see as.Date()
  
  # extract year, pin month, and set day:
  y <- data.table::year(t)
  m <- pin.month(t, freq=freq)
  d <- "01" # set day to 01
  
  # coerce year, month and day into character:
  out <- paste(y, formatC(m, width=2, flag="0"), d, sep="-")
  
  # coerce into date:
  if(!is.null(format)){
    out <- as.Date(x=out, format=format) # defining the format is faster
  }
  
  # return output:
  return(out)
  
}

# paste year and month into format YYYY-MM:
yearmonth <- function(y, m){
  
  # equivalent to format(x, "%Y-%m") but faster even if 
  # y and m have to be derived first using data.table::year()
  # and data.table::month()
  
  # @args:
  # y, m   year and month as integer
  
  # paste into character format YYYY-MM:
  out <- paste(y, formatC(m, width=2, flag="0"), sep="-")
  
  # return output:
  return(out)
  
}

# lag year and month:
lag.yearmonth <- function(y, m, n=0){
  
  # @args:
  # y, m   year and month as integer
  # n      lag t by n months, integer>0
  
  # lag if asked for:
  if(n>0){
    
    # remaining months of year:
    my <- n%%12
    
    # adjust year and month:
    y <- y-ifelse(test=m>my, yes=n%/%12, no=1+n%/%12)
    m <- ifelse(test=m>my, m-my, m-my+12)
    
  }
  
  # paste into character format YYYY-MM:
  out <- yearmonth(y=y, m=m)
  
  # return output:
  return(out)
  
}

# compute average(s) based on minimum number of observations:
navg <- function(x, g=NULL, n=12L, na.rm=FALSE){
  
  # @args:
  # x       numeric vector
  # g       vector of quarters or years as grouping variable
  # n       integer specifying the minimum number of observations,
  #         e.g., 12 observations for monthly frequency
  # na.rm   logical, drop NAs in computation or not
  
  f <- function(x, n, na.rm){
    
    if(na.rm){
      if(all(is.na(x))){NA_real_}else{mean(x, na.rm=TRUE)}
    }else{
      if(all(is.na(x)) | length(x)<n){NA_real_}else{mean(x, na.rm=FALSE)}
    }
    
  }
  
  if(is.null(g)){
    res <- f(x=x, n=n, na.rm=na.rm)
  }else{
    res <- tapply(X=x, INDEX=g, FUN=f, n=n, na.rm=na.rm)
    res <- stats::setNames(as.vector(res), names(res))
  }
  
  return(res)
  
}


### functions currently not used in the package:
#
# # anchor YYYY-MM to monthly, quarterly, or yearly cut points:
# pin.yearmonth <- function(x, freq=12L){
#   
#   # similar to pin.date but for formats YYYY-MM
#   
#   # @args:
#   # x       character vector
#   # freq    frequency of date vector [12=month, 4=quarter, 1=year]
#   
#   # adjustments to frequency only needed for quarters or years 
#   if(freq<12L){
#     
#     # check for valid YYYY-MM pattern
#     idx <- grepl(pattern="^[0-9]{4}\\-(0[1-9]|10|11|12)$", x=x)
#     
#     if(any(idx, na.rm=TRUE)){
#       
#       ym <- data.table::tstrsplit(x[idx], split="-", fixed=TRUE)
#       y <- ym[[1]]
#       m <- as.integer(ym[[2]])
#       
#       # if quarter, set month to last month of quarter:
#       if(freq==4) m <- findInterval(x=m, vec=c(1,4,7,10))*3
#       
#       # if year, set month to last month of year:
#       if(freq==1) m <- "12"
#       
#       # coerce year, month and day into date:
#       x[idx] <- paste(y, formatC(m, width=2, flag="0"), sep="-")
#       
#     }
#     
#   }
#   
#   # return output:
#   return(x)
#   
# }

# END