# START

# Title:  Chain-linking, rebasing and index frequency conversion
# Author: Sebastian Weinand
# Date:   2 June 2024

# unchain monthly time series:
unchain <- function(x, t, by=12){
  
  # input checks:
  check.num(x=x)
  check.date(x=t, na.ok=FALSE)
  check.lengths(x=x, y=t)
  check.num(x=by, min.len=1, max.len=1, null.ok=TRUE, int=c(1,12))
  
  # extract year and month:
  t <- as.Date(x=t, format="%Y-%m-%d")
  y <- as.integer(format(t, "%Y"))
  m <- as.integer(format(t, "%m"))
  
  if(is.null(by)){
    
    # compute shifted annual average using full calendar years:
    x.ref <- tapply(X=x, INDEX=y+1, FUN=function(z) if(all(is.na(z)) | length(z)<12L){NA_real_}else{mean(z, na.rm=TRUE)})
    
    # match index values of reference period to time periods:
    x.ref.shifted <- x.ref[match(x=y, table=names(x.ref))]
    
  }else{
    
    # index value in price reference period:
    x.ref <- x[m==by]
    
    # derive reference time period:
    y.ref <- paste(y-1+ifelse(m>by,1,0), by, sep="-")
    
    # match index values of reference period to time periods:
    x.ref.shifted <- x.ref[match(x=y.ref, table=paste(y[m==by], by, sep="-"))]
    
  }
  
  # unchain index:
  x.unchained <- as.vector(x/x.ref.shifted)
  
  # return output:
  return(x.unchained)
  
}

# chain monthly time series:
chain <- function(x, t, by=12){
  
  # input checks:
  check.num(x=x)
  check.date(x=t, na.ok=FALSE)
  check.lengths(x=x, y=t)
  check.num(x=by, min.len=1, max.len=1, null.ok=TRUE, int=c(1,12))
  
  # extract year and month:
  t <- as.Date(x=t, format="%Y-%m-%d")
  y <- as.integer(format(t, "%Y"))
  m <- as.integer(format(t, "%m"))
  
  if(is.null(by)){
    
    # align years to previous price reference periods:
    y.ref <- y-1
    
    # compute annual average using full calendar years:
    x.ref <- tapply(X=x, INDEX=y, FUN=function(z) if(all(is.na(z)) | length(z)<12L){NA_real_}else{mean(z, na.rm=TRUE)})
    
    # match annual averages to years:
    x.ref <- x.ref[match(x=unique(y.ref), table=names(x.ref))]
    
    # overwrite for checking in last step:
    by <- m
    
  }else{
    
    # align years to price reference month:
    y.ref <- y-1+ifelse(m>by, 1, 0)
    
    # match price reference indices to years:
    x.ref <- x[match(x=paste(unique(y.ref), by, sep="-"), table=paste(y,m,sep="-"))]
    
  }
  
  # overwrite names:
  names(x.ref) <- unique(y.ref)
  
  # years that contain only NAs indicate break in time series.
  # cumulative growth rates should start from 1 again after
  # each break. thus, time groups are derived:
  y.ref.grp <- cumsum(tapply(X=x, INDEX=y.ref, FUN=function(z) all(is.na(z))))
  
  # cumulative growth rates per time group:
  x.ref.cum <- tapply(x.ref, y.ref.grp, FUN=function(z) cumprod(ifelse(is.na(z), 1, z)))
  
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
  x.chained[y%in%y.base & m==by] <- 100
  
  # print unchained values:
  return(x.chained)
  
}

# rebase index series:
rebase <- function(x, t, t.ref, verbose=FALSE){

  # input checks:
  check.num(x=x)
  check.date(x=t, na.ok=FALSE)
  check.lengths(x=x, y=t)
  check.char(x=t.ref, min.len=1, max.len=1, miss.ok=TRUE, null.ok=TRUE, na.ok=FALSE)
  check.log(x=verbose, min.len=1, max.len=1, na.ok=FALSE)

  # set in case no input provided:
  if(missing(t.ref)) t.ref <- "no_rebasement"

  # match index reference period to underlying years and months:
  t <- as.Date(x=t, format="%Y-%m-%d")
  y.ref <- format(t, "%Y") == t.ref
  m.ref <- format(t, "%Y-%m") == t.ref
  x.ref <- NA_real_

  # check if any reference year match was found:
  if(any(y.ref)){

    # select the index value of the new reference year:
    if(sum(y.ref)==12L) x.ref <- mean(x=x[y.ref], na.rm=FALSE)

    # be chatty:
    if(verbose & !is.na(x.ref)){
      message(paste0("New index reference period is ", t.ref, ". Rebased with index value of ", round(x=x.ref, digits=3), "."))
    }

  }

  # check if any base month match was found:
  if(any(m.ref)){

    # select the index value of the new base month:
    x.ref <- x[m.ref]

    # be chatty:
    if(verbose & !is.na(x.ref)){
      message(paste0("New index reference period is ", t.ref, ". Rebased with index value of ", round(x=x.ref, digits=3), "."))
    }

  }

  # check if neither base year nor base month was found:
  if((!any(y.ref) & !any(m.ref)) | is.na(x.ref)){

    if(verbose){
      if(t.ref == "no_rebasement"){
        message(paste0("No index reference period provided -> no rebasement of 'x' performed."))
      }else{
        if(!any(y.ref) & !any(m.ref)){
          warning(paste0("Index reference period '", t.ref, "' not found in 't' -> no rebasement of 'x' performed."))
        }else{
          if(any(y.ref) && sum(y.ref)!=12L){
            warning(paste0("Index reference period '", t.ref, "' does not include 12 months -> no rebasement of 'x' performed."))
          }else{
            warning(paste0("Index reference period '", t.ref, "' is NA -> no rebasement of 'x' performed."))
          }
        }
      }
    }

    # set new base value to 100 (-> no changes):
    x.ref <- 100

  }

  # rebase index series:
  out <- 100*x/x.ref

  # return output:
  return(out)

}

# convert monthly into annual or quarterly index:
convert <- function(x, t, freq="annual"){

  # input checks:
  check.num(x=x)
  check.date(x=t, na.ok=FALSE)
  check.lengths(x=x, y=t)
  check.char(x=freq, min.len=1, max.len=1, miss.ok=TRUE, null.ok=FALSE, na.ok=FALSE)

  # match input:
  freq <- match.arg(arg=freq, choices=c("annual","quarterly"))

  # set grouping variable and number of observations required per year:
  by <- format(t, "%Y")
  n <- 12L
  if(freq=="quarterly"){
    by <- paste(by, quarters(t), sep="-")
    n <- 3L
  }

  # compute average index if all months or quarters are available:
  out <- tapply(X=x, INDEX=by, FUN=mean, na.rm=FALSE)
  out[tapply(x, by, length)<n] <- NA_real_
  out <- stats::setNames(as.vector(out), names(out))

  # return output:
  return(out)

}

# END
