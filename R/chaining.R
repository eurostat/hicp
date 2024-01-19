# START

# Title:  Chain-linking, rebasing and index frequency conversion
# Author: Sebastian Weinand
# Date:   19 January 2024

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

    # compute shifted average:
    x.ref.shifted <- tapply(X=x, INDEX=y+1, FUN=mean, na.rm=TRUE)

    # use only full calendar years:
    x.ref.shifted[tapply(X=x, INDEX=y+1, FUN=length)<12] <- NA

    # match index values of reference period to other index values:
    x.ref.shifted <- x.ref.shifted[match(x=y, table=names(x.ref.shifted))]

  }else{

    # derive reference time period:
    t.ref <- paste(y-1+ifelse(m>by,1,0), by, sep="-")

    # match index values of reference period to other index values:
    x.ref.shifted <- (x[m==by])[match(x=t.ref, table=paste(y[m==by], by, sep="-"))]

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

    # subset to relevant months:
    x.ref <- tapply(X=x, INDEX=y, FUN=mean, na.rm=TRUE)

    # make sure that only full calendar years are considered:
    x.ref[tapply(X=x, INDEX=y, FUN=length)<12] <- NA

    # years that contain only NAs indicate break in time series.
    # cumulative growth rates should start from 1 again after
    # each break. thus, time groups are derived:
    y.ref.grp <- cumsum(tapply(X=x, INDEX=y+1, FUN=function(z) all(is.na(z))))

    # cumulative growth rates per time group:
    x.ref.cum <- unlist(
      x=tapply(x.ref, y.ref.grp, FUN=function(z) cumprod(ifelse(is.na(z), 1, z))),
      use.names=FALSE)
    #x.ref.cum <- cumprod(ifelse(is.na(x.ref), 1, x.ref))

    # set names:
    names(x.ref.cum) <- names(y.ref.grp)

    # replicate cumulative growth rates:
    x.ref.cum.shifted <- x.ref.cum[match(x=y, table=names(x.ref.cum))]

    # chain_annual series:
    x.chained <- 100*as.vector(x*x.ref.cum.shifted)

    # set index start value to 100:
    y.base <- which(diff(tapply(X=x.chained, INDEX=y-1, FUN=function(z) all(is.na(z))))<0)
    x.chained[y%in%names(y.base)] <- 100

  }else{

    # frequency of price reference years:
    y.ref <- y-1+ifelse(m>by, 1, 0)
    y.ref.tab <- table(y.ref)

    # subset to relevant months:
    x.ref <- x[m==by]

    # set names:
    names(x.ref) <- format(t[m==by], "%Y")

    # match with years:
    x.ref <- x.ref[match(names(y.ref.tab), names(x.ref))]

    # set names:
    names(x.ref) <- names(y.ref.tab)

    # years that contain only NAs indicate break in time series.
    # cumulative growth rates should start from 1 again after
    # each break. thus, time groups are derived:
    y.ref.grp <- cumsum(tapply(x, y.ref, FUN=function(z) all(is.na(z))))

    # cumulative growth rates per time group:
    x.ref.cum <- unlist(
      x=tapply(x.ref, y.ref.grp, FUN=function(z) cumprod(ifelse(is.na(z), 1, z))),
      use.names=FALSE)
    #x.ref.cum <- cumprod(ifelse(is.na(x.ref), 1, x.ref))

    # replicate cumulative growth rates:
    x.ref.cum.shifted <- rep(x=x.ref.cum, y.ref.tab)

    # chain series:
    x.chained <- 100*as.vector(x*x.ref.cum.shifted)

    # set index start value to 100:
    x.chained[c(diff(is.na(x.chained)),0)<0] <- 100

  }

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
  out <- data.table("time"=as.character(names(out)), "index"=as.numeric(out))

  # return output:
  return(out)

}

# END
