# START

# Title:    Index number methods and aggregation
# Author:   Sebastian Weinand
# Date:     26 January 2026

# bilateral index functions:
jevons <- function(x){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  
  # compute index:
  if(all(is.na(x))){
    out <- NA_real_  # avoid NaN in case no complete observations are present
  }else{
    out <- exp(mean(log(x), na.rm=TRUE))
  }
  
  # return output:
  return(out)
  
}
carli <- function(x){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  
  # compute index:
  if(all(is.na(x))){
    out <- NA_real_ # avoid NaN in case no complete observations are present
  }else{
    out <- mean(x, na.rm=TRUE)
  }
  
  # return output:
  return(out)
  
}
harmonic <- function(x){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  
  # compute index:
  if(all(is.na(x))){
    out <- NA_real_ # avoid NaN in case no complete observations are present
  }else{
    out <- 1/mean(1/x, na.rm=TRUE)
  }
  
  # return output:
  return(out)
  
}
laspeyres <- function(x, w0){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  
  # complete cases:
  idx <- stats::complete.cases(x, w0)
  
  # compute index:
  if(any(idx)){
    
    # subset to complete cases:
    x <- x[idx]
    w0 <- w0[idx]
    # if index is NA, no computation is possible.
    # dropping specific weights implies re-normalizing
    # the remaining weights.
    
    # renormalize weights:
    w0 <- w0/sum(w0, na.rm=TRUE)
    
    # compute index:
    out <- sum(x*w0, na.rm=TRUE)
    # see Ilo et al. (2004), p. 265, formula (15.8)
    
  }else{
    
    # set to NA as otherwise sum(NA, na.rm=TRUE)
    # will return 0:
    out <- NA_real_
    
  }
  
  # return output:
  return(out)
  
}
paasche <- function(x, wt){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=wt, int=c(0,Inf))
  check.lengths(x=x, y=wt)
  
  # complete cases:
  idx <- stats::complete.cases(x, wt)
  
  # compute index:
  if(any(idx)){
    
    # subset to complete cases:
    x <- x[idx]
    wt <- wt[idx]
    # if index is NA, no computation is possible.
    # dropping specific weights implies re-normalizing
    # the remaining weights.

    # renormalize weights:
    wt <- wt/sum(wt, na.rm=TRUE)
    
    # compute index:
    out <- 1/sum((x^(-1))*wt, na.rm=TRUE)
    # see Ilo et al. (2004), p. 266, formula (15.9)
    
  }else{
    
    # set to NA as otherwise sum(NA, na.rm=TRUE)
    # will return 0:
    out <- NA_real_
    
  }
  
  # return output:
  return(out)
  
}
fisher <- function(x, w0, wt){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.num(x=wt, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=wt)
  
  # complete cases:
  idx <- stats::complete.cases(x, w0, wt)
  
  # compute index:
  if(any(idx)){
    
    # subset to complete cases:
    x <- x[idx]
    w0 <- w0[idx]
    wt <- wt[idx]
    
    # renormalize weights:
    w0 <- w0/sum(w0, na.rm=TRUE)
    wt <- wt/sum(wt, na.rm=TRUE)
    
    # compute index:
    out <- sqrt(laspeyres(x=x, w0=w0)*paasche(x=x, wt=wt))
    # see Ilo et al. (2004), p. 267, formula (15.12)
    
  }else{
    
    # set to NA as otherwise sum(NA, na.rm=TRUE)
    # will return 0:
    out <- NA_real_
    
  }
  
  # return output:
  return(out)
  
}
toernqvist <- function(x, w0, wt){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.num(x=wt, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=wt)
  
  # complete cases:
  idx <- stats::complete.cases(x, w0, wt)
  
  # compute index:
  if(any(idx)){
    
    # subset to complete cases:
    x <- x[idx]
    w0 <- w0[idx]
    wt <- wt[idx]
    
    # renormalize weights:
    w0 <- w0/sum(w0, na.rm=TRUE)
    wt <- wt/sum(wt, na.rm=TRUE)
    
    # compute index:
    out <- prod(x^((w0+wt)/2), na.rm=TRUE)
    # see Ilo et al. (2004), p. 283, formula (15.81)
    
  }else{
    
    # set to NA as otherwise sum(NA, na.rm=TRUE)
    # will return 0:
    out <- NA_real_
    
  }
  
  # return output:
  return(out)
  
}
walsh <- function(x, w0, wt){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.num(x=wt, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=wt)
  
  # complete cases:
  idx <- stats::complete.cases(x, w0, wt)
  
  # compute index:
  if(any(idx)){
    
    # subset to complete cases:
    x <- x[idx]
    w0 <- w0[idx]
    wt <- wt[idx]
    
    # renormalize weights:
    w0 <- w0/sum(w0, na.rm=TRUE)
    wt <- wt/sum(wt, na.rm=TRUE)
    
    # compute index:
    out <- sum(sqrt(x)*sqrt(w0*wt), na.rm=TRUE)/sum(sqrt(1/x)*sqrt(w0*wt), na.rm=TRUE)
    # see Ilo et al. (2004), p. 269, formula (15.21)
    
  }else{
    
    # set to NA as otherwise sum(NA, na.rm=TRUE)
    # will return 0:
    out <- NA_real_
    
  }
  
  # return output:
  return(out)
  
}

# non-exported helper function to match index formulas to weights:
matchformula <- function(f, x, w0, wt){
  
  fargs <- names(formals(f))
  
  if(all(c("w0","wt")%in%fargs)){
    out <- f(x=x, w0=w0, wt=wt)
  }else{
    if(all(!c("w0","wt")%in%fargs)){
      out <- f(x=x)
    }
    if("w0"%in%fargs){
      out <- f(x=x, w0=w0)
    }
    if("wt"%in%fargs){
      out <- f(x=x, wt=wt)
    }
  }
  
  return(out)
  
}

# compute one or multiple user-defined aggregates:
aggregate <- function(x, w0, wt, id, formula=laspeyres, agg=list(), settings=list()){
  
  # set defaults:
  if(is.null(settings$exact)) settings$exact <- TRUE
  if(is.null(settings$names)) settings$names <- NULL
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  
  # input checks:
  check.num(x=x, min.len=0, int=c(0,Inf))
  check.num(x=w0, min.len=0, int=c(0,Inf), miss.ok=TRUE)
  check.num(x=wt, min.len=0, int=c(0,Inf), miss.ok=TRUE)
  check.char(x=id, min.len=0)
  check.lengths(x=x, y=id)
  check.list(x=agg, min.len=0, miss.ok=TRUE, null.ok=TRUE, names=FALSE)
  check.log(x=settings$exact, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$names, min.len=1, null.ok=TRUE, na.ok=FALSE)
  check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  check.lengths(x=agg, y=settings$names)
  
  # input checks on weights:
  if(missing(w0)){w0miss <- TRUE; w0 <- rep(0, length(x))}else{w0miss <- FALSE}
  if(missing(wt)){wtmiss <- TRUE; wt <- rep(0, length(x))}else{wtmiss <- FALSE}
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=wt)
  
  # input checks on formula:
  # we have to substitute first; otherwise, if x=laspeyres and 
  # formula=laspeyres, formula would be overwritten by x
  formula0 <- substitute(formula) 
  formula <- eval(formula0)
  if(is.function(formula)){
    formula <- stats::setNames(list(match.fun(formula)), deparse(formula0))
  }
  check.list(x=formula, miss.ok=FALSE, null.ok=FALSE, names=TRUE)
  fnw0 <- sapply(X=formula, FUN=function(z) !is.null(formals(z)$w0) & w0miss)
  fnwt <- sapply(X=formula, FUN=function(z) !is.null(formals(z)$wt) & wtmiss)
  fnw <- fnw0 | fnwt
  if(any(fnw, na.rm=TRUE)){
    fnok <- paste(names(which(fnw)), collapse=",")
    if(sum(fnw)>1L) fnok <- paste0("(", fnok, ")")
    stop(paste0("Non-valid input for w0 or wt -> weights required for 'formula=", fnok, "'"), call.=FALSE)
  }
  
  # check for duplicated aggregate names:
  if(sum(duplicated(settings$names))>0L && settings$chatty){
    warning("Duplicated aggregate names supplied -> names adjusted by 'make.unique(settings$names)'", call.=FALSE)
  }
  
  # set aggregate names:
  aggnm <- character()
  if(length(agg)>0L){
    if(length(settings$names)>0L){
      aggnm <- make.unique(names=settings$names)
    }else{
      aggnm <- as.character(seq_along(agg))
    }
  }
  
  # set output column names:
  cnm <- c(names(formula),"w0","wt")
  
  # subset to complete cases but store initial ids for checking:
  id0 <- id
  nonas <- stats::complete.cases(x, w0, wt, id)
  x <- x[nonas]
  w0 <- w0[nonas]
  wt <- wt[nonas]
  id <- id[nonas]
  
  # print user warning:
  if(settings$chatty && all(!nonas)){
    warning("No complete cases available for calculation", call.=FALSE)
  }
  
  # compute user-defined aggregates:
  noagg <- !logical(length=length(agg))
  out <- matrix(data=NA_real_, nrow=length(agg), ncol=length(cnm), dimnames=list(NULL, cnm))
  for(j in seq_along(agg)){
    
    # check if all ids are available:
    aggids.avail <- agg[[j]]%in%id0
    
    # subset data to relevant ids:
    idx1 <- id%in%agg[[j]]
    # duplicated elements in 'agg' without impact on aggregation
    
    # calculations only if:
    if((all(aggids.avail) || !settings$exact) && any(idx1)){
      
      # adjust logical:
      noagg[j] <- FALSE
      
      # subset indices and weights:
      x1 <- x[idx1]
      w01 <- w0[idx1]
      wt1 <- wt[idx1]
      
      # aggregate indices and sum weights:
      out[j, ] <- as.vector(c(
        mapply(FUN=matchformula, 
               f=formula, 
               MoreArgs=list(x=x1, w0=w01, wt=wt1), 
               SIMPLIFY=TRUE),
        "w0"=sum(w01, na.rm=TRUE),
        "wt"=sum(wt1, na.rm=TRUE)))
      
    }
    
  }
  
  # print message if there were any user-defined groups that were no aggregated:
  if(settings$chatty && any(noagg) && any(nonas)){
    msg <- "Not all elements in 'agg' were found in 'id' -> no calculation for 'agg=("
    message(
      paste(msg, paste(aggnm[noagg], collapse=","), ")'", sep="")
    )
  }
  
  # add aggregate ids and set column order:
  out <- data.table::data.table("id"=aggnm, out)
  data.table::setcolorder(x=out, c("id","w0","wt"))
  # do not set a key; otherwise, the output is resorted
  
  # drop weights if these were not provided by the user:
  if(w0miss) out$w0 <- NULL
  if(wtmiss) out$wt <- NULL
  
  # return output to console:
  return(out)
  
}

# compute one or multiple user-defined aggregates:
disaggregate <- function(x, w0, id, agg=list(), settings=list()){
  
  # set defaults:
  if(is.null(settings$exact)) settings$exact <- TRUE
  if(is.null(settings$names)) settings$names <- NULL
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  
  # input checks:
  check.num(x=x, min.len=0, int=c(0,Inf))
  check.num(x=w0, min.len=0, int=c(0,Inf))
  check.char(x=id, min.len=0)
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=id)
  check.list(x=agg, min.len=0, miss.ok=TRUE, null.ok=TRUE)
  check.log(x=settings$exact, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$names, min.len=1, null.ok=TRUE, na.ok=FALSE)
  check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  check.lengths(x=agg, y=settings$names)
  
  # check for duplicated aggregate names:
  if(sum(duplicated(settings$names))>0L && settings$chatty){
    warning("Duplicated aggregate names supplied -> names adjusted by 'make.unique(settings$names)'", call.=FALSE)
  }
  
  # set aggregate names:
  aggnm <- character()
  if(length(agg)>0L){
    if(length(settings$names)>0L){
      aggnm <- make.unique(names=settings$names)
    }else{
      aggnm <- as.character(seq_along(agg))
    }
  }
  
  # set output column names:
  cnm <- c("w0","laspeyres")
  
  # subset to complete cases but store initial ids for checking:
  id0 <- id
  nonas <- stats::complete.cases(x, w0, id)
  x <- x[nonas]
  w0 <- w0[nonas]
  id <- id[nonas]
  
  # print user warning:
  if(settings$chatty & all(!nonas)){
    warning("No complete cases available for calculation", call.=FALSE)
  }
  
  # compute user-defined aggregates:
  out <- matrix(data=NA_real_, nrow=length(agg), ncol=length(cnm), dimnames=list(NULL, cnm))
  noagg <- !logical(length=length(agg))
  for(j in seq_along(agg)){
    
    # check if all ids are available:
    aggids.avail <- agg[[j]]%in%id0
    
    # subset data to relevant ids:
    idx1 <- id%in%agg[[j]]
    idx2 <- id%in%names(agg)[[j]]
    # duplicated elements in 'agg' without impact on aggregation
    
    # calculations only if:
    if((all(aggids.avail) || !settings$exact) && any(idx1) && any(idx2)){
      
      # adjust logical:
      noagg[j] <- FALSE
      
      # subset indices and weights:
      x1 <- x[idx1]
      w01 <- w0[idx1]
      x2 <- x[idx2]
      w02 <- w0[idx2]
      
      # deduct indices and sum weights:
      out[j, ] <- c(w02-sum(w01), (w02*x2-sum(w01*x1))/(w02-sum(w01)))
      
    }
    
  }
  
  # print message if there were any user-defined groups that were no aggregated:
  if(settings$chatty && any(noagg) && any(nonas)){
    msg <- "Not all elements in 'agg' were found in 'id' -> no calculation for 'agg=("
    message(
      paste(msg, paste(aggnm[noagg], collapse=","), ")'", sep="")
    )
  }
  
  # set index and weights to NA if one is negative:
  out[rowSums(out<0L)>0L, ] <- NA_real_
  out[is.nan(out)] <- NA_real_
  # this can happen if aggregates are not properly
  # defined, e.g., the total is deducted from a subcomponent
  
  # add aggregate ids and set column order:
  out <- data.table::data.table("id"=aggnm, out)
  data.table::setcolorder(x=out, c("id","w0","laspeyres"))
  # do not set a key; otherwise, the output is resorted
  
  # return output to console:
  return(out)
  
}

# step-wise index aggregation following the COICOP tree:
aggregate.tree <- function(x, w0, wt, id, formula=laspeyres, settings=list()){
  
  # set defaults:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$coicop.prefix)) settings$coicop.prefix <- getOption("hicp.coicop.prefix")
  if(is.null(settings$all.items.code)) settings$all.items.code <- getOption("hicp.all.items.code")
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  
  # input checks:
  check.num(x=x, min.len=0, int=c(0,Inf))
  check.num(x=w0, min.len=0, int=c(0,Inf), miss.ok=TRUE)
  check.num(x=wt, min.len=0, int=c(0,Inf), miss.ok=TRUE)
  check.char(x=id, min.len=0)
  check.lengths(x=x, y=id)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.prefix, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$all.items.code, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  
  # input checks on weights:
  if(missing(w0)){w0miss <- TRUE; w0 <- rep(0, length(x))}else{w0miss <- FALSE}
  if(missing(wt)){wtmiss <- TRUE; wt <- rep(0, length(x))}else{wtmiss <- FALSE}
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=wt)
  
  # input checks on formula:
  # we have to substitute first; otherwise, if x=laspeyres and 
  # formula=laspeyres, formula would be overwritten by x
  formula0 <- substitute(formula) 
  formula <- eval(formula0)
  if(is.function(formula)){
    formula <- stats::setNames(list(match.fun(formula)), deparse(formula0))
  }
  check.list(x=formula, miss.ok=FALSE, null.ok=FALSE, names=TRUE)
  fnw0 <- sapply(X=formula, FUN=function(z) !is.null(formals(z)$w0) & w0miss)
  fnwt <- sapply(X=formula, FUN=function(z) !is.null(formals(z)$wt) & wtmiss)
  fnw <- fnw0 | fnwt
  if(any(fnw, na.rm=TRUE)){
    fnok <- paste(names(which(fnw)), collapse=",")
    if(sum(fnw)>1L) fnok <- paste0("(", fnok, ")")
    stop(paste0("Non-valid input for w0 or wt -> weights required for 'formula=", fnok, "'"), call.=FALSE)
  }
  
  # set all-items code:
  allid <- settings$all.items.code
  
  # gather data:
  dt <- data.table::data.table(id, w0, wt, x)
  dt <- dt[hicp::is.coicop(id=id, settings=settings) | hicp::is.bundle(id=id, settings=settings),] # keep only valid coicop codes and bundle codes
  if(!all(is.na(dt$x))) dt <- dt[!is.na(x), ] # drop these NAs because they would bias aggregation
  dt[, names(formula) := x] # copy values for each formula
  dt[, "x":=NULL] # drop initial values
  data.table::setkey(x=dt, key="id") # do not set the key before
  
  # stop if aggregation not possible:
  if(nrow(dt)<1L){
    
    if(settings$chatty){
      warning("No complete cases available for calculation", call.=FALSE)
    }
    
    # keep empty data.table with all relevant columns:
    out <- cbind(dt[0,], "is_aggregated"=logical())
    
  }else{
    
    # derive aggregation structure:
    p <- parent(id=dt$id, usedict=TRUE, closest=FALSE, k=1:9, settings=settings)
    dtagg <- data.table("id"=rep(x=dt$id, lengths(p)), "parent"=unlist(x=p, use.names=FALSE))
    dtagg[, "id":=c(parent[-1], id[1]), by="id"]
    dtagg <- rbindlist(l=list(data.table("id"=allid, "parent"=NA_character_), dtagg))
    dtagg <- unique(dtagg)
    dtagg <- dtagg[keep.bundle(id=id, settings=settings),]
    dtagg[, "lvl":=hicp::level(id=id, settings=settings)]
    dtagg <- merge(x=dtagg, y=dt, by="id", all.x=TRUE, sort=FALSE)
    dtagg[, "is_aggregated":=FALSE]
    
    # compute aggregates from bottom to top levels in aggregation structure:
    lvl <- NULL # avoid no visible binding note when checking
    lvls <- max(dtagg$lvl):(min(dtagg$lvl)+1L)
    for(i in lvls){
      
      # aggregate prices and sum weights for each parent:
      res <- dtagg[lvl==i, 
                   c("is_aggregated"=TRUE,
                     mapply(FUN=matchformula, f=formula, x=.SD, MoreArgs=list(w0=w0, wt=wt), SIMPLIFY=FALSE),
                     "w0"=if(all(is.na(w0))){NA_real_}else{sum(w0, na.rm=TRUE)},
                     "wt"=if(all(is.na(wt))){NA_real_}else{sum(wt, na.rm=TRUE)}
                     ),
                   by=list("id"=parent),
                   .SDcols=names(formula),
                   env=list(i=i)]
      
      # merge and overwrite existing values:
      dtagg[res, names(res):=res, on="id"]
      
    }
    
    # prepare results, set column order and key:
    out <- subset(x=dtagg, select=-c(parent,lvl))
    
  }
  
  # set column order and key of output:
  data.table::setcolorder(x=out, c("id","is_aggregated","w0","wt"))
  data.table::setkeyv(x=out, cols="id")

  # drop weights if these were not provided by the user:
  if(w0miss) out$w0 <- NULL
  if(wtmiss) out$wt <- NULL
  
  # return output to console:
  return(out)
  
}

# END
