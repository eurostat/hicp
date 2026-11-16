# START

# Title:    Index number methods and aggregation
# Author:   Sebastian Weinand
# Date:     16 November 2026

# bilateral index functions:
jevons <- function(x){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  
  # compute index:
  out <- exp(stats::weighted.mean(x=log(x), na.rm=FALSE)) 
  out[is.nan(out)] <- NA_real_
  return(out)
  
}
carli <- function(x){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  
  # compute index:
  out <- stats::weighted.mean(x=x, na.rm=FALSE)
  out[is.nan(out)] <- NA_real_
  return(out)
  
}
harmonic <- function(x){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  
  # compute index:
  out <- 1/stats::weighted.mean(x=1/x, na.rm=FALSE)
  out[is.nan(out)] <- NA_real_
  return(out)
  
}
laspeyres <- function(x, w0){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  
  # see Ilo et al. (2004), p. 265, formula (15.8):
  w0 <- w0/sum(w0)
  out <- stats::weighted.mean(x=x, w=w0, na.rm=FALSE)
  out[is.nan(out)] <- NA_real_
  return(out)
  
}
paasche <- function(x, wt){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=wt, int=c(0,Inf))
  check.lengths(x=x, y=wt)
  
  # see Ilo et al. (2004), p. 266, formula (15.9):
  wt <- wt/sum(wt)
  out <- 1/stats::weighted.mean(x=1/x, w=wt, na.rm=FALSE)
  out[is.nan(out)] <- NA_real_
  return(out)
  
}
fisher <- function(x, w0, wt){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.num(x=wt, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=wt)
  
  # see Ilo et al. (2004), p. 267, formula (15.12):
  w0 <- w0/sum(w0)
  wt <- wt/sum(wt)
  out <- sqrt(stats::weighted.mean(x, w0, na.rm=FALSE) * 1/stats::weighted.mean(1/x, wt, na.rm=FALSE))
  out[is.nan(out)] <- NA_real_
  return(out)
  
}
toernqvist <- function(x, w0, wt){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.num(x=wt, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=wt)
  
  # see Ilo et al. (2004), p. 283, formula (15.81)
  w0 <- w0/sum(w0)
  wt <- wt/sum(wt)
  out <- exp(stats::weighted.mean(x=log(x), w=(w0+wt)/2, na.rm=FALSE)) 
  out[is.nan(out)] <- NA_real_
  return(out)
  
}
walsh <- function(x, w0, wt){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.num(x=wt, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=wt)
  
  # see Ilo et al. (2004), p. 269, formula (15.21):
  w0 <- w0/sum(w0)
  wt <- wt/sum(wt)
  out <- stats::weighted.mean(x=sqrt(x), w=sqrt(w0*wt), na.rm=FALSE) / stats::weighted.mean(x=sqrt(1/x), w=sqrt(w0*wt), na.rm=FALSE)
  out[is.nan(out)] <- NA_real_
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
  if(is.null(settings$names)) settings$names <- NULL
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  
  # input checks:
  check.num(x=x, min.len=0, int=c(0,Inf))
  check.num(x=w0, min.len=0, int=c(0,Inf), miss.ok=TRUE)
  check.num(x=wt, min.len=0, int=c(0,Inf), miss.ok=TRUE)
  check.char(x=id, min.len=0)
  check.lengths(x=x, y=id)
  check.list(x=agg, min.len=0, miss.ok=TRUE, null.ok=TRUE, names=FALSE)
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
  fn <- eval(formula0)
  if(is.function(formula)){
    fn <- stats::setNames(list(match.fun(fn)), deparse(formula0))
  }
  check.list(x=fn, miss.ok=FALSE, null.ok=FALSE, names=TRUE)
  
  # check if all elements are functions:
  fnl <- !sapply(X=fn, FUN=is.function)
  if(any(fnl, na.rm=TRUE)){
    fnok <- paste(names(which(fnl)), collapse=",")
    if(sum(fnl)>1L) fnok <- paste0("(", fnok, ")")
    stop(paste0("Non-valid input for formula -> formula=", fnok, " must be a function"), call.=FALSE)
  }
  
  # check if each function contains argument x:
  fnx <- sapply(X=fn, FUN=function(z) !"x"%in%names(formals(z)))
  if(any(fnx, na.rm=TRUE)){
    fnok <- paste(names(which(fnx)), collapse=",")
    if(sum(fnx)>1L) fnok <- paste0("(", fnok, ")")
    stop(paste0("Non-valid input for formula -> argument 'x' required for formula=", fnok), call.=FALSE)
  }
  
  # check if weights are provided for functions requiring weights:
  fnw0 <- sapply(X=fn, FUN=function(z) "w0"%in%names(formals(z)))
  fnwt <- sapply(X=fn, FUN=function(z) "wt"%in%names(formals(z)))
  fnw <- (fnw0&w0miss) | (fnwt&wtmiss)
  if(any(fnw, na.rm=TRUE)){
    fnok <- paste(names(which(fnw)), collapse=",")
    if(sum(fnw)>1L) fnok <- paste0("(", fnok, ")")
    stop(paste0("Non-valid input for w0 or wt -> weights required for formula=", fnok), call.=FALSE)
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
  cnm <- c(names(fn),"w0","wt")
  
  # compute user-defined aggregates:
  noagg <- !logical(length=length(agg))
  out <- matrix(data=NA_real_, nrow=length(agg), ncol=length(cnm), dimnames=list(NULL, cnm))
  for(j in seq_along(agg)){
    
    # check if all ids are available:
    aggids.avail <- agg[[j]]%in%id
    
    # subset data to relevant ids:
    idx1 <- id%in%agg[[j]]
    # duplicated elements in 'agg' without impact on aggregation
    
    # calculations only if:
    if(all(aggids.avail) && any(idx1)){
      
      # adjust logical:
      noagg[j] <- FALSE
      
      # subset indices and weights:
      x1 <- x[idx1]
      w01 <- w0[idx1]
      wt1 <- wt[idx1]
      
      # aggregate indices and sum weights:
      out[j, ] <-  as.vector(c(
        mapply(FUN=matchformula, 
               f=fn, 
               MoreArgs=list(x=x1, w0=w01, wt=wt1), 
               SIMPLIFY=TRUE),
        "w0"=sum(w01),
        "wt"=sum(wt1)))
      
    }
    
  }
  
  # print message if any user-defined aggregates were not calculated:
  if(settings$chatty && any(noagg)){
    msg <- "Not all elements in 'agg' present in 'id' -> no calculation for 'agg=("
    warning(paste(msg, paste(aggnm[noagg], collapse=","), ")'", sep=""), call.=FALSE)
  }
  
  # define output:
  dtagg <- data.table::data.table("id"=aggnm, out)
  data.table::setcolorder(x=dtagg, c("id","w0","wt"))
  # do not set a key; otherwise, the output is resorted
  
  # drop weights if not provided by user:
  if(w0miss) dtagg$w0 <- NULL 
  if(wtmiss) dtagg$wt <- NULL
  
  return(dtagg)
  
}

# compute one or multiple user-defined aggregates:
disaggregate <- function(x, w0, id, agg=list(), settings=list()){
  
  # set defaults:
  if(is.null(settings$names)) settings$names <- NULL
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  
  # input checks:
  check.num(x=x, min.len=0, int=c(0,Inf))
  check.num(x=w0, min.len=0, int=c(0,Inf))
  check.char(x=id, min.len=0)
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=id)
  check.list(x=agg, min.len=0, miss.ok=TRUE, null.ok=TRUE)
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
  
  # compute user-defined aggregates:
  out <- matrix(data=NA_real_, nrow=length(agg), ncol=length(cnm), dimnames=list(NULL, cnm))
  noagg <- !logical(length=length(agg))
  for(j in seq_along(agg)){
    
    # check if all ids are available:
    aggids.avail <- agg[[j]]%in%id
    
    # subset data to relevant ids:
    idx1 <- id%in%agg[[j]]
    idx2 <- id%in%names(agg)[[j]]
    # duplicated elements in 'agg' without impact on aggregation
    
    # calculations only if:
    if(all(aggids.avail) && any(idx1) && any(idx2)){
      
      # adjust logical:
      noagg[j] <- FALSE
      
      # subset indices and weights:
      x1 <- x[idx1]
      w01 <- w0[idx1]
      x2 <- x[idx2]
      w02 <- w0[idx2]
      
      # deduct indices and sum weights:
      out[j, ] <- c(w02-sum(w01), (w02*x2-sum((w01*x1)[w01!=0]))/(w02-sum(w01)))
      
    }
    
  }
  
  # print message if any user-defined aggregates were not calculated:
  if(settings$chatty && any(noagg)){
    msg <- "Not all elements in 'agg' present in 'id' -> no calculation for 'agg=("
    warning(paste(msg, paste(aggnm[noagg], collapse=","), ")'", sep=""), call.=FALSE)
  }
  
  # set index and weights to NA if one is negative:
  out[rowSums(out<0L, na.rm=TRUE)>0L, ] <- NA_real_
  out[is.nan(out)] <- NA_real_
  # this can happen if aggregates are not properly
  # defined, e.g., the total is deducted from a subcomponent
  
  # add aggregate ids and set column order:
  dtagg <- data.table::data.table("id"=aggnm, out)
  data.table::setcolorder(x=dtagg, c("id","w0","laspeyres"))
  # do not set a key; otherwise, the output is resorted
  
  return(dtagg)
  
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
  fn <- eval(formula0)
  if(is.function(fn)){
    fn <- stats::setNames(list(match.fun(formula)), deparse(formula0))
  }
  check.list(x=fn, miss.ok=FALSE, null.ok=FALSE, names=TRUE)
  
  # check if all elements are functions:
  fnl <- !sapply(X=fn, FUN=is.function)
  if(any(fnl, na.rm=TRUE)){
    fnok <- paste(names(which(fnl)), collapse=",")
    if(sum(fnl)>1L) fnok <- paste0("(", fnok, ")")
    stop(paste0("Non-valid input for formula -> formula=", fnok, " must be a function"), call.=FALSE)
  }
  
  # check if each function contains argument x:
  fnx <- sapply(X=fn, FUN=function(z) !"x"%in%names(formals(z)))
  if(any(fnx, na.rm=TRUE)){
    fnok <- paste(names(which(fnx)), collapse=",")
    if(sum(fnx)>1L) fnok <- paste0("(", fnok, ")")
    stop(paste0("Non-valid input for formula -> argument 'x' required for formula=", fnok), call.=FALSE)
  }
  
  # check if weights are provided for functions requiring weights:
  fnw0 <- sapply(X=fn, FUN=function(z) "w0"%in%names(formals(z)))
  fnwt <- sapply(X=fn, FUN=function(z) "wt"%in%names(formals(z)))
  fnw <- (fnw0&w0miss) | (fnwt&wtmiss)
  if(any(fnw, na.rm=TRUE)){
    fnok <- paste(names(which(fnw)), collapse=",")
    if(sum(fnw)>1L) fnok <- paste0("(", fnok, ")")
    stop(paste0("Non-valid input for w0 or wt -> weights required for formula=", fnok), call.=FALSE)
  }

  # set all-items code:
  allid <- settings$all.items.code
  
  # keep only valid coicop codes, bundle codes and all-items code:
  dt <- data.table::data.table(id, w0, wt, x)
  dt <- dt[is.coicop(id=id, settings=settings) | is.bundle(id=id, settings=settings) | allid==id,]
  dt[, names(fn) := x] # copy values for each formula
  dt[, "x":=NULL] # drop initial values
  data.table::setkey(x=dt, key="id") # do not set the key before
  
  # stop if aggregation not possible:
  if(nrow(dt)<1L){
    
    if(settings$chatty){
      warning("No valid COICOP codes found for aggregation", call.=FALSE)
    }
    
    # keep empty data.table with all relevant columns:
    dtagg <- cbind("is_aggregated"=logical(), dt[0,])
    
  }else{
    
    # derive aggregation structure:
    p <- parent(id=dt$id, usedict=TRUE, closest=FALSE, k=1:9, settings=settings)
    dtagg <- data.table(
      "id"=c(allid, rep(x=dt$id, lengths(p))),
      "parent"=c(NA_character_, unlist(x=p, use.names=FALSE)))
    dtagg[, "id":=c(parent[-1], id[1]), by="id"]
    dtagg <- unique(dtagg)
    dtagg <- dtagg[keep.bundle(id=id, settings=settings),]
    dtagg[, "lvl":=level(id=id, settings=settings)]
    dtagg <- merge(x=dtagg, y=dt, by="id", all.x=TRUE, sort=FALSE)
    dtagg[, "is_aggregated":=FALSE]
    
    # compute aggregates from bottom to top levels in aggregation structure:
    lvl <- NULL # avoid no visible binding note when checking
    lvls <- max(dtagg$lvl):(min(dtagg$lvl)+1L)
    for(i in lvls){
      
      # aggregate prices and sum weights for each parent:
      res <- dtagg[lvl==i,
                   c("is_aggregated"=TRUE,
                     mapply(FUN=matchformula, f=fn, x=.SD, MoreArgs=list(w0=w0, wt=wt), SIMPLIFY=FALSE),
                     "w0"=if(all(is.na(w0))){NA_real_}else{sum(w0)},
                     "wt"=if(all(is.na(wt))){NA_real_}else{sum(wt)}
                   ),
                   by=list("id"=parent),
                   .SDcols=names(fn),
                   env=list(i=i)]
      
      # overwrite existing values at higher-level with those calculated
      # if these are no NA:
      res <- res[!is.na(w0),]
      dtagg[res, names(res):=res, on="id"]
      
    }
    
    # drop columns:
    dtagg$parent <- dtagg$lvl <- NULL
    
  }
  
  # set column order and key of output:
  data.table::setcolorder(x=dtagg, c("id","is_aggregated","w0","wt"))
  data.table::setkeyv(x=dtagg, cols="id")
  if(w0miss) dtagg$w0 <- NULL # drop weights if not provided by user
  if(wtmiss) dtagg$wt <- NULL
  return(dtagg)
  
}

# END
