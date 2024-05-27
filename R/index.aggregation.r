# START

# Title:  Index number methods and aggregation
# Author: Sebastian Weinand
# Date:   27 May 2024

# bilateral index functions:
jevons <- function(x, w0=NULL, wt=NULL){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  
  if(all(is.na(x))){
    P <- NA_real_
  }else{
    P <- exp(mean(log(x), na.rm=TRUE))
  }
  
}
carli <- function(x, w0=NULL, wt=NULL){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  
  if(all(is.na(x))){
    P <- NA_real_
  }else{
    P <- mean(x, na.rm=TRUE)
  }
  
}
harmonic <- function(x, w0=NULL, wt=NULL){
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  
  if(all(is.na(x))){
    P <- NA_real_
  }else{
    P <- 1/mean(1/x, na.rm=TRUE)
  }
  
}
laspeyres <- function(x, w0, wt=NULL){

  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.lengths(x=x, y=w0)

  if(missing(w0) || all(is.na(w0) | is.na(x))){

    # set to NA:
    P <- NA_real_

  }else{

    # set weights to NA if index is NA:
    w0_laspey <- ifelse(is.na(x), NA, w0)
    # if index is NA, no computation is possible.
    # however, dropping specific weights implies
    # re-normalizing the remaining weights.

    # renormalize weights:
    w0_laspey <- w0_laspey/sum(w0_laspey, na.rm=TRUE)

    # compute index:
    P <- sum(x*w0_laspey, na.rm=TRUE)
    # see Ilo et al. (2004), p. 265, formula (15.8)

  }

  return(P)

}
paasche <- function(x, w0=NULL, wt){

  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=wt, int=c(0,Inf))
  check.lengths(x=x, y=wt)

  if(missing(wt) || all(is.na(wt) | is.na(x))){

    # set to NA:
    P <- NA_real_

  }else{

    # set weights to NA if index is NA:
    wt_paasche <- ifelse(is.na(x), NA, wt)
    # if index is NA, no computation is possible.
    # however, dropping specific weights implies
    # re-normalizing the remaining weights.

    # renormalize weights:
    wt_paasche <- wt_paasche/sum(wt_paasche, na.rm=TRUE)

    # compute index:
    P <- 1/sum((x^(-1))*wt_paasche, na.rm=TRUE)
    # see Ilo et al. (2004), p. 266, formula (15.9)

  }

  return(P)

}
fisher <- function(x, w0, wt){

  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.num(x=wt, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=wt)

  if(missing(w0) || missing(wt) || all(is.na(w0) | is.na(wt) | is.na(x))){

    # set to NA:
    P <- NA_real_

  }else{

    # compute index:
    P <- sqrt(laspeyres(x=x, w0=w0)*paasche(x=x, wt=wt))
    # see Ilo et al. (2004), p. 267, formula (15.12)

  }

  return(P)

}
toernqvist <- function(x, w0, wt){

  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.num(x=wt, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=wt)

  if(missing(w0) || missing(wt) || all(is.na(w0) | is.na(wt) | is.na(x))){

    # set to NA:
    P <- NA_real_

  }else{

    # set weights to NA if index value is NA:
    w0_sup <- ifelse(is.na(x), NA, w0)
    wt_sup <- ifelse(is.na(x), NA, wt)
    # for Paasche and Layspeyres, we set weight to NA
    # if index value is NA. Fisher index is simply the
    # geometric average of Paasche and Laspeyres. For
    # Toernqvist and Walsh we adopt this logic.

    # renormalize weights:
    w0_sup <- w0_sup/sum(w0_sup, na.rm = TRUE)
    wt_sup <- wt_sup/sum(wt_sup, na.rm = TRUE)

    # compute index:
    P <- prod(x^((w0_sup+wt_sup)/2), na.rm = TRUE)
    # see Ilo et al. (2004), p. 283, formula (15.81)

  }

  return(P)

}
walsh <- function(x, w0, wt){

  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf))
  check.num(x=wt, int=c(0,Inf))
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=wt)

  if(missing(w0) || missing(wt) || all(is.na(w0) | is.na(wt) | is.na(x))){

    # set to NA:
    P <- NA_real_

  }else{

    # set weights to NA if index value is NA:
    w0_sup <- ifelse(is.na(x), NA, w0)
    wt_sup <- ifelse(is.na(x), NA, wt)
    # for Paasche and Layspeyres, we set weight to NA
    # if index value is NA. Fisher index is simply the
    # geometric average of Paasche and Laspeyres. For
    # Toernqvist and Walsh we adopt this logic.

    # renormalize weights:
    w0_sup <- w0_sup/sum(w0_sup, na.rm = TRUE)
    wt_sup <- wt_sup/sum(wt_sup, na.rm = TRUE)

    # compute index:
    P <- sum(sqrt(x)*sqrt(w0_sup*wt_sup), na.rm = TRUE)/sum(sqrt(1/x)*sqrt(w0_sup*wt_sup), na.rm = TRUE)
    # see Ilo et al. (2004), p. 269, formula (15.21)

  }

  return(P)

}

# index aggregation:
aggregate <- function(x, w0, wt, grp, index=laspeyres, add=list(), settings=list()){
  
  # @Args:
  # x             unchained index series, i.e. unchain(x, t)
  # w0            weights used in the current period
  # wt            weights from a period t+1
  # grp           the grouping variable, should be coicop codes
  #               defined by [0-9]{1,5}
  # index         a function or named list of functions specifiyng
  #               the index type, e.g. laspey() for the laspeyres index.
  #               each function must have arguments x, w0 and wt, even if
  #               the arguments w0 and wt are not used
  # add           named list of ids that can be found in grp, which are
  #               aggregated
  # keep.lowest   keep the lowest level indices that form
  #               the base of all aggregation steps
  # add.exact     aggregate ids in add only if all are present or using 
  #               the available ones
  
  # set defaults:
  if(is.null(settings$keep.lowest)) settings$keep.lowest <- TRUE
  if(is.null(settings$add.exact)) settings$add.exact <- TRUE
  
  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf), miss.ok=TRUE)
  check.num(x=wt, int=c(0,Inf), miss.ok=TRUE)
  check.char(x=grp)
  check.lengths(x=x, y=grp)
  check.log(x=settings$keep.lowest, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$add.exact, min.len=1, max.len=1, na.ok=FALSE)
  if(!is.list(add)) stop("Non-valid input for add -> only lists allowed")
  
  # input checks on both "w0" and "wt:
  if(missing(w0)){w0.miss <- TRUE; w0 <- rep(NA_real_, length(x))}else{w0.miss <- FALSE}
  if(missing(wt)){wt.miss <- TRUE; wt <- rep(NA_real_, length(x))}else{wt.miss <- FALSE}
  check.lengths(x=x, y=w0)
  check.lengths(x=x, y=wt)
  
  # input checks on "index":
  if(is.function(index)){index <- stats::setNames(list(index), deparse(substitute(index)))}
  if(!is.list(index)){stop("Non-valid input for index -> must be a function or named list of functions")}
  if(any(names(index)=="")){stop("Non-valid input for index -> all list elements must have names")}
  if(!all(sapply(X=index, FUN=function(z) all(c("x","w0","wt")%in%names(formals(z)))))){
    stop("Non-valid input for index -> all functions must have arguments 'x', 'w0' and 'wt'")
  }
  
  # check if index formula needs w0 or wt, but is missing:
  w0.needed <- sapply(X=index, FUN=function(z) !is.null(formals(z)$w0) & w0.miss)
  wt.needed <- sapply(X=index, FUN=function(z) !is.null(formals(z)$wt) & wt.miss)
  if(any(w0.needed) || any(wt.needed)){
    stop("Non-valid input for w0 or wt -> weights required for at least one of the functions in 'index'")
  }
  
  # gather data:
  dt <- data.table::data.table(grp, w0, wt, x)
  dt <- dt[grp!="00",]
  if(!all(is.na(dt$x))) dt <- dt[!is.na(x), ] # drop these NAs because they would bias aggregation
  dt[, names(index) := x]
  dt[, "x":=NULL]
  data.table::setkey(x=dt, key="grp") # do not set the key before
  if(nrow(dt)<=1L) return(NULL) # no aggregation possible
  
  # data for stepwise aggregation:
  dt.agg <- data.table::copy(dt)
  
  # keep either coicop bundles or their components,
  # but not both. to allow aggregation, where the last
  # digit of a coicop id is dropped in each step,
  # replace bundle codes with their (first) component:
  dt.agg <- dt.agg[keep.bundle(id=grp),]
  grp.unbl <- hicp::unbundle(dt.agg$grp)
  dt.agg$grp <- grp.unbl[!duplicated(names(grp.unbl))]
  
  # bottom level to top:
  max.lvl <- max(nchar(dt.agg$grp), na.rm=TRUE):2
  
  # empty container to store aggregation results:
  out <- vector(mode="list", length=length(max.lvl))
  
  # move from bottom to top in aggregation:
  for(i in max.lvl){
    
    # set coicop parents:
    dt.agg[, "parent" := substr(x=grp, start=1, stop=nchar(grp)-1)]
    dt.agg[nchar(grp)==2, "parent" := "00"]
    
    # aggregate to next level:
    res <- dt.agg[nchar(grp)==i, c(
      mapply(FUN=function(f,x,w0,wt) as.list(f(x, w0, wt)), index, .SD, MoreArgs=list(w0=w0, wt=wt)),
      "w0"=sum(w0, na.rm=TRUE),
      "wt"=sum(wt, na.rm=TRUE)),
      by=list("grp"=parent),
      .SDcols=names(index)]
    # https://stackoverflow.com/questions/59059743/apply-different-functions-to-different-columns-programmatically-in-data-table-r
    
    # # drop NAs to allow that higher-level indices are added in the next step
    # res <- res[!is.na(laspey),]
    
    # add higher-level groups that were not considered until now:
    grp.add <- setdiff(unique(dt.agg[nchar(grp)<i, grp]), unique(res$grp))
    dt.add <- dt.agg[grp%in%grp.add, ]
    dt.add[, "parent" := NULL]
    dt.agg <- data.table::rbindlist(l=list(res, dt.add), use.names=TRUE)
    
    # store results of this aggregation step:
    out[[i-1]] <- res
    
  }
  
  # rbind results:
  out <- data.table::rbindlist(l=out)
  
  # add user-defined aggregates:
  for(j in seq_along(add)){
    if(all(add[[j]]%in%dt$grp, na.rm=TRUE) || (!settings$add.exact && any(add[[j]]%in%dt$grp, na.rm=TRUE))){
      add[[j]] <- dt[grp%in%add[[j]], c(
        "grp"=names(add)[[j]],
        mapply(FUN=function(f,x,w0,wt) as.list(f(x, w0, wt)), index, .SD, MoreArgs=list(w0=w0, wt=wt)),
        "w0"=sum(w0, na.rm=TRUE),
        "wt"=sum(wt, na.rm=TRUE)),
        .SDcols=names(index)]
    }else{
      add[[j]] <- data.table::data.table("grp"=names(add)[j])
    }
  }
  
  # rbind results:
  add <- data.table::rbindlist(l=add, use.names=TRUE, fill=TRUE)
  out <- data.table::rbindlist(l=list(out, add), use.names=TRUE, fill=TRUE)
  out[, "is_aggregated":=TRUE]
  data.table::setcolorder(x=out, c("grp","is_aggregated","w0","wt"))
  
  # add lowest level if wanted:
  if(settings$keep.lowest){
    
    # lowest level indices are always laspeyres in HICP:
    dt.low <- data.table::data.table(grp, "is_aggregated"=FALSE, w0, wt)
    dt.low[, names(index) := x]
    grp.add <- setdiff(unique(dt.low$grp), unique(out$grp))
    out <- data.table::rbindlist(l=list(out, dt.low[grp%in%grp.add]), use.names=TRUE, fill=TRUE)
    
  }else{
    
    # drop column
    out[, "is_aggregated":=NULL]
    
  }
  
  # set names and key:
  if(w0.miss) out[, "w0":=NULL]
  if(wt.miss) out[, "wt":=NULL]
  data.table::setkeyv(x=out, cols="grp")
  
  # print output to console:
  return(out)
  
}

# END
