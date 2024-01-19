# START

# Title:  Index number methods and aggregation
# Author: Sebastian Weinand
# Date:   19 January 2024

# bilateral index functions:
laspey <- function(x, w0, wt=NULL){

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
    P <- sqrt(laspey(x=x, w0=w0)*paasche(x=x, wt=wt))
    # see Ilo et al. (2004), p. 267, formula (15.12)

  }

  return(P)

}
toernq <- function(x, w0, wt){

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
aggregate <- function(x, w0, wt, grp, index=laspey, add=list(), keep.lowest=TRUE){

  # @Args:
  # x             unchained index series, i.e. unchain(x, t)
  # w0            weights used in the current period
  # wt            weights from a period t+1
  # grp           the grouping variable, should be coicop codes
  #               defined by [0-9]{1,5}; if NULL (default) or
  #               missing, one aggregation step only
  # index         a function or named list of functions specifiyng
  #               the index type, e.g. laspey() for the laspeyres index.
  #               each function must have arguments x, w0 and wt, even if
  #               the attributes are not used
  # add           named list of ids that can be found in grp, which are
  #               aggregated
  # keep.lowest   keep the lowest level indices that form
  #               the base of all aggregation steps

  # input checks:
  check.num(x=x, int=c(0,Inf))
  check.num(x=w0, int=c(0,Inf), miss.ok=TRUE)
  check.num(x=wt, int=c(0,Inf), miss.ok=TRUE)
  check.char(x=grp)
  check.lengths(x=x, y=grp)
  check.log(x=keep.lowest, min.len=1, max.len=1, na.ok=FALSE)
  if(!is.list(add)) stop("Non-valid input for add -> only lists allowed")

  # input checks on both "w0" and "wt:
  if(missing(w0) & missing(wt)){stop("Both vectors 'w0' and 'wt' are missing")}
  if(missing(w0)){w0.miss <- TRUE; w0 <- wt}else{w0.miss <- FALSE}
  if(missing(wt)){wt.miss <- TRUE; wt <- w0}else{wt.miss <- FALSE}
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
  na.index <- names(which(c(w0.needed, wt.needed)))
  if(all(names(index)%in%na.index, na.rm=TRUE)) stop("Non-valid input for index -> weights must match index function(s)")

  # gather data:
  dt <- data.table(grp, w0, wt, x)
  dt <- dt[grp!="00",]
  if(!all(is.na(dt$x))) dt <- dt[!is.na(x), ] # drop these NAs because they would bias aggregation
  dt[, names(index) := x]
  dt[, "x":=NULL]
  setkey(x=dt, key="grp") # do not set the key before
  if(nrow(dt)<=1L) return(NULL) # no aggregation possible

  # data for stepwise aggregation:
  dt.agg <- copy(dt)

  # keep either coicop bundles or their components,
  # but not both. to allow aggregation, where the last
  # digit of a coicop id is dropped in each step,
  # replace bundle codes with their (first) component:
  dt.agg <- dt.agg[keep.bundle(id=grp),]
  grp.unbl <- unbundle(dt.agg$grp)
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
    dt.agg <- rbindlist(l=list(res, dt.add), use.names=TRUE)

    # store results of this aggregation step:
    out[[i-1]] <- res

  }

  # rbind results:
  out <- rbindlist(l=out)

  # add user-defined aggregates:
  for(j in seq_along(add)){
    if(all(add[[j]]%in%dt$grp, na.rm=TRUE)){
      add[[j]] <- dt[grp%in%add[[j]], c(
        "grp"=names(add)[[j]],
        mapply(FUN=function(f,x,w0,wt) as.list(f(x, w0, wt)), index, .SD, MoreArgs=list(w0=w0, wt=wt)),
        "w0"=sum(w0, na.rm=TRUE),
        "wt"=sum(wt, na.rm=TRUE)),
        .SDcols=names(index)]
    }else{
      add[[j]] <- data.table("grp"=names(add)[[j]])
    }
  }

  # rbind results:
  add <- rbindlist(l=add, use.names=TRUE, fill=TRUE)
  out <- rbindlist(l=list(out, add), use.names=TRUE, fill=TRUE)
  out[, "is_aggregated":=TRUE]
  setcolorder(x=out, c("grp","is_aggregated","w0","wt"))
  if(length(na.index)>0) out[, (na.index) := NA_real_]

  # add lowest level if wanted:
  if(keep.lowest){

    # lowest level indices are always laspeyres in HICP:
    dt.low <- data.table(grp,"is_aggregated"=FALSE,w0,wt)
    dt.low[, names(index) := x]
    grp.add <- setdiff(unique(dt.low$grp), unique(out$grp))
    out <- rbindlist(l=list(out, dt.low[grp%in%grp.add]), use.names=TRUE, fill=TRUE)

  }else{

    # drop column
    out[, "is_aggregated":=NULL]

  }

  # set names and key:
  if(w0.miss) out[, "w0":=NULL]
  if(wt.miss) out[, "wt":=NULL]
  setkeyv(x=out, cols="grp")

  # print output to console:
  return(out)

}

# END
