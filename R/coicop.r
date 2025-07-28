# START

# Title:    COICOP codes, bundles, and relatives
# Author:   Sebastian Weinand
# Date:     22 July 2025

# check validity of coicop code:
is.coicop <- function(id, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  
  # input checks:
  check.char(x=id, min.len=0)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  
  # set coicop dictionary:
  dict <- set.coicop(x=settings$coicop.version)
  
  # flag valid coicop codes:
  out <- id%in%dict
  
  # return output:
  return(out)
  
}

# get coicop level:
level <- function(id, label=FALSE, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$all.items.code)) settings$all.items.code <- getOption("hicp.all.items.code")
  if(is.null(settings$coicop.bundles)) settings$coicop.bundles <- getOption("hicp.coicop.bundles")
  
  # input checks:
  check.char(x=id, min.len=0)
  check.log(x=label, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$all.items.code, min.len=1, max.len=1, na.ok=FALSE)
  check.list(x=settings$coicop.bundles, min.len=0, names=TRUE)
  
  # set all-items code:
  allid <- settings$all.items.code
  
  # set to NA if no valid coicop code, no bundle code and not all-items code:
  bdl <- hicp::is.bundle(id=id, settings=settings)
  id[!hicp::is.coicop(id, settings=settings) & id!=allid & !bdl] <- NA_character_
  
  # derive levels:
  out <- as.integer(nchar(id))
  
  # resolve level of coicop bundles correctly:
  if(any(bdl)){
    lookup <- nchar(sapply(X=settings$coicop.bundles, "[[", 1)) # look up table
    idx <- match(x=id, table=names(lookup))
    out <- ifelse(test=is.na(idx), yes=out, no=lookup[idx])
  }
  
  # set highest level to 1:
  out[id==allid] <- 1L
  
  # resolve labels:
  if(label){
    lbls <- c("total","division","group","class","subclass","subsubclass")
    out <- lbls[match(x=out, table=1:6)]
  }
  
  # return output to console:
  return(out)
  
}

# check if coicop code is bundle:
is.bundle <- function(id, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.bundles)) settings$coicop.bundles <- getOption("hicp.coicop.bundles")
  
  # input checks:
  check.char(x=id, min.len=0)
  check.list(x=settings$coicop.bundles, min.len=0, names=TRUE)
  
  # match:
  out <- id%in%names(settings$coicop.bundles)
  
  # return output:
  return(out)
  
}

# resolve coicop bundles into their components:
unbundle <- function(id, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.bundles)) settings$coicop.bundles <- getOption("hicp.coicop.bundles")
  if(is.null(settings$simplify)) settings$simplify <- FALSE
  
  # input checks:
  check.char(x=id, min.len=0)
  check.list(x=settings$coicop.bundles, min.len=0, names=TRUE)
  check.log(x=settings$simplify, min.len=1, max.len=1, na.ok=FALSE)
  
  # match id to bundle dictionary:
  out <- settings$coicop.bundles[match(x=id, table=names(settings$coicop.bundles))]
  names(out) <- NULL
  idx <- lengths(out)<1L
  out[idx] <- id[idx]
  
  # simplify list to vector:
  if(settings$simplify){
    if(length(out)>0){
      # length might be greater now!
      out <- stats::setNames(unlist(x=out, use.names=FALSE), rep(id, lengths(out)))
    }else{
      out <- id
    }
  }
  
  # return output:
  return(out)
  
}

# non-exported helper function to simplify the output of parent() and child():
simplify.rel <- function(x, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.bundles)) settings$coicop.bundles <- getOption("hicp.coicop.bundles")
  
  if(length(x)>0){
    
    # store copy of input:
    x0 <- x
    
    # replace empty list elements with NA and coerce into vector:
    x[lengths(x)<1L]<-NA_character_
    out <- unlist(x=x, use.names=FALSE)
    
    # derive list ids and lengths:
    l <- lengths(x)
    gid <- rep(x=1:length(x), times=l)
    gl <- rep(x=l, times=l)
    
    # drop bundle codes if subcomponents are present:
    idx <- gl>1 & hicp::is.bundle(id=out, settings=settings)
    
    # check if simplification works:
    if(any(duplicated(gid[!idx]), na.rm=TRUE)){
      out <- x0
    }else{
      out <- out[!idx]
    }
    
  }else{
    
    out <- character()
    
  }
  
  # return output:
  return(out)
  
}

# derive coicop parents:
parent <- function(id, usedict=TRUE, closest=TRUE, k=1L, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$all.items.code)) settings$all.items.code <- getOption("hicp.all.items.code")
  if(is.null(settings$coicop.bundles)) settings$coicop.bundles <- getOption("hicp.coicop.bundles")
  if(is.null(settings$simplify)) settings$simplify <- FALSE
  
  # input checks:
  check.char(x=id, min.len=0)
  check.log(x=usedict, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=closest, min.len=1, max.len=1, na.ok=FALSE)
  check.num(x=k, na.ok=FALSE, int=c(1,Inf))
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$all.items.code, min.len=1, max.len=1, na.ok=FALSE)
  check.list(x=settings$coicop.bundles, min.len=0, names=TRUE)
  check.log(x=settings$simplify, min.len=1, max.len=1, na.ok=FALSE)
  
  # set coicop dictionary:
  dict <- set.coicop(x=settings$coicop.version)
  
  # set all-items code:
  allid <- settings$all.items.code
  
  # set to NA if no valid coicop code, no bundle code and not all-items code:
  bdl <- hicp::is.bundle(id=id, settings=settings)
  id[!hicp::is.coicop(id, settings=settings) & id!=allid & !bdl] <- NA_character_
  
  # copy initial ids:
  id0 <- id
  
  # subset to unique ids:
  id <- unique(id[!is.na(id)])
  
  # resolve coicop bundles for further processing:
  id <- hicp::unbundle(id=id, settings=list(simplify=TRUE, coicop.bundles=settings$coicop.bundles))
  # unbundle() must use settings$simplify=TRUE
  
  # derive levels of coicop codes:
  n <- hicp::level(id=id, label=FALSE, settings=settings)
  
  # define code dictionary and set levels of coicop codes in dictionary:
  if(usedict){
    dict <- c(allid, dict)
    names(dict) <- dict
    ndict <- hicp::level(id=dict, label=FALSE, settings=settings)
  }else{
    dict <- id
    ndict <- n
  }
  
  # position of all-items code in dictionary:
  mall <- allid==dict
  
  # derive parents:
  out <- vector(mode="list", length(id))
  if(closest){
    
    for(i in seq_along(id)){
      
      # all higher levels:
      ml <- n[i]>ndict # same as (n[i]-ndict)>0L but faster
      
      # flag parents of ids based on prefix and level and add all-items code if appropriate:
      idx <- (startsWith(x=id[i], prefix=dict) | mall) & ml
      
      # subset to closest parents if available:
      if(any(idx, na.rm=TRUE)) out[[i]] <- names(dict[idx][ndict[idx]==max(ndict[idx])])
      
    }
    
  }else{
    
    for(i in seq_along(id)){
      
      # all levels in k:
      ml <- (n[i]-ndict)%in%k
      
      # flag parents of ids based on prefix and level and add all-items code if appropriate:
      idx <- (startsWith(x=id[i], prefix=dict) | mall) & ml
      
      # subset to k-th parents if available:
      if(any(idx, na.rm=TRUE)) out[[i]] <- names(dict[idx])
      
    }
    
  }
  
  # for bundle codes there can only be the same one parent code
  # so no merging is needed
  # out <- tapply(X=out, INDEX=names(id), FUN=function(z) unique(unlist(z)), simplify=FALSE)
  
  # match to initial ordering:
  out <- out[match(x=id0, table=names(id))]
  
  # drop names and reuse initial NAs:
  names(out) <- NULL
  out[is.na(id0)] <- NA_character_
  # attr(out, "dim") <- NULL
  
  # try to simplify list into vector:
  if(settings$simplify) out <- simplify.rel(x=out, settings=settings)
  
  # return output:
  return(out)
  
}

# derive coicop children:
child <- function(id, usedict=TRUE, closest=TRUE, k=1L, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$all.items.code)) settings$all.items.code <- getOption("hicp.all.items.code")
  if(is.null(settings$coicop.bundles)) settings$coicop.bundles <- getOption("hicp.coicop.bundles")
  if(is.null(settings$simplify)) settings$simplify <- FALSE
  
  # input checks:
  check.char(x=id, min.len=0)
  check.log(x=usedict, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=closest, min.len=1, max.len=1, na.ok=FALSE)
  check.num(x=k, na.ok=FALSE, int=c(1,Inf))
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$all.items.code, min.len=1, max.len=1, na.ok=FALSE)
  check.list(x=settings$coicop.bundles, min.len=0, names=TRUE)
  check.log(x=settings$simplify, min.len=1, max.len=1, na.ok=FALSE)
  
  # set coicop dictionary:
  dict <- set.coicop(x=settings$coicop.version)
  
  # set all-items code:
  allid <- settings$all.items.code
  
  # set to NA if no valid coicop code, no bundle code and not all-items code:
  bdl <- hicp::is.bundle(id=id, settings=settings)
  id[!hicp::is.coicop(id, settings=settings) & id!=allid & !bdl] <- NA_character_
  
  # copy initial ids:
  id0 <- id
  
  # subset to unique ids:
  id <- unique(id[!is.na(id)])
  
  # resolve coicop bundles for further processing:
  id <- hicp::unbundle(id=id, settings=list(simplify=TRUE, coicop.bundles=settings$coicop.bundles))
  # unbundle() must use settings$simplify=TRUE
  
  # derive levels of coicop codes:
  n <- hicp::level(id=id, label=FALSE, settings=settings)
  
  # define code dictionary and set levels of coicop codes in dictionary:
  if(usedict){
    dict <- c(allid, dict)
    names(dict) <- dict
    ndict <- hicp::level(id=dict, label=FALSE, settings=settings)
  }else{
    dict <- id
    ndict <- n
  }
  
  # position of all-items code in ids:
  mall <- allid==id
  
  # derive children:
  out <- vector(mode="list", length(id))
  if(closest){
    
    for(i in seq_along(id)){
      
      # all lower levels: 
      ml <- ndict>n[i] # same as (ndict-n[i])>0L but faster
      
      # flag children of ids based on prefix and level and children of all-items code:
      # idx <- (startsWith(x=dict, prefix=id[i]) & ml) | (allid==id[i] & ml)
      idx <- (startsWith(x=dict, prefix=id[i]) | mall[i]) & ml
      
      # subset to closest children if available:
      if(any(idx, na.rm=TRUE)) out[[i]] <- names(dict[idx][ndict[idx]==min(ndict[idx])])
      
    }
    
  }else{
    
    for(i in seq_along(id)){
      
      # all levels in k: 
      ml <- (ndict-n[i])%in%k
      
      # flag children of ids based on prefix and level and children of all-items code:
      # idx <- (startsWith(x=dict, prefix=id[i]) & ml) | (allid==id[i] & ml)
      idx <- (startsWith(x=dict, prefix=id[i]) | mall[i]) & ml
      
      # subset to k-th children if available:
      if(any(idx, na.rm=TRUE)) out[[i]] <- names(dict[idx])
      
    }
    
  }
  
  # merge bundle codes again if any are present:
  if(any(bdl)){
    out <- tapply(X=out, INDEX=names(id), FUN=function(z) unique(unlist(z)), simplify=FALSE)
  }else{
    names(out) <- names(id)
  }
  
  # match to initial ordering:
  out <- out[match(x=id0, table=names(out))]
  
  # drop names and reuse initial NAs:
  names(out) <- NULL
  out[is.na(id0)] <- NA_character_
  attr(out, "dim") <- NULL
  
  # try to simplify list into vector:
  if(settings$simplify) out <- simplify.rel(x=out, settings=settings)
  
  # return output:
  return(out)
  
}

# END
