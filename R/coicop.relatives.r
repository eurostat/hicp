# START

# Title:    COICOP relatives
# Author:   Sebastian Weinand
# Date:     27 January 2026

# non-exported helper function to simplify the output of parent() and child():
simplify.rel <- function(x, settings=list()){
  
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
    idx <- gl>1 & is.bundle(id=out, settings=settings)
    
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

# derive COICOP parents:
parent <- function(id, usedict=TRUE, closest=TRUE, k=1L, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$coicop.prefix)) settings$coicop.prefix <- getOption("hicp.coicop.prefix")
  if(is.null(settings$all.items.code)) settings$all.items.code <- getOption("hicp.all.items.code")
  if(is.null(settings$simplify)) settings$simplify <- FALSE
  
  # input checks:
  check.char(x=id, min.len=0)
  check.log(x=usedict, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=closest, min.len=1, max.len=1, na.ok=FALSE)
  check.num(x=k, na.ok=FALSE, int=c(1,Inf))
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.prefix, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$all.items.code, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$simplify, min.len=1, max.len=1, na.ok=FALSE)
  
  # set dictionary:
  dict <- dictionary(x="COICOP", which="CODE", settings=settings)
  
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
  id <- unbundle(id=id, settings=list(simplify=TRUE, 
                                      coicop.version=settings$coicop.version,
                                      coicop.prefix=settings$coicop.prefix))
  # unbundle() must use settings$simplify=TRUE
  
  # derive levels of coicop codes:
  n <- hicp::level(id=id, settings=settings)
  
  # define code dictionary and set levels of coicop codes in dictionary:
  if(usedict){
    dict <- c(allid, dict)
    names(dict) <- dict
    ndict <- hicp::level(id=dict, settings=settings)
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

# derive COICOP children:
child <- function(id, usedict=TRUE, closest=TRUE, k=1L, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$coicop.prefix)) settings$coicop.prefix <- getOption("hicp.coicop.prefix")
  if(is.null(settings$all.items.code)) settings$all.items.code <- getOption("hicp.all.items.code")
  if(is.null(settings$simplify)) settings$simplify <- FALSE
  
  # input checks:
  check.char(x=id, min.len=0)
  check.log(x=usedict, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=closest, min.len=1, max.len=1, na.ok=FALSE)
  check.num(x=k, na.ok=FALSE, int=c(1,Inf))
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.prefix, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$all.items.code, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$simplify, min.len=1, max.len=1, na.ok=FALSE)
  
  # set dictionary:
  dict <- dictionary(x="COICOP", which="CODE", settings=settings)
  
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
  id <- unbundle(id=id, settings=list(simplify=TRUE,
                                      coicop.version=settings$coicop.version,
                                      coicop.prefix=settings$coicop.prefix))
  # unbundle() must use settings$simplify=TRUE
  
  # derive levels of coicop codes:
  n <- hicp::level(id=id, settings=settings)
  
  # define code dictionary and set levels of coicop codes in dictionary:
  if(usedict){
    dict <- c(allid, dict)
    names(dict) <- dict
    ndict <- hicp::level(id=dict, settings=settings)
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
