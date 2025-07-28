# START

# Title:      COICOP tree
# Author:     Sebastian Weinand
# Date:       22 July 2025

# non-exported helper function to check weight sum including coicop bundles:
check.weightsum <- function(id, w, w.all, w.tol=1/100, settings=list()){
  
  # @description:
  # check if the weights in w add up to w.all, taking into
  # account coicop bundle codes. for example, for the codes
  # c("081","082","083","08X"), the function will check if
  # (081,082,083) and also if (081,08X) add up to w.all
  
  # note that the function is not vectorized, i.e., it can
  # only be checked if all w add up to one w.all
  
  # @args:
  # id        character vector of codes, e.g. (081,082,083)
  # w         numeric weight of each code, e.g. (0.1,0.2,0.3)
  # w.all     numeric weight of the parent of the codes, e.g. (0.6)
  # settings  see is.bundle()
  
  # set default settings if missing:
  if(is.null(settings$coicop.bundles)) settings$coicop.bundles <- getOption("hicp.coicop.bundles")
  
  # flag bundle codes:
  bdl.flag <- hicp::is.bundle(id=id, settings=settings)
  
  # drop bundle codes and check weight sum:
  excl <- !bdl.flag
  check.excl <- abs(sum(w[excl], na.rm=TRUE)-w.all) < w.tol
  
  # keep bundle codes but drop bundle components and check weight sum:
  if(any(bdl.flag, na.rm=TRUE)){
    incl <- !id%in%unique(unlist(x=settings$coicop.bundles, use.names=FALSE))
    check.incl <- abs(sum(w[incl], na.rm=TRUE)-w.all) < w.tol
  }else{
    incl <- check.incl <- FALSE
  }
  
  # output:
  out <- excl*check.excl | incl*check.incl
  return(out)
  
}

# non-exported helper function to check frequency including coicop bundles:
check.by <- function(id, by=NULL, settings=list()){
  
  # @description:
  # check if all ids are present in all by
  
  # @args:
  # id        character, codes
  # by        factor, grouping variable 
  # settings  see is.bundle() and unbundle()
  
  # check if bundle code or components should be kept:
  idx <- keep.bundle(id=id, by=by, settings=settings)
  
  # check if all ids present in all by:
  if(all(table(id[idx], by[idx])>=1L)){
    res <- idx
  }else{
    res <- rep(x=FALSE, times=length(id))
  }
  
  # return output:
  return(res)
  
}

# non-exported core function to derive tree:
derive.tree <- function(id, by=NULL, w=NULL, lvl, p, settings=list()){
  
  # @description:
  # loop to derive the coicop tree following a top-down-approach.
  # starting at the highest level, it is checked (1) if the code 
  # in id has children, (2) if the children's weights correctly 
  # add up to the weight of the (parent) id (if w provided), 
  # (3) and if the children can all be found in all groups in 
  # by (if by provided). only if all checks are ok, the children 
  # are stored and further processed. if not, the parent id is 
  # kept and the processing stops in the respective node. 
  
  # @args:
  # id        character of codes
  # lvl       integer specifying the coicop level of id
  # p         character specifying the parent for each id
  # by        vector specifying the grouping variable
  # w         numeric vector of weights for each id
  # settings  control settings, see parent() and is.bundle()
  
  # initialize loop:
  idtab <- NA_character_ # this gives the highest level ids as children
  idkeep <- character() # container to keep codes which do not need to be processed further
  K <- unique(by) # unique groups
  L <- sort(unique(lvl)) # sorted unique code levels
  res <- vector(mode="list", length=length(L))
  
  # loop from minimum to maximum level:
  for(l in seq_along(res)){
    
    # store the parent code, which is only overwritten if all checks below are satisfied:
    out <- as.list(idtab) 
    for(j in seq_along(out)){
      
      # check for each group in by if children are available
      # and if their weights add up correctly to the weight
      # of the parent in idtab:
      outtmp <- vector(mode="list", length=length(K))
      for(k in seq_along(outtmp)){
        
        # match parent to id (including also matching of NAs):
        idx <- match(x=p[by==K[k]], table=idtab[j], nomatch=0)
        
        # check if any children found:
        if(sum(idx)>0){
          
          # children id:
          idtmp <- id[by==K[k]][idx>0]
          
          # weight of parent:
          wp <- w[by==K[k] & id%in%idtab[j]] # use %in% to make it numeric() if no match
          
          # check if weights add up, if provided:
          if(is.null(w) || length(wp)<1L){
            wok <- TRUE
          }else{
            wok <- check.weightsum(
              id=idtmp,
              w=w[by==K[k]][idx>0], # weights of children
              w.all=wp, # weight of parent
              w.tol=settings$w.tol, # tolerance for checking
              settings=settings)
          }
          
          # store only if any id satisfied the check:
          if(any(wok)){
            outtmp[[k]] <- idtmp[wok]
          }
          
        }
        
      }
      
      # we want to check against all levels in by:
      bytmp <- factor(x=rep(x=K, times=lengths(outtmp)), levels=K) 
      outtmp <- unlist(x=outtmp, use.names=FALSE)
      
      # check if all codes are present in all elements in by.
      # it is important that this is done after the checking of
      # the weights due to bundle codes. if, for example, the weights
      # for (081,08X) as well as for (081,082,083) correctly add to to
      # the weight for 08 but 083 is not present in all elements of by,
      # the code 08X can be kept.
      if(length(outtmp)>0L){
        byok <- check.by(id=outtmp, by=bytmp, settings=settings)
        if(any(byok)) out[[j]] <- outtmp[byok]
      }
      
    }
    
    # store all relevant codes for each i but do not
    # further process if it is known that there will
    # be no further child:
    restmp <- unique(unlist(x=out, use.names=FALSE))
    idkeep <- c(idkeep, intersect(restmp, idtab))
    idtab <- setdiff(restmp, idtab)
    res[[l]] <- sort(c(idtab, idkeep))
    
  }
  
  # return results of last iteration:
  return(res)
  
}

# derive coicop tree:
tree <- function(id, by=NULL, w=NULL, flag=FALSE, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$all.items.code)) settings$all.items.code <- getOption("hicp.all.items.code")
  if(is.null(settings$coicop.bundles)) settings$coicop.bundles <- getOption("hicp.coicop.bundles")
  if(is.null(settings$w.tol)) settings$w.tol <- 1/100
  if(is.null(settings$max.lvl)) settings$max.lvl <- NULL
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  settings$simplify <- TRUE # for function parent()
  
  # input checks:
  check.char(x=id, min.len=0)
  if(!(!is.list(by) & is.vector(by) || is.factor(by) || is.null(by))){
    stop(paste("Non-valid input for by -> must be a vector or NULL"), call.=FALSE)
  }
  check.num(x=w, null.ok=TRUE, int=c(0,Inf))
  check.lengths(x=id, y=w)
  check.lengths(x=id, y=by)
  check.log(x=flag, max.len=1, na.ok=FALSE)
  check.num(x=settings$max.lvl, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE, int=c(0,Inf))
  check.num(x=settings$w.tol, min.len=1, max.len=1, na.ok=FALSE, int=c(0,Inf))
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$all.items.code, min.len=1, max.len=1, na.ok=FALSE)
  check.list(x=settings$coicop.bundles, min.len=0, names=TRUE)
  check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  
  # set all-items code:
  allid <- settings$all.items.code
  
  # set to NA if no valid coicop code, no bundle code and not all-items code:
  bdl <- hicp::is.bundle(id=id, settings=settings)
  id[!hicp::is.coicop(id, settings=settings) & id!=allid & !bdl] <- NA_character_
  
  # copy initial ids:
  id0 <- id
  
  # flag missing values:
  nas <- !stats::complete.cases(id, by, w)
  
  # subset to non-missing values:
  id <- id[!nas]
  w <- w[!nas]
  by <- by[!nas]
  
  # resolve level of coicop bundles:
  lvl <- hicp::level(id=id, label=FALSE, settings=settings)
  
  # prepare groups:
  if(is.null(by)){
    # create one artificial group:
    by <- rep(x=1L, times=length(id))
  }else{
    # subset to intersecting levels:
    idx <- lvl%in%Reduce(f=intersect, x=split(lvl, by))
    id <- id[idx]
    w <- w[idx]
    by <- by[idx]
    lvl <- lvl[idx]
  }
  
  # check for duplicated ids:
  if(settings$chatty && any(duplicated(data.frame(id, by)))){
    warning("Duplicated ids found -> results may be misleading.")
  }
  
  # set maximum coicop level if there are any ids:
  if(length(id)>0L){
    max.lvl.check <- max(lvl)
  }else{
    max.lvl.check <- 0L
  }
  
  # check maximum coicop level against user input:
  if(is.null(settings$max.lvl)){
    max.lvl <- max.lvl.check
  }else{
    max.lvl <- min(max.lvl.check, settings$max.lvl)
  }
  
  # subset to ids below or equal to maximum coicop level:
  w <- w[lvl<=max.lvl]
  id <- id[lvl<=max.lvl]
  by <- by[lvl<=max.lvl]
  lvl <- lvl[lvl<=max.lvl] # put this at last position!
  
  # derive parents for each id:
  p <- hicp::parent(id=id, usedict=FALSE, closest=TRUE, settings=settings)
  
  # derive final tree of ids:
  out <- derive.tree(id=id, by=by, w=w, lvl=lvl, p=p, settings=settings)
  
  # flag ids to be kept in the data, but do not flag NAs as FALSE:
  if(flag){
    out <- id0%in%unlist(x=out[length(out)], use.names=FALSE)
    out <- as.logical(out)
  }
  
  # return output:
  return(out)
  
}

# END