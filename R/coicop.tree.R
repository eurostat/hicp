# START

# Title:      COICOP tree
# Author:     Sebastian Weinand
# Date:       19 July 2024

# non-exported helper function to check weight sum including coicop bundles:
check.weight <- function(id, w, w.all, w.tol=1e-4){
  
  # without bundle:
  excl <- !hicp::is.bundle(id)
  check.excl <- abs(sum(w[excl], na.rm=TRUE)-w.all) < w.tol
  
  # with bundle:
  if(sum(!excl, na.rm=TRUE)>0){
    incl <- !(id%in%hicp::unbundle(id[!excl]))
    check.incl <- abs(sum(w[incl], na.rm=TRUE)-w.all) < w.tol
  }else{
    incl <- check.incl <- FALSE
  }
  
  # output:
  check.out <- excl*check.excl | incl*check.incl
  return(check.out)
  
}

# non-exported helper function to check frequency including coicop bundles:
check.freq <- function(id, freq){
  
  # flag coicop bundles:
  bdls <- hicp::is.bundle(id)
  
  # no bundle subcomponents present:
  if(any(bdls)) check <- all(!id%in%hicp::unbundle(id[bdls]), na.rm=TRUE) else check <- FALSE
  
  # no bundles or no bundle subcomponents present:
  if(all(!bdls) | check){
    if(all(table(id)>=freq, na.rm=TRUE)){
      out <- unique(id)
    }else{
      out <- character()
    }
  }else{
    # both bundle and corresponding subcomponents present
    # be careful, code order matters!
    out <- character()
    
    # all bundles present freq-times:
    id.tmp <- id[!id%in%hicp::unbundle(id[bdls])]
    if(all(table(id.tmp)>=freq, na.rm=TRUE)){
      out <- unique(id.tmp)
    }
    
    # all bundle components present freq-times:
    id.tmp <- id[!bdls]
    if(all(hicp::unbundle(id[bdls])%in%id, na.rm=TRUE) & all(table(id.tmp)>=freq, na.rm=TRUE)){
      out <- unique(id.tmp)
    }
    
  }
  
  # output:
  return(out)
  
}

# derive coicop tree:
tree <- function(id, by=NULL, w=NULL, max.lvl=NULL, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$unbundle)) settings$unbundle <- getOption("hicp.unbundle")
  if(is.null(settings$all.items.code)) settings$all.items.code <- getOption("hicp.all.items.code")
  if(is.null(settings$w.tol)) settings$w.tol <- 1/100

  # input checks:
  check.char(x=id)
  if(!(!is.list(by) & is.vector(by) || is.factor(by) || is.null(by))){
    stop(paste("Non-valid input for by -> must be a vector or NULL"), call.=FALSE)
  }
  check.num(x=w, null.ok=TRUE, int=c(0,Inf))
  check.lengths(x=id, y=w)
  check.lengths(x=id, y=by)
  check.num(x=max.lvl, min.len=1, max.len=1, null.ok=TRUE, na.ok=FALSE, int=c(0,Inf))
  check.num(x=settings$w.tol, min.len=1, max.len=1, na.ok=FALSE, int=c(0,Inf))
  check.log(x=settings$unbundle, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$all.items.code, min.len=1, max.len=1, na.ok=FALSE)
  
  # set all-items code:
  allid <- settings$all.items.code
  
  # one group:
  if(is.null(by)) by <- rep(1, length(id))
  if(any(duplicated(data.frame(id, by)))){
    warning("Duplicated ids found. Results may be misleading.")
  }
  
  # copy initial ids:
  id0 <- id
  
  # set non-valid coicop codes to NA:
  id[!hicp::is.coicop(id, settings=settings) & id!=allid] <- NA_character_
  
  # flag missing values:
  if(is.null(w)){
    nas <- is.na(id) | is.na(by)
  }else{
    nas <- is.na(id) | is.na(by) | is.na(w)
  }
  
  if(all(nas)){
    
    out <- id
    
  }else{
    
    # subset to non-missing values:
    id <- id[!nas]
    w <- w[!nas]
    by <- by[!nas]
    
    # resolve level of coicop bundles:
    lvl <- hicp::level(id=id, label=FALSE, settings=settings)
    
    # define maximum coicop level:
    max.lvl.check <- min(tapply(lvl, by, max))
    if(is.null(max.lvl)){
      max.lvl <- max.lvl.check
    }else{
      max.lvl <- min(max.lvl.check, max.lvl)
    }
    
    # set to false if all levels higher than maximum level wanted:
    if(all(lvl>max.lvl, na.rm=TRUE)) return(rep(FALSE, length(id0)))
    
    # derive coicop parents of each id:
    p <- hicp::parent(id=id, flag=FALSE, direct=FALSE, settings=settings)
    
    # subset to coicop ids below maximum level:
    p <- p[lvl<=max.lvl]
    w <- w[lvl<=max.lvl]
    id <- id[lvl<=max.lvl]
    by <- by[lvl<=max.lvl]
    lvl <- lvl[lvl<=max.lvl] # put this at last position!
    
    # initalize derivation of coicop tree:
    if(min(lvl)>1){
      id.tab <- unlist(
        x=lapply(
          X=split(x=id, f=substr(id,start=1,stop=2)),
          FUN=function(z) z[nchar(z)==min(nchar(z))]),
        use.names=FALSE)
    }else{
      id.tab <- id[lvl==min(lvl)]
    }
    
    # initialize loop:
    i <- 1
    M <- unique(by)
    res <- list(unique(id.tab))
    while(i<max.lvl){
      
      out <- as.list(id.tab)
      for(j in seq_along(id.tab)){
        
        out.tmp <- list()
        for(m in seq_along(M)){
          
          # match parent to id:
          idx <- match(x=p[by%in%M[m]], table=id.tab[j], nomatch=0)
          
          # check if any temporary ids found:
          if(sum(idx)>0){
            
            # temporary id:
            id.tmp <- id[by%in%M[m]][idx>0]
            
            # check if weights add up, if provided:
            if(is.null(w)){
              check <- TRUE
            }else{
              check <- check.weight(
                id=id.tmp,
                w=w[by%in%M[m]][idx>0], # weight of temporary ids
                w.all=w[by%in%M[m] & id==id.tab[j]], # weight of parent
                w.tol=settings$w.tol)
            }
            
            out.tmp[[m]] <- id.tmp[check]
            
          }else{
            
            out.tmp[[m]] <- id.tab[j]
            
          }
          
        }
        
        # unlist:
        out.tmp <- unlist(out.tmp)
        
        # drop bundles if subcomponents available:
        out.tmp <- check.freq(id=out.tmp, freq=length(M))
        
        # store result:
        if(length(out.tmp)>0) out[[j]] <- out.tmp
        
      }
      
      # store results and check if change happened:
      #res[[i]] <- unlist(out)
      #check <- length(setdiff(res[[i]], id.tab))>0
      #id.tab <- res[[i]]
      i <- i+1
      res[[i]] <- id.tab <- unique(unlist(out))
      
    }
    
    # flag ids to be kept in the data, but do not flag NAs as FALSE:
    out <- id0%in%res[[i]]
    out[is.na(id0)] <- NA
    
  }
  
  # coerce to logical:
  out <- as.logical(out)
  return(out)
  
}
