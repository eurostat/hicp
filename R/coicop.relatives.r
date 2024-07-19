# START

# Title:    COICOP relatives
# Author:   Sebastian Weinand
# Date:     19 July 2024

# check validity of coicop code:
is.coicop <- function(id, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$unbundle)) settings$unbundle <- getOption("hicp.unbundle")
  
  # input checks:
  check.char(x=id)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$unbundle, min.len=1, max.len=1, na.ok=FALSE)
  
  # match arguments:
  ver <- match.arg(arg=settings$coicop.version, choices=c("ecoicop","ecoicop-hicp","coicop-1999","coicop-2018"))
  
  # set ecoicop version:
  if(ver=="ecoicop") codes <- coicop[["ecoicop"]]
  if(ver=="ecoicop-hicp") codes <- coicop[["ecoicop-hicp"]]
  if(ver=="coicop-1999") codes <- coicop[["coicop-1999"]]
  if(ver=="coicop-2018") codes <- coicop[["coicop-2018"]]
  
  # check if valid bundle code or not:
  if(settings$unbundle){
    check.bdl <- hicp::is.bundle(id)
  }else{
    check.bdl <- FALSE
  } 
  
  # flag valid coicop codes:
  out <- id%in%codes | check.bdl
  
  # return output:
  return(out)
  
}

# get coicop level:
level <- function(id, label=FALSE, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$unbundle)) settings$unbundle <- getOption("hicp.unbundle")
  if(is.null(settings$all.items.code)) settings$all.items.code <- getOption("hicp.all.items.code")
  
  # input checks:
  check.char(x=id)
  check.log(x=label, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$unbundle, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$all.items.code, min.len=1, max.len=1, na.ok=FALSE)
  
  # set all-items code:
  allid <- settings$all.items.code
  
  # set to NA if no valid coicop code and not total:
  id[!hicp::is.coicop(id, settings=settings) & id!=allid] <- NA_character_
  
  # resolve levels of coicop bundles properly:
  lvl <- as.integer(nchar(sapply(X=strsplit(id,"\\_|-"), "[[", 1L)))
  
  # set highest level to 1:
  lvl[id==allid] <- 1L
  
  # resolve labels:
  if(label){
    lvl <- c("total","division","group","class","subclass")[match(x=lvl, table=1:5)]
  }
  
  # return output to console:
  return(lvl)
  
}

# derive coicop parents:
parent <- function(id, flag=TRUE, direct=FALSE, settings=list()){

  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$unbundle)) settings$unbundle <- getOption("hicp.unbundle")
  if(is.null(settings$all.items.code)) settings$all.items.code <- getOption("hicp.all.items.code")

  # input checks:
  check.char(x=id)
  check.log(x=flag, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=direct, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$unbundle, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$all.items.code, min.len=1, max.len=1, na.ok=FALSE)

  # set all-items code:
  allid <- settings$all.items.code
  
  # set non-valid coicop codes to NA:
  id[!hicp::is.coicop(id, settings=settings) & id!=allid] <- NA_character_
  
  if(all(is.na(id))){
    
    out <- id
    
  }else{
    
    # subset to unique ids:
    id.tab <- unique(id[!is.na(id)])
    
    # resolve coicop bundles for further processing:
    if(settings$unbundle){
      id.tab <- hicp::unbundle(id=id.tab)
    }else{
      names(id.tab) <- id.tab
      id.tab[hicp::is.bundle(id.tab)] <- NA
    }
    
    # drop NAs:
    id.tab <- id.tab[!is.na(id.tab)]
    
    # coicop levels:
    n <- hicp::level(id=id.tab, label=FALSE, settings=settings)
    
    # derive parents:
    out <- rep(NA_character_, length(id.tab))
    for(i in seq_along(id.tab)){
      
      # subset to relevant codes:
      id.sub <- id.tab[n<n[i]]
      
      # flag direct or all parents:
      if(direct){
        idx <- id.sub%in%substr(x=id.tab[i], start=1, stop=nchar(id.tab[i])-1)
      }else{
        idx <- startsWith(x=id.tab[i], prefix=id.sub)
      }
      
      # resolve bundle codes:
      if(any(idx, na.rm=TRUE)){
        res <- id.sub[idx][which.max(nchar(id.sub[idx]))]
        if(res%in%names(id.tab)){
          out[i] <- res
        }else{
          out[i] <- names(res)
        }
      }
      
    }
    
    # set all-items hicp as parent of divisions:
    if(any(id.tab==allid, na.rm=TRUE)){
      out[id.tab!=allid & n==2L] <- allid
      if(!direct) out[id.tab!=allid & is.na(out)] <- allid
    }
    
    # match to initial ordering:
    out <- out[match(x=id, table=names(id.tab))]
    
  }
  
  # flag if parent available or not:
  if(flag){
    out <- !is.na(out)
    out[is.na(id)] <- NA
  }
  
  # return output:
  return(out)
  
}

# derive coicop children:
child <- function(id, flag=TRUE, direct=FALSE, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$unbundle)) settings$unbundle <- getOption("hicp.unbundle")
  if(is.null(settings$all.items.code)) settings$all.items.code <- getOption("hicp.all.items.code")

  # input checks:
  check.char(x=id)
  check.log(x=flag, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=direct, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.log(x=settings$unbundle, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$all.items.code, min.len=1, max.len=1, na.ok=FALSE)
  
  # set all-items code:
  allid <- settings$all.items.code
  
  # set non-valid coicop codes to NA:
  id[!hicp::is.coicop(id, settings=settings) & id!=allid] <- NA_character_
  
  if(all(is.na(id))){
    
    out <- as.list(id)
    
  }else{
    
    # subset to unique ids:
    id.tab <- unique(id[!is.na(id)])
    
    # resolve coicop bundles for further processing:
    if(settings$unbundle){
      id.tab <- hicp::unbundle(id=id.tab)
    }else{
      names(id.tab) <- id.tab
      id.tab[hicp::is.bundle(id.tab)] <- NA
    }
    
    # drop NAs:
    id.tab <- id.tab[!is.na(id.tab)]
    
    # coicop levels:
    n <- hicp::level(id=id.tab, label=FALSE, settings=settings)
    
    # derive children:
    out <- vector(mode="list", length=length(id.tab))
    for(i in seq_along(id.tab)){
      
      # direct or all children:
      if(direct){
        id.sub <- id.tab[-i][n[-i]==n[i]+1]
      }else{
        id.sub <- id.tab[-i][n[-i]>n[i]]
      }
      
      # flag children:
      idx <- startsWith(x=id.sub, prefix=id.tab[i])
      
      # resolve bundle codes:
      if(any(idx, na.rm=TRUE)){
        res <- id.sub[idx]
        if(all(res%in%names(id.tab))){
          out[[i]] <- as.vector(res)
        }else{
          out[[i]] <- unique(names(res))
        }
      }
      
    }
    
    # allowed children below top-level:
    if(direct){
      achld <- coicop[[settings$coicop.version]]
      achld <- achld[nchar(achld)==2L]
    }else{
      achld <- id.tab[id.tab!=allid]
      if(length(achld)>0) achld <- achld[hicp::is.coicop(id=achld, settings=settings)]
    }
    
    # set children below top-level:
    if(allid%in%id.tab & any(achld%in%id.tab, na.rm=TRUE)){
      out[[which(id.tab==allid & any(id.tab%in%achld, na.rm=TRUE))]] <- unique(names(id.tab[id.tab%in%achld]))
    }
    
    # match to initial ordering:
    out <- out[match(x=id, table=names(id.tab))]
    
  }
  
  # replace nulls by NA or empty character:
  out[is.na(id)] <- NA_character_
  out <- lapply(X=out, FUN=function(z) if(is.null(z)){character()}else{z})
  
  # flag if parent available or not:
  if(flag){
    out <- lengths(out)>0
    out[is.na(id)] <- NA
  }
  
  # return output:
  return(out)
  
}

# END
