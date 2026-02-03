# START

# Title:    COICOP codes and special aggregates
# Author:   Sebastian Weinand
# Date:     26 January 2026

# flag valid COICOP codes:
is.coicop <- function(id, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$coicop.prefix)) settings$coicop.prefix <- getOption("hicp.coicop.prefix")
  
  # input checks:
  check.char(x=id, min.len=0)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.prefix, min.len=1, max.len=1, na.ok=FALSE)
  
  # set dictionary and flag valid codes:
  dict <- dictionary(x="COICOP", which="CODE", settings=settings)
  out <- id%in%dict
  return(out)
  
}

# flag bundle codes:
is.bundle <- function(id, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$coicop.prefix)) settings$coicop.prefix <- getOption("hicp.coicop.prefix")
  
  # input checks:
  check.char(x=id, min.len=0)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.prefix, min.len=1, max.len=1, na.ok=FALSE)
  
  # set dictionary and flag valid codes:
  dict <- dictionary(x="BDL", which="CODE", settings=settings)
  out <- id%in%dict
  return(out)
  
}

# flag special aggregate codes:
is.spec.agg <- function(id, settings=list()){

  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$coicop.prefix)) settings$coicop.prefix <- getOption("hicp.coicop.prefix")
  
  # input checks:
  check.char(x=id, min.len=0)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.prefix, min.len=1, max.len=1, na.ok=FALSE)

  # set dictionary and flag valid codes:
  dict <- dictionary(x="SA", which="CODE", settings=settings)
  out <- id%in%dict
  return(out)

}

# get COICOP digit level:
level <- function(id, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$coicop.prefix)) settings$coicop.prefix <- getOption("hicp.coicop.prefix")
  if(is.null(settings$all.items.code)) settings$all.items.code <- getOption("hicp.all.items.code")
  
  # input checks:
  check.char(x=id, min.len=0)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.prefix, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$all.items.code, min.len=1, max.len=1, na.ok=FALSE)
  
  # set all-items code:
  allid <- settings$all.items.code
  
  # set to NA if no valid coicop code, no bundle code and not all-items code:
  bdl <- hicp::is.bundle(id=id, settings=settings)
  id[!hicp::is.coicop(id, settings=settings) & id!=allid & !bdl] <- NA_character_
  
  # derive levels:
  out <- nchar(id)
  
  # resolve level of coicop bundles correctly:
  if(any(bdl)){
    dict <- dictionary(x="BDL", which="DEF", settings=settings)
    lookup <- nchar(sapply(X=dict, "[[", 1))
    idx <- match(x=id, table=names(lookup))
    out <- ifelse(test=is.na(idx), yes=out, no=lookup[idx])
  }
  
  out <- as.integer(out-nchar(settings$coicop.prefix)) # subtract prefix
  out[id==allid] <- 1L # set highest level to 1

  # return output to console:
  return(out)
  
}

# get labels:
label <- function(id, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$coicop.prefix)) settings$coicop.prefix <- getOption("hicp.coicop.prefix")
  if(is.null(settings$all.items.code)) settings$all.items.code <- getOption("hicp.all.items.code")
  
  # input checks:
  check.char(x=id, min.len=0)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.prefix, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$all.items.code, min.len=1, max.len=1, na.ok=FALSE)
  
  # set dictionary:
  dict <- dictionary(x="ALL", which="LABEL", settings=settings)
  if(is.null(names(settings$all.items.code))){
    allid <- "All-items"
  }else{
    allid <- names(settings$all.items.code)
  }
  names(allid) <- settings$all.items.code
  dict <- c(allid, dict)
  res <- as.character(dict[match(x=id, table=names(dict))])
  return(res)
  
}

# get composition of special aggregate:
spec.agg <- function(id=NULL, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$coicop.prefix)) settings$coicop.prefix <- getOption("hicp.coicop.prefix")
  
  # input checks:
  check.char(x=id, min.len=0, null.ok=TRUE)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.prefix, min.len=1, max.len=1, na.ok=FALSE)
  
  # special aggregate compositions:
  res <- dictionary(x="SA", which="DEF", settings=settings)
  if(!is.null(id)) res <- res[names(res)%in%id]
  return(res)
  
}

# END
