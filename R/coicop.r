# START

# Title:    COICOP codes and special aggregates
# Author:   Sebastian Weinand
# Date:     15 November 2026

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
  out <- data.table::`%chin%`(x=id, table=dict)
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
  out <- data.table::`%chin%`(x=id, table=dict)
  return(out)
  
}

# flag special aggregate codes:
is.special.agg <- function(id, settings=list()){

  # set default settings if missing:
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$coicop.prefix)) settings$coicop.prefix <- getOption("hicp.coicop.prefix")
  
  # input checks:
  check.char(x=id, min.len=0)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.prefix, min.len=1, max.len=1, na.ok=FALSE)

  # set dictionary and flag valid codes:
  dict <- dictionary(x=c("SA","AP"), which="CODE", settings=settings)
  out <- data.table::`%chin%`(x=id, table=dict)
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
  
  # keep only unique elements:
  id0 <- unique(id)
  
  # set to NA if no valid coicop code, no bundle code and not all-items code:
  bdl <- hicp::is.bundle(id=id0, settings=settings)
  id0[!hicp::is.coicop(id0, settings=settings) & id0!=allid & !bdl] <- NA_character_
  
  # derive levels:
  out <- nchar(id0)
  
  # resolve level of coicop bundles correctly:
  if(any(bdl)){
    dict <- dictionary(x="BDL", which="DEF", settings=settings)
    lookup <- nchar(sapply(X=dict, "[[", 1))
    idx <- match(x=id0, table=names(lookup))
    out <- ifelse(test=is.na(idx), yes=out, no=lookup[idx])
  }
  
  out <- as.integer(out-nchar(settings$coicop.prefix)) # subtract prefix
  out[id0==allid] <- 1L # set highest level to 1
  out <- out[data.table::chmatch(x=id, table=id0)]

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
  dict <- dictionary(x=c("COICOP","BDL","SA","AP"), which="LABEL", settings=settings)
  if(is.null(names(settings$all.items.code))){
    allid <- "All-items"
  }else{
    allid <- names(settings$all.items.code)
  }
  names(allid) <- settings$all.items.code
  dict <- c(allid, dict)
  res <- as.character(dict[data.table::chmatch(x=id, table=names(dict))])
  return(res)
  
}

# get composition of special aggregate:
special.agg <- function(id=NULL, settings=list()){
  
  # set default settings if missing:
  if(is.null(settings$chatty)) settings$chatty <- getOption("hicp.chatty")
  if(is.null(settings$coicop.version)) settings$coicop.version <- getOption("hicp.coicop.version")
  if(is.null(settings$coicop.prefix)) settings$coicop.prefix <- getOption("hicp.coicop.prefix")
  
  # input checks:
  check.char(x=id, min.len=0, null.ok=TRUE)
  check.log(x=settings$chatty, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.version, min.len=1, max.len=1, na.ok=FALSE)
  check.char(x=settings$coicop.prefix, min.len=1, max.len=1, na.ok=FALSE)
  
  # special aggregate compositions:
  res <- dictionary(x="SA", which="DEF", settings=settings)
  if(!is.null(id)){
    idx <- match(x=id, table=names(res))
    if(any(is.na(idx)) && settings$chatty){
      nm <- paste(unique(id[is.na(idx)]), collapse=", ")
      warning(paste("No definitions found for the following elements in id:", nm), call.=FALSE)
    }
    res <- res[idx]
    names(res) <- id
  }
  return(res)
  
}

# END
