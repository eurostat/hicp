# START

# Title:  Download HICP data
# Author: Sebastian Weinand
# Date:   19 January 2026

# list available HICP datasets:
datasets <- function(pattern="^prc_hicp", ...){
  
  # input checks:
  check.char(x=pattern, max.len=1)
  
  # internal function defaults:
  defaults <- list("mode"="txt")
  
  # overwrite internal defaults to avoid duplicated input:
  dots <- list(...)
  if(any(names(dots)%in%names(defaults))){
    defaults[names(defaults)%in%names(dots)] <- NULL
  }

  # download table of contents of all datasets:
  tmp <- data.table::as.data.table(
    do.call(what=restatapi::get_eurostat_toc, args=c(defaults, dots))
  )

  # subset to hicp datasets:
  if(is.null(tmp)){
    out <- NULL
  }else{
    out <- tmp[grepl(pattern=pattern, x=tmp$code, ignore.case=TRUE),]
  }

  # return output:
  return(out)

}

# show available filters for a dataset:
datafilters <- function(id, ...){

  # input checks:
  check.char(x=id)
  
  # further function defaults:
  dots <- list(...)

  # download data set definitions:
  out <- data.table::as.data.table(
    do.call(what=restatapi::get_eurostat_dsd, args=c(list("id"=id), dots))
  )
  
  # return output:
  return(out)

}

# download dataset:
data <- function(id, filters=list(), date.range=NULL, flags=FALSE, ...){

  # input checks:
  check.char(x=id)
  check.char(x=date.range, min.len=1, max.len=2, null.ok=TRUE)
  check.log(x=flags, min.len=1, max.len=1, na.ok=FALSE)
  if(!is.list(filters)) stop("Non-valid input for filters -> only lists allowed")

  # set time filters in query:
  if(all(is.na(date.range))) date.range <- NULL
  if(!is.null(date.range) && length(date.range)>0){
    if(is.na(date.range[1])) date.range[1] <- "<"
    if(is.na(date.range[2])) date.range[2] <- "<"
    if("<"%in%date.range){
      date.range <- paste(date.range, collapse="")
    }else{
      date.range <- paste(date.range, collapse=":")
    }
  }

  # set filter on key:
  if(length(filters)<1L) filters <- NULL

  # internal function defaults:
  defaults <- list("stringsAsFactors"=FALSE, "mode"="csv")
  
  # overwrite internal defaults to avoid duplicated input:
  dots <- list(...)
  if(any(names(dots)%in%names(defaults))){
    defaults[names(defaults)%in%names(dots)] <- NULL
  }
  
  # gather function arguments:
  fargs <- c(
    list("id"=id, "filters"=filters, "date_filter"=date.range, "keep_flags"=flags), 
    defaults, 
    dots)
  
  # get data:
  dt <- data.table::as.data.table(
    do.call(what=restatapi::get_eurostat_data, args=fargs)
  )

  # set missing flags to NA:
  if(flags) dt[flags=="", "flags":=NA_character_]

  # return output:
  return(dt)

}

# END
