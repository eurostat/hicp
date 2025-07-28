# START

# Title:  Download HICP data
# Author: Sebastian Weinand
# Date:   9 July 2025

# list available HICP datasets:
datasets <- function(pattern="^prc_hicp", ...){
  
  # input checks:
  check.char(x=pattern, max.len=1)
  
  # input defaults for function:
  defaults <- list(mode="txt")
  
  # get further arguments and adjust if needed:
  dots <- list(...)
  if(names(defaults)%in%names(dots)){
    dots[names(dots)%in%names(defaults)] <- NULL
  }

  # download table of contents of all datasets:
  tmp <- data.table::as.data.table(
    do.call(restatapi::get_eurostat_toc, args=c(defaults, dots))
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

  # input defaults for function:
  defaults <- list(id=id)
  
  # get further arguments and adjust if needed:
  dots <- list(...)
  if(names(defaults)%in%names(dots)){
    dots[names(dots)%in%names(defaults)] <- NULL
  }
  
  # download data set definitions:
  out <- data.table::as.data.table(
    do.call(restatapi::get_eurostat_dsd, args=c(defaults, dots))
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
  if(length(filters)<=0) filters <- NULL

  # input defaults for function:
  defaults <- list(id=id,
                   filters=filters,
                   date_filter=date.range,
                   keep_flags=flags,
                   stringsAsFactors=FALSE,
                   mode="csv")
  
  # get further arguments and adjust if needed:
  dots <- list(...)
  if(any(names(defaults)%in%names(dots))){
    dots[names(dots)%in%names(defaults)] <- NULL
  }
  
  # get data:
  dt <- data.table::as.data.table(
    do.call(restatapi::get_eurostat_data, args=c(defaults, dots))
  )

  # set missing flags to NA:
  if(flags) dt[flags=="", "flags":=NA_character_]

  # return output:
  return(dt)

}

# END
