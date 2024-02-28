# START

# Title:  Download HICP data
# Author: Sebastian Weinand
# Date:   5 February 2024

# list available HICP datasets:
hicp.datasets <- function(){

  # download table of contents of all datasets:
  tmp <- data.table::as.data.table(
    restatapi::get_eurostat_toc(mode="txt", lang="en", verbose=FALSE)
  )

  # subset to hicp datasets:
  if(is.null(tmp)){
    out <- NULL
  }else{
    out <- tmp[grepl(pattern="^prc_hicp", x=tmp$code, ignore.case=TRUE),]
  }

  # return output:
  return(out)

}

# show available filters for a dataset:
hicp.datafilters <- function(id){

  # input checks:
  check.char(x=id)

  out <- data.table::as.data.table(
    restatapi::get_eurostat_dsd(
      id=id,
      lang="en",
      verbose=FALSE))

  return(out)

}

# download dataset:
hicp.dataimport <- function(id, filters=list(), date.range=NULL, flags=FALSE){

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

  # get data:
  dt <- data.table::as.data.table(
    restatapi::get_eurostat_data(
      id=id,
      filters=filters,
      date_filter=date.range,
      keep_flags=flags,
      stringsAsFactors=FALSE,
      mode="csv"))

  # set missing flags to NA:
  if(flags) dt[flags=="", "flags":=NA_character_]

  # return output:
  return(dt)

}

# END
