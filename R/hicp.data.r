# START

# Title:  Download HICP data
# Author: Sebastian Weinand
# Date:   2023-08-09

# list available HICP datasets:
hicp.datasets <- function(){

  # download table of contents of all datasets:
  tmp <- as.data.table(
    restatapi::get_eurostat_toc(mode="txt", lang="en", verbose=FALSE)
  )

  # subset to hicp datasets:
  if(is.null(tmp)){
    out <- NULL
  }else{
    out <- tmp[grepl(pattern="^prc_hicp", x=code, ignore.case=TRUE, )]
  }

  # return output:
  return(out)

}

# show available filters for a dataset:
hicp.datafilters <- function(id){

  # input checks:
  .check.char(x=id)

  out <- as.data.table(
    restatapi::get_eurostat_dsd(
      id=id,
      lang="en",
      verbose=FALSE))

  return(out)

}

# download dataset:
hicp.dataimport <- function(id, filters=list(), date.range=NULL, flags=FALSE){

  # input checks:
  .check.char(x=id)
  .check.char(x=date.range, min.len=1, max.len=2, null.ok=TRUE)
  .check.log(x=flags, min.len=1, max.len=1, na.ok=FALSE)
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
  dt <- as.data.table(
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

# # download price indices:
# get.prices <- function(unit="I15", geo=NULL, coicop=NULL, date_range=NULL, flags=FALSE){
#
#   # set time filters in query:
#   if(is.null(date_range) || length(date_range)<1) date_range <- c(NA, NA)
#   t0 <- date_range[1]
#   t1 <- date_range[2]
#
#   # query monthly price indices:
#   cat("Downloading data...\r")
#   f <- list("freq"="M", "unit"=unit, "coicop"=coicop, "geo"=geo)
#   dt <- eurostat.api::get.data(
#     id="prc_hicp_midx",
#     filter=f,
#     date_range=c(t0,t1),
#     flags=flags,
#     compressed=TRUE)
#
#   # manipulate data:
#   dt[, "year" := as.integer(substr(TIME_PERIOD, 1, 4))]
#   dt[, "TIME_PERIOD" := as.Date(paste0(TIME_PERIOD, "-01"), format="%Y-%m-%d")]
#   dt[, c("DATAFLOW", "LAST UPDATE", "freq") := NULL]
#   if("OBS_FLAG"%in%names(dt)) dt[OBS_FLAG=="", "OBS_FLAG":=NA_character_]
#   setnames(
#     x=dt,
#     old=c("TIME_PERIOD", "OBS_VALUE", "OBS_FLAG"),
#     new=c("time", "value", "value_flag"),
#     skip_absent=TRUE)
#
#   # set column ordering:
#   co <- c("unit","geo","coicop","year","time","value","value_flag")
#   setcolorder(x=dt, neworder=co[co%in%names(dt)])
#
#   # set data key and column order:
#   setkeyv(x=dt, cols=c("unit","geo","coicop","time"))
#
#   # return output:
#   return(dt)
#
# }
#
# # download item weights:
# get.itemweights <- function(geo=NULL, coicop=NULL, date_range=NULL, flags=FALSE){
#
#   # set time filters in query:
#   if(is.null(date_range) || length(date_range)<1) date_range <- c(NA, NA)
#   t0 <- substr(date_range[1], 1, 4)
#   t1 <- substr(date_range[2], 1, 4)
#
#   # query annual item weights:
#   cat("Downloading data...\r")
#   f <- list("freq"="A", "coicop"=coicop, "geo"=geo)
#   dt <- eurostat.api::get.data(
#     id="prc_hicp_inw",
#     filter=f,
#     date_range=c(t0,t1),
#     flags=flags,
#     compressed=TRUE)
#
#   # manipulate data:
#   dt[, "TIME_PERIOD" := as.integer(TIME_PERIOD)]
#   dt[, c("DATAFLOW", "LAST UPDATE", "freq") := NULL]
#   if("OBS_FLAG"%in%names(dt)) dt[OBS_FLAG=="", "OBS_FLAG":=NA_character_]
#   setnames(
#     x=dt,
#     old=c("TIME_PERIOD", "OBS_VALUE", "OBS_FLAG"),
#     new=c("year", "value", "value_flag"),
#     skip_absent=TRUE)
#
#   # drop observations with missing or zero item weights:
#   dt <- dt[!is.na(value) & value>0, ]
#
#   # set column ordering:
#   co <- c("geo","coicop","year","value","value_flag")
#   setcolorder(x=dt, neworder=co[co%in%names(dt)])
#
#   # set data key and column order:
#   setkeyv(x=dt, cols=c("geo","coicop","year"))
#
#   # return output:
#   return(dt)
#
# }
#
# # download country weights:
# get.countryweights <- function(statinfo=NULL, geo=NULL, date_range=NULL, flags=FALSE){
#
#   # set time filters in query:
#   if(is.null(date_range) || length(date_range)<1) date_range <- c(NA, NA)
#   t0 <- substr(date_range[1], 1, 4)
#   t1 <- substr(date_range[2], 1, 4)
#
#   # query annual country weights:
#   cat("Downloading data...\r")
#   f <- list("freq"="A", "statinfo"=statinfo, "geo"=geo)
#   dt <- eurostat.api::get.data(
#     id="prc_hicp_cow",
#     filter=f,
#     date_range=c(t0,t1),
#     flags=flags,
#     compressed=TRUE)
#
#   # manipulate data:
#   dt[, "TIME_PERIOD" := as.integer(TIME_PERIOD)]
#   dt[, c("DATAFLOW", "LAST UPDATE", "freq") := NULL]
#   if("OBS_FLAG"%in%names(dt)) dt[OBS_FLAG=="", "OBS_FLAG":=NA_character_]
#   setnames(
#     x=dt,
#     old=c("TIME_PERIOD", "OBS_VALUE", "OBS_FLAG"),
#     new=c("year", "value", "value_flag"),
#     skip_absent=TRUE)
#
#   # drop observations with missing or zero item weights:
#   dt <- dt[!is.na(value) & value>0, ]
#
#   # set column ordering:
#   co <- c("statinfo","geo","year","value","value_flag")
#   setcolorder(x=dt, neworder=co[co%in%names(dt)])
#
#   # set data key and column order:
#   setkeyv(x=dt, cols=c("statinfo","geo","year"))
#
#   # return output:
#   return(dt)
#
# }

# END
