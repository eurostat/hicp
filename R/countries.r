# START

# Title:    European aggregates and countries
# Author:   Sebastian Weinand
# Date:     16 November 2026

# all countries with HICP data published:
countries <- function() return(dict.countries)

# composition of European aggregates:
european.agg <- function(id, t=Sys.Date()){
  
  # input checks:
  check.char(x=id, min.len=1, max.len=1, na.ok=FALSE)
  check.date(x=t, min.len=1, max.len=1, na.ok=FALSE)
  
  # match input:
  id <- match.arg(arg=id, choices=c("EA","EU","EEA"))
  
  # composition of European aggregate at specific date:
  from <- to <- NULL # avoid visible binding note
  res <- dict.country.aggs[[id]][t>=from & t<=to,]
  res <- as.character(res$country)
  #res <- dict.countries[names(dict.countries)%in%res$country]
  return(res)
  
}

# non-exported helper function:
is.agg.member <- function(id, t=Sys.Date(), agg){
  
  # input checks:
  check.char(x=id, min.len=0)
  check.date(x=t, min.len=0, na.ok=FALSE)
  if(length(id)>1L & length(t)>1L) check.lengths(x=id, y=t)
  
  # transform non-matches of non-equi join into logical vector:
  res <- agg[data.table("country"=id, "time"=t), on=c("country", "from<=time", "to>=time")]
  res <- as.logical(!is.na(res$code))
  return(res)
  
}

# check if part of euro area:
is.ea <- function(id, t=Sys.Date()){
  
  is.agg.member(id=id, t=t, agg=dict.country.aggs[["EA"]])
  
}

# check if part of EU:
is.eu <- function(id, t=Sys.Date()){
  
  is.agg.member(id=id, t=t, agg=dict.country.aggs[["EU"]])
  
}

# check if part of EEA:
is.eea <- function(id, t=Sys.Date()){
  
  is.agg.member(id=id, t=t, agg=dict.country.aggs[["EEA"]])
  
}

# END