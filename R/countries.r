# START

# Title:    European aggregates and countries
# Author:   Sebastian Weinand
# Date:     26 January 2026

# countries with HICP data published:
countries <- function(group="All", t=Sys.Date()){
  
  # input checks:
  check.char(x=group, min.len=1, max.len=1, na.ok=FALSE)
  check.date(x=t, min.len=1, max.len=1, na.ok=FALSE)
  
  # match input:
  group <- match.arg(arg=group, choices=c("All","EA","EU","EEA"))
  
  # set output to all countries or subset to specific group:
  res <- dict.countries
  if(group=="EA") res <- res[is.ea(id=names(res), t=t)]
  if(group=="EU") res <- res[is.eu(id=names(res), t=t)]
  if(group=="EEA") res <- res[is.eea(id=names(res), t=t)]
  
  # return output:
  return(res)
  
}

# non-exported helper function:
is.agg.member <- function(id, t=Sys.Date(), agg){
  
  # input checks:
  check.char(x=id, min.len=0)
  check.date(x=t, na.ok=FALSE)
  if(length(id)>1L & length(t)>1L) check.lengths(x=id, y=t)
  
  if(length(id)>0L){
    
    # match by time intervals:
    res <- data.table::foverlaps(
      x=data.table::data.table("country"=id, "from"=as.Date(t), "to"=as.Date(t)),
      y=agg,
      by.x=c("country","from","to"),
      by.y=c("country","from","to"),
      type="within")
    
  }else{
    
    # empty data.table:
    res <- data.table::data.table("from"=character())
    
  }
  
  # logical vector without names:
  res <- as.logical(!is.na(res$from))
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