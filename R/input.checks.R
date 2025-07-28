# START

# Title:  Input checks
# Author: Sebastian Weinand
# Date:   29 April 2025

# check character and factor inputs:
check.char <- function(x, min.len=1, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, na.ok=TRUE){

  input <- deparse(substitute(x))
  msg_prefix <- paste("Non-valid input for", input, "->")
  if(abs(max.len-min.len)<1e-6) one.len <- TRUE else one.len <- FALSE

  if(missing(x) & !miss.ok) stop(paste(msg_prefix, "missing input"), call. = FALSE)
  if(missing(x) & miss.ok) return(invisible())

  if(!(null.ok && is.null(x))){
    if(!(is.vector(x=x, mode="character") | is.factor(x=x))) stop(paste(msg_prefix, "only character vectors or factors allowed"), call. = FALSE)
    if(one.len & length(x)!=max.len) stop(paste(msg_prefix, "must contain exactly one element"), call. = FALSE)
    if(length(x)<min.len) stop(paste(msg_prefix, "length must be greater or equal", min.len), call. = FALSE)
    if(length(x)>max.len) stop(paste(msg_prefix, "length must be smaller or equal", max.len), call. = FALSE)
    if(any(is.na(x)) & !na.ok) stop(paste(msg_prefix, "NAs not allowed"), call.=FALSE)
  }

}

# check logical inputs:
check.log <- function(x, min.len=1, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, na.ok=TRUE){

  input <- deparse(substitute(x))
  msg_prefix <- paste("Non-valid input for", input, "->")
  if(abs(max.len-min.len)<1e-6) one.len <- TRUE else one.len <- FALSE

  if(missing(x) & !miss.ok) stop(paste(msg_prefix, "missing input"), call. = FALSE)
  if(missing(x) & miss.ok) return(invisible())

  if(!(null.ok && is.null(x))){
    if(!is.vector(x=x, mode = "logical")) stop(paste(msg_prefix, "only logical allowed"), call. = FALSE)
    if(one.len & length(x)!=max.len) stop(paste(msg_prefix, "must contain exactly one element"), call. = FALSE)
    if(length(x)<min.len) stop(paste(msg_prefix, "length must be greater or equal", min.len), call. = FALSE)
    if(length(x)>max.len) stop(paste(msg_prefix, "length must be smaller or equal", max.len), call. = FALSE)
    if(any(is.na(x)) & !na.ok) stop(paste(msg_prefix, "NAs not allowed"), call.=FALSE)
    if(!all(x%in%c(NA, TRUE, FALSE))) stop(paste(msg_prefix, "only TRUE and FALSE allowed"), call. = FALSE)
  }

}

# check numeric inputs:
check.num <- function(x, min.len=1, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, na.ok=TRUE, int=c(-Inf, Inf)){

  input <- deparse(substitute(x))
  msg_prefix <- paste("Non-valid input for", input, "->")
  if(abs(max.len-min.len)<1e-6) one.len <- TRUE else one.len <- FALSE

  if(missing(x) & !miss.ok) stop(paste(msg_prefix, "missing input"), call. = FALSE)
  if(missing(x) & miss.ok) return(invisible())

  if(!(null.ok && is.null(x))){
    if(!is.vector(x=x, mode="numeric")) stop(paste(msg_prefix, "only numeric vectors allowed"), call. = FALSE)
    if(one.len & length(x)!=max.len) stop(paste(msg_prefix, "must contain exactly one element"), call. = FALSE)
    if(length(x)<min.len) stop(paste(msg_prefix, "length must be greater or equal", min.len), call. = FALSE)
    if(length(x)>max.len) stop(paste(msg_prefix, "length must be smaller or equal", max.len), call. = FALSE)
    if(any(is.na(x)) & !na.ok) stop(paste(msg_prefix, "NAs not allowed"), call.=FALSE)
    if(any(x<int[1] | x>int[2], na.rm = TRUE)) stop(paste(msg_prefix, "only values between", paste(int, collapse = " and "), "allowed"), call. = FALSE)
  }

}

# check dates:
check.date <- function(x, min.len=1, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, na.ok=TRUE){

  input <- deparse(substitute(x))
  msg_prefix <- paste("Non-valid input for", input, "->")
  if(abs(max.len-min.len)<1e-6) one.len <- TRUE else one.len <- FALSE

  if(missing(x) & !miss.ok) stop(paste(msg_prefix, "missing input"), call. = FALSE)
  if(missing(x) & miss.ok) return(invisible())

  if(!(null.ok && is.null(x))){
    if(!inherits(x, what="Date")) stop(paste(msg_prefix, "only date vectors allowed"), call. = FALSE)
    if(one.len & length(x)!=max.len) stop(paste(msg_prefix, "must contain exactly one element"), call. = FALSE)
    if(length(x)<min.len) stop(paste(msg_prefix, "length must be greater or equal", min.len), call. = FALSE)
    if(length(x)>max.len) stop(paste(msg_prefix, "length must be smaller or equal", max.len), call. = FALSE)
    if(any(is.na(x)) & !na.ok) stop(paste(msg_prefix, "NAs not allowed"), call.=FALSE)
  }
  
}

# check date vector for chronological ordering, duplicates and gaps:
check.dateseries <- function(x, freq=12L, tol=1e-6, chatty=TRUE){
  
  # @args
  # x       date vector
  # freq    frequency of date vector [12=month, 4=quarter, 1=year] 
  # tol     the tolerance to differentiate between frequencies
  # chatty  print warnings or messages
  
  input <- deparse(substitute(x))
  
  # is the ordering chronological?
  if(chatty && any(diff(x)<0, na.rm=TRUE)){
    warning(paste("Dates in", input, "not in chronological order"), call.=FALSE)
  } 
  
  # extract years:
  y <- data.table::year(x)
  
  # check monhtly frequency:
  if(freq>=12){
    
    # are there duplicated months?
    m <- data.table::month(x)
    if(any(table(y, m)>1, na.rm=TRUE)){
      stop(paste("Non-valid input for", input, "-> duplicated months present"), call.=FALSE)
    }
    
    # are there missing months?
    mdiff <- diff(sort(data.table::yearmon(x)))
    if(chatty && any(mdiff>ceiling(1000*1/12)/1000, na.rm=TRUE)){
      warning(paste("Some months in", input, "are missing"), call.=FALSE)
    }
  }
  
  # check quarterly frequency:
  if(freq>=4 & freq<12){
    
    # are there duplicated months?
    q <- data.table::quarter(x)
    if(any(table(y, q)>1, na.rm=TRUE)){
      stop(paste("Non-valid input for", input, "-> duplicated quarters present"), call.=FALSE)
    }
    
    # are there missing quarters?
    qdiff <- diff(sort(data.table::yearqtr(x)))
    if(chatty && any(qdiff>ceiling(1000*1/4)/1000, na.rm=TRUE)){
      warning(paste("Some quarters in", input, "are missing"), call.=FALSE)
    }
  }
  
  # check annual frequency:
  if(freq<4){
    
    # are there duplicated years?
    if(any(table(y)>1, na.rm=TRUE)){
      stop(paste("Non-valid input for", input, "-> duplicated years present"), call.=FALSE)
    }
    
    # are there missing years?
    ydiff <- diff(sort(data.table::year(x)))
    if(chatty && any(ydiff>ceiling(1000*1/1)/1000, na.rm=TRUE)){
      warning(paste("Some years in", input, "are missing"), call.=FALSE)
    }
  } 
  
}

# check identical length of input vectors:
check.lengths <- function(x, y){

  input_x <- deparse(substitute(x))
  input_y <- deparse(substitute(y))
  msg_prefix <- paste("Non-valid input for", input_x, "or", input_y, "->")

  if(!is.null(x) && !is.null(y)){
    if(length(x) != length(y)) stop(paste(msg_prefix, "vectors must be of equal length"), call. = FALSE)
  }

}

# check if input is list with names:
check.list <- function(x, min.len=1, max.len=Inf, miss.ok=FALSE, null.ok=FALSE, names=TRUE){
  
  input <- deparse(substitute(x))
  msg_prefix <- paste("Non-valid input for", input, "->")
  if(abs(max.len-min.len)<1e-6) one.len <- TRUE else one.len <- FALSE
  
  if(missing(x) & !miss.ok) stop(paste(msg_prefix, "missing input"), call.=FALSE)
  if(missing(x) & miss.ok) return(invisible())
  
  if(!(null.ok && is.null(x))){
    if(!is.list(x)) stop(paste(msg_prefix, "only lists allowed"), call.=FALSE)
    if(one.len & length(x)!=max.len) stop(paste(msg_prefix, "must contain exactly one element"), call. = FALSE)
    if(length(x)<min.len) stop(paste(msg_prefix, "length must be greater or equal", min.len), call. = FALSE)
    if(length(x)>max.len) stop(paste(msg_prefix, "length must be smaller or equal", max.len), call. = FALSE)
    nm <- names(x)
    if(names & length(x)>0 & (is.null(nm) || ""%in%nm)) stop(paste(msg_prefix, "list must have names"), call.=FALSE)
  }
  
}

# END
