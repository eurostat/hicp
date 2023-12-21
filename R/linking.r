# START

# Title:  Linking-in new index series
# Author: Sebastian Weinand
# Date:   2023-09-05

# link-in new index series:
link <- function(x, x.new, t, t.overlap=NULL){

  # input checks:
  .check.num(x=x)
  .check.num(x=x.new)
  .check.date(x=t, na.ok=FALSE)
  .check.lengths(x=x, y=t)
  .check.lengths(x=x, y=x.new)
  .check.char(x=t.overlap, null.ok=TRUE, na.ok=FALSE)

  # check:
  if(is.null(t.overlap)) check <- TRUE else check <- FALSE

  # last value of current index:
  t.last <- max(t[!is.na(x)])

  # time period intersections:
  t.avail <- as.Date(intersect(as.character(t[!is.na(x)]), as.character(t[!is.na(x.new)])))

  # year period intersections:
  y.avail <- tapply(X=t.avail, INDEX=format(t.avail, "%Y"), FUN=length)
  y.avail <- names(y.avail[y.avail==12L])
  y.avail <- t.avail[format(t.avail, "%Y")%in%y.avail]

  # set overlap period if not provided:
  if(is.null(t.overlap) & length(t.avail)>0){
    t.overlap <- unique(c(format(y.avail, "%Y"), format(t.avail, "%Y-%m")))
  }

  # set each overlap period:
  t.use <- vector(mode="list", length(t.overlap))
  for(j in seq_along(t.overlap)){
    t.use[[j]] <- c(y.avail[format(y.avail, "%Y")%in%t.overlap[j]], t.avail[format(t.avail, "%Y-%m")%in%t.overlap[j]])
  }

  # missing overlap periods:
  keep <- lengths(t.use)>0
  if(all(!keep)) message("No overlap period(s) found.")
  if(!all(!keep) & any(!keep)) message("Some overlap period(s) not found.")

  # output container:
  out <- matrix(data=x, byrow=FALSE, ncol=max(1, length(t.overlap)), nrow=length(t), dimnames=list(NULL, t.overlap))

  # loop:
  for(j in seq_along(t.overlap)[keep]){

    # linked indices:
    x.new.linked <- x.new[t>max(t.use[[j]], na.rm=TRUE)]*mean(x[t%in%t.use[[j]]], na.rm=TRUE)/mean(x.new[t%in%t.use[[j]]], na.rm=TRUE)

    # old and linked index series:
    out[,j] <- c(x[t<=t.last], x.new.linked[t[t>max(t.use[[j]], na.rm=TRUE)]>t.last])

  }

  # return output to console:
  if(ncol(out)<=1L){
    if(!all(!keep) & check) message("Overlap period ", t.overlap, " used.")
    res <- as.vector(out[,1])
  }else{
    res <- as.data.table(out)
  }

  return(res)

}

# compute level-shift factor:
lsf <- function(x, x.new, t, t.overlap=NULL){

  # input checks:
  .check.num(x=x)
  .check.num(x=x.new)
  .check.date(x=t, na.ok=FALSE)
  .check.lengths(x=x, y=t)
  .check.lengths(x=x, y=x.new)
  .check.char(x=t.overlap, null.ok=TRUE, na.ok=FALSE)

  # time period intersections:
  t.avail <- as.Date(intersect(as.character(t[!is.na(x)]), as.character(t[!is.na(x.new)])))

  # year period intersections:
  y.avail <- tapply(X=t.avail, INDEX=format(t.avail, "%Y"), FUN=length)
  y.avail <- names(y.avail[y.avail==12L])
  y.avail <- t.avail[format(t.avail, "%Y")%in%y.avail]

  # last december index values available in both index series:
  t12 <- t.avail[format(t.avail, "%m")=="12"]
  if(length(t12)>0) t12 <- max(t12, na.rm=TRUE)

  # set overlap period if not provided:
  if(is.null(t.overlap) & length(t.avail)>0){
    t.overlap <- unique(c(format(y.avail, "%Y"), format(t.avail, "%Y-%m")))
  }

  # set each overlap period:
  t.use <- vector(mode="list", length(t.overlap))
  for(j in seq_along(t.overlap)){
    t.use[[j]] <- c(y.avail[format(y.avail, "%Y")%in%t.overlap[j]], t.avail[format(t.avail, "%Y-%m")%in%t.overlap[j]])
  }

  # missing overlap periods:
  keep <- lengths(t.use)>0

  # output container:
  out <- rep(x=NA_real_, times=max(1, length(t.overlap)))
  names(out) <- t.overlap

  # compute level-shift factors:
  for(j in seq_along(t.overlap)[keep]){
    out[j] <- (x.new[t%in%t12] / mean(x.new[t%in%t.use[[j]]], na.rm=TRUE)) / (x[t%in%t12] / mean(x[t%in%t.use[[j]]], na.rm=TRUE))
  }

  # return output to console:
  return(out)

}

# END
