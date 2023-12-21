# START

# Title:    COICOP relatives
# Author:   Sebastian Weinand
# Date:     8 August 2023

# check validity of coicop code:
is.coicop <- function(id, unbundle=TRUE){

  # input checks:
  .check.char(x=id)
  .check.log(x=unbundle, min.len=1, max.len=1, na.ok=FALSE)

  rgx <- "(0[0-9]{1}|1[0-2]{1})([1-9]{1}|[1-9]{1}[0-9]{1,2})?"
  # if(!is.null(prefix)) rgx <- paste0("(", prefix, ")?", rgx)
  rgx <- paste0("^", rgx, "$")
  if(unbundle) check.bdl <- hicp::is.bundle(id) else check.bdl <- FALSE
  out <- grepl(pattern=rgx, x=id) | check.bdl
  return(out)

}

# get coicop level:
level <- function(id, unbundle=TRUE, label=FALSE){

  # input checks:
  .check.char(x=id)
  .check.log(x=label, min.len=1, max.len=1, na.ok=FALSE)

  # set to NA if no valid coicop code:
  id[!hicp::is.coicop(id, unbundle=unbundle)] <- NA_character_

  # resolve levels of coicop bundles properly:
  lvl <- nchar(sapply(X=strsplit(id,"\\_|-"), "[[", 1L))

  # set highest level to 1:
  lvl[id=="00"] <- 1

  # resolve labels:
  if(label){
    lvl <- c("total","division","group","class","subclass")[match(x=lvl, table=1:5)]
  }

  # return output to console:
  return(lvl)

}

# derive coicop parents:
parent <- function(id, flag=TRUE, unbundle=TRUE, direct=FALSE){

  # @Args:
  # id            character vector of coicop ids
  # flag          flag if parent available (TRUE) or return
  #               parent id (FALSE)
  # unbundle      logical indicating if coicop bundles (e.g.
  #               08X, 0531_2) should be taken into account or not
  # direct        logical indicating if only direct parents
  #               should be flagged TRUE (e.g. 031->03) or
  #               also highler-level parents (e.g. 0311->03) if
  #               the parent in between are missing

  # input checks:
  .check.char(x=id)
  .check.log(x=flag, min.len=1, max.len=1, na.ok=FALSE)
  .check.log(x=unbundle, min.len=1, max.len=1, na.ok=FALSE)
  .check.log(x=direct, min.len=1, max.len=1, na.ok=FALSE)

  # set non-valid coicop codes to NA:
  id[!hicp::is.coicop(id, unbundle=unbundle)] <- NA_character_

  if(all(is.na(id))){

    out <- id

  }else{

    # subset to unique ids:
    id.tab <- unique(id[!is.na(id)])

    # resolve coicop bundles for further processing:
    if(unbundle){
      id.tab <- hicp::unbundle(id=id.tab)
    }else{
      names(id.tab) <- id.tab
      id.tab[is.bundle(id.tab)] <- NA
    }

    # drop NAs:
    id.tab <- id.tab[!is.na(id.tab)]

    # coicop levels:
    n <- hicp::level(id=id.tab, unbundle=unbundle)

    # derive parents:
    out <- rep(NA_character_, length(id.tab))
    for(i in seq_along(id.tab)){

      # subset to relevant codes:
      id.sub <- id.tab[n<n[i]]

      # flag direct or all parents:
      if(direct){
        idx <- id.sub%in%substr(x=id.tab[i], start=1, stop=n[i]-1)
      }else{
        idx <- startsWith(x=id.tab[i], prefix=id.sub)
      }

      # resolve bundle codes:
      if(any(idx, na.rm=TRUE)){
        res <- id.sub[idx][which.max(nchar(id.sub[idx]))]
        if(res%in%names(id.tab)){
          out[i] <- res
        }else{
          out[i] <- names(res)
        }
      }

    }

    # set all-items hicp as parent of divisions:
    if(any(id.tab=="00", na.rm=TRUE)){
      out[id.tab!="00" & n==2L] <- "00"
      if(!direct) out[id.tab!="00" & is.na(out)] <- "00"
    }

    # match to initial ordering:
    out <- out[match(x=id, table=names(id.tab))]

  }

  # flag if parent available or not:
  if(flag){
    out <- !is.na(out)
    out[is.na(id)] <- NA
  }

  # return output:
  return(out)

}

# derive coicop children:
child <- function(id, flag=TRUE, unbundle=TRUE, direct=FALSE){

  # @Args:
  # id            character vector of coicop ids
  # flag          flag if children available (TRUE) or return
  #               children ids (FALSE)
  # unbundle      logical indicating if coicop bundles (e.g.
  #               08X, 0531_2) should be taken into account or not
  # direct        logical indicating if only direct children
  #               should be flagged TRUE (e.g. 03->031) or
  #               also lower-level childs (e.g. 03->0311) if
  #               the childs in between are missing

  # input checks:
  .check.char(x=id)
  .check.log(x=flag, min.len=1, max.len=1, na.ok=FALSE)
  .check.log(x=unbundle, min.len=1, max.len=1, na.ok=FALSE)
  .check.log(x=direct, min.len=1, max.len=1, na.ok=FALSE)

  # set non-valid coicop codes to NA:
  id[!hicp::is.coicop(id, unbundle=unbundle)] <- NA_character_

  if(all(is.na(id))){

    out <- as.list(id)

  }else{

    # subset to unique ids:
    id.tab <- unique(id[!is.na(id)])

    # resolve coicop bundles for further processing:
    if(unbundle){
      id.tab <- hicp::unbundle(id=id.tab)
    }else{
      names(id.tab) <- id.tab
      id.tab[is.bundle(id.tab)] <- NA
    }

    # drop NAs:
    id.tab <- id.tab[!is.na(id.tab)]

    # coicop levels:
    n <- hicp::level(id=id.tab, unbundle=unbundle)

    # derive children:
    out <- vector(mode="list", length=length(id.tab))
    for(i in seq_along(id.tab)){

      # direct or all children:
      if(direct){
        id.sub <- id.tab[-i][n[-i]==n[i]+1]
      }else{
        id.sub <- id.tab[-i][n[-i]>n[i]]
      }

      # flag children:
      idx <- startsWith(x=id.sub, prefix=id.tab[i])

      # resolve bundle codes:
      if(any(idx, na.rm=TRUE)){
        res <- id.sub[idx]
        if(all(res%in%names(id.tab))){
          out[[i]] <- as.vector(res)
        }else{
          out[[i]] <- unique(names(res))
        }
      }

    }

    # allowed children below top-level:
    if(direct){
      achld <- formatC(x=1:12, width=2, format="d", flag="0")
    }else{
      achld <- id.tab[id.tab!="00"]
      if(length(achld)>0) achld <- achld[hicp::is.coicop(id=achld, unbundle=unbundle)]
    }

    # set children below top-level:
    if("00"%in%id.tab & any(achld%in%id.tab, na.rm=TRUE)){
      out[[which(id.tab=="00" & any(id.tab%in%achld, na.rm=TRUE))]] <- unique(names(id.tab[id.tab%in%achld]))
    }

    # match to initial ordering:
    out <- out[match(x=id, table=names(id.tab))]

  }

  # replace nulls by NA or empty character:
  out[is.na(id)] <- NA_character_
  out <- lapply(X=out, FUN=function(z) if(is.null(z)){character()}else{z})

  # flag if parent available or not:
  if(flag){
    out <- lengths(out)>0
    out[is.na(id)] <- NA
  }

  # return output:
  return(out)

}

# END
