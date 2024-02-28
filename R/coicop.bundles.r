# START

# Title:    COICOP bundles
# Author:   Sebastian Weinand
# Date:     5 February 2024

# eurobase or matis bundle codes and their components:
coicop.bundles <- list(
  "0531_0532" = c("0531", "0532"),
  "0531_2" = c("0531", "0532"),
  "0612_0613" = c("0612", "0613"),
  "0612_3" = c("0612", "0613"),
  "0621_0623" = c("0621", "0623"),
  "0621_3" = c("0621", "0623"),
  "0712-0714" = c("0712", "0713", "0714"),
  "0712_34" = c("0712", "0713", "0714"),
  "08X" = c("082", "083"),
  "082_083" = c("082", "083"),
  "0921_0922" = c("0921", "0922"),
  "0921_2" = c("0921", "0922"),
  "0934_0935" = c("0934", "0935"),
  "0934_5" = c("0934", "0935"),
  "0953_0954" = c("0953", "0954"),
  "0953_4" = c("0953", "0954"),
  "1212_1213" = c("1212", "1213"),
  "1212_3" = c("1212", "1213")
)

# check if coicop id is bundle:
is.bundle <- function(id){

  # input checks:
  check.char(x=id, min.len=0, max.len=Inf)

  # output:
  return(id%in%names(coicop.bundles))

}

# resolve coicop bundles:
unbundle <- function(id){

  # input checks:
  check.char(x=id)

  # match id to dictionary:
  out <- coicop.bundles[match(x=id, table=names(coicop.bundles))]
  names(out) <- id
  idx <- sapply(X=out, FUN=is.null)
  out[idx] <- id[idx]
  out <- stats::setNames(unlist(x=out, use.names=FALSE), rep(id, lengths(out)))
  return(out)

}

# keep bundle code or its components:
keep.bundle <- function(id){

  # flag if the coicop bundle or its components should be kept,
  # if both are present. if all components are present, they
  # are kept, while the bundle code is dropped. otherwise, the
  # bundle code is kept, while its components are dropped.

  # input checks:
  check.char(x=id, min.len=0, max.len=Inf)

  # define output:
  res <- rep(TRUE, length(id))

  # flag bundles:
  bdl.flag <- is.bundle(id)

  # if any bundles present:
  if(any(bdl.flag, na.rm=TRUE)){

    # bundles:
    bdls <- id[bdl.flag]

    # loop over bundles:
    for(j in seq_along(bdls)){

      # unbundle bundle ids:
      bdls.clean <- unbundle(bdls[j])

      if(all(bdls.clean%in%id[!bdl.flag], na.rm=TRUE)){
        res[id%in%bdls[j]] <- FALSE
      }else{
        res[id%in%bdls.clean] <- FALSE
      }

    }

  }

  # return output to console:
  return(res)

}

# END
