# START

# Title:  Package options
# Author: Sebastian Weinand
# Date:   28 January 2026

.onLoad <- function(libname, pkgname){
  options(hicp.chatty=TRUE) # print package internal messages and warnings
  options(hicp.coicop.version="ecoicop2.hicp") # set COICOP version used by functions
  options(hicp.coicop.prefix="CP") # prefix of COICOP codes
  options(hicp.all.items.code="TOTAL") # set code of all-items index
}

.onAttach <- function(libname, pkgname){
  options(hicp.chatty=TRUE)
  options(hicp.coicop.version="ecoicop2.hicp")
  options(hicp.coicop.prefix="CP")
  options(hicp.all.items.code="TOTAL")
}

# END
