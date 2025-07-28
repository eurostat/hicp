# START

# Title:  Package options
# Author: Sebastian Weinand
# Date:   22 July 2025

.onLoad <- function(libname, pkgname){
  options(hicp.chatty=TRUE) # print package internal messages and warnings
  options(hicp.coicop.version="ecoicop.hicp") # set coicop version used by functions
  options(hicp.all.items.code="00") # set code of all-items index
  options(hicp.coicop.bundles=coicop.bundles) # coicop bundle dictionary
}

.onAttach <- function(libname, pkgname){
  options(hicp.chatty=TRUE)
  options(hicp.coicop.version="ecoicop.hicp")
  options(hicp.all.items.code="00")
  options(hicp.coicop.bundles=coicop.bundles)
}

# END
