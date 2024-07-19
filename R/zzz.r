# START

# Title:  Package options
# Author: Sebastian Weinand
# Date:   18 July 2024

.onLoad <- function(libname, pkgname){
  options(hicp.unbundle=TRUE) # unbundle coicop bundle codes
  options(hicp.coicop.version="ecoicop-hicp") # set coicop version used by functions
  options(hicp.all.items.code="00") # set code of all-items index
}

.onAttach <- function(libname, pkgname){
  options(hicp.unbundle=TRUE) 
  options(hicp.coicop.version="ecoicop-hicp")
  options(hicp.all.items.code="00")
}

# END
