# START

# Title:    Special aggregates
# Author:   Sebastian Weinand
# Date:     21 December 2023

# load packages:
library(xml2)
library(data.table)

# get special aggregates:
doc <- read_xml("http://publications.europa.eu/resource/authority/ed1/ecoicop/B")
agg.urls <- xml_attr(x = xml_find_all(doc, xpath="//skos:member"), attr="resource")
agg.codes <- sapply(X=strsplit(agg.urls, split="/", fixed=TRUE), FUN=function(z) z[length(z)])

# get compositions:
agg.comp <- vector(mode="list", length=length(agg.urls))
for(j in seq_along(agg.comp)){
  
  cat("processing url", j, "/", length(agg.comp), "\r")
  
  sa <- read_xml(x=agg.urls[j])
  
  agg.comp[[j]]$code <- agg.codes[j]
  
  xpath <- paste0("//rdf:Description[contains(@rdf:about, '", agg.codes[j], "')]")
  
  xml.node <- xml_find_all(sa, xpath=xpath)
  
  agg.comp[[j]]$name_en <- xml_text(xml_find_all(xml.node, xpath="skos:prefLabel[@xml:lang='en']"))
  agg.comp[[j]]$name_fr <- xml_text(xml_find_all(xml.node, xpath="skos:prefLabel[@xml:lang='fr']"))
  agg.comp[[j]]$name_de <- xml_text(xml_find_all(xml.node, xpath="skos:prefLabel[@xml:lang='de']"))
  
  comp <- xml_attr(xml_find_all(xml.node, xpath="skos:member"), "resource")
  agg.comp[[j]]$composition <- sort(sapply(X=strsplit(comp, split="/", fixed=TRUE), FUN=function(z) z[length(z)]))
  
  Sys.sleep(runif(n=1, min=0.1, max=0.5))
  
}

# coerce to datatable:
spec.aggs <- rbindlist(lapply(agg.comp, "[", 1:4))
spec.aggs$composition <- lapply(agg.comp, "[[", 5)

usethis::use_data(spec.aggs, overwrite=TRUE)

# END