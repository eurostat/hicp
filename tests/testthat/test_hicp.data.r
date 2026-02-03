# START

# set cores for testing on CRAN via devtools::check_rhub()
suppressPackageStartupMessages(library(restatapi))
options(restatapi_cores=1)


# datasets() --------------------------------------------------------------


dt1 <- datasets()

expect_equal(data.table::is.data.table(dt1), TRUE)
if(ncol(dt1)>0L){
  expect_true(nrow(dt1)>0L)
  expect_contains(names(dt1), c("title","code","type","lastUpdate","lastModified","dataStart","dataEnd","values"))
}


# datafilters() -----------------------------------------------------------


dt2 <- datafilters(id="prc_hicp_iw")

expect_equal(data.table::is.data.table(dt2), TRUE)
if(ncol(dt2)>0L){
  expect_true(nrow(dt2)>0L)
  expect_contains(names(dt2), c("concept","code","name"))
}


# data() ------------------------------------------------------------------


y.min <- as.integer(format(Sys.Date(), "%Y"))-3L

dt3 <- data(id="prc_hicp_iw", 
            filters=list("geo"="EA", "coicop18"=c("CP01","CP02")), 
            date.range=as.character(c(y.min, NA)))

expect_equal(data.table::is.data.table(dt3), TRUE)
if(ncol(dt3)>0L){
  expect_true(nrow(dt3)>0)
  expect_contains(names(dt3), c("coicop18","geo","time","values"))
  expect_equal(unique(as.character(dt3$geo)), "EA")
  expect_equal(unique(as.character(dt3$coicop)), c("CP01","CP02"))
  expect_equal(min(as.integer(dt3$time)), y.min)
}

# END