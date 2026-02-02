# START

# set cores for testing on CRAN via devtools::check_rhub()
suppressPackageStartupMessages(library(restatapi))
options(restatapi_cores=1)


# Function datasets() -----------------------------------------------------


dt1 <- datasets()

expect_equal(data.table::is.data.table(dt1), TRUE)
if(ncol(dt1)>0L){
  expect_equal(nrow(dt1)>0L, TRUE)
  expect_equal(names(dt1), c("title","code","type","lastUpdate","lastModified","dataStart","dataEnd","values"))
}

Sys.sleep(1)



# Function datafilters() --------------------------------------------------


dt2 <- datafilters(id="prc_hicp_inw")

expect_equal(data.table::is.data.table(dt2), TRUE)
if(ncol(dt2)>0L){
  expect_equal(nrow(dt2)>0L, TRUE)
  expect_equal(names(dt2), c("concept","code","name"))
}

Sys.sleep(1)


# Function data() ---------------------------------------------------------


y.min <- as.character(as.integer(format(Sys.Date(), "%Y"))-3L)

dt3 <- data(id="prc_hicp_inw", 
            filters=list("geo"="EA", "coicop"=c("CP01","CP02")), 
            date.range=c(y.min, NA))

expect_equal(data.table::is.data.table(dt3), TRUE)
if(ncol(dt3)>0L){
  expect_equal(nrow(dt3)>0, TRUE)
  expect_equal(names(dt3), c("coicop","geo","time","values"))
  expect_equal(unique(as.character(dt3$geo)), "EA")
  expect_equal(unique(as.character(dt3$coicop)), c("CP01","CP02"))
  expect_equal(min(as.integer(dt3$time)), as.integer(y.min))
}


# END
