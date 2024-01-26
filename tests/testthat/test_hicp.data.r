# START

# set cores for testing on CRAN via devtools::check_rhub()
suppressPackageStartupMessages(library(restatapi))
options(restatapi_cores=1)


# Function hicp.datasets() ------------------------------------------------


dt1 <- hicp.datasets()

expect_equal(data.table::is.data.table(dt1), TRUE)
expect_equal(nrow(dt1)>0, TRUE)
expect_equal(names(dt1), c("title","code","type","lastUpdate","lastModified","dataStart","dataEnd","values"))

Sys.sleep(1)


# Function hicp.datafilters() ---------------------------------------------


dt2 <- hicp.datafilters(id="prc_hicp_inw")

expect_equal(data.table::is.data.table(dt2), TRUE)
expect_equal(nrow(dt2)>0, TRUE)
expect_equal(names(dt2), c("concept","code","name"))

Sys.sleep(1)


# Function hicp.dataimport() ----------------------------------------------


dt3 <- hicp.dataimport(id="prc_hicp_inw", filter=list("geo"="EA"), date.range=c("2020", NA))

expect_equal(data.table::is.data.table(dt3), TRUE)
expect_equal(nrow(dt3)>0, TRUE)
expect_equal(names(dt3), c("coicop","geo","time","values"))
expect_equal(unique(as.character(dt3$geo)), "EA")
expect_equal(min(as.integer(dt3$time)), 2020)

# END
