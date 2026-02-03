# START

# countries():
expect_equal(length(countries(group="EA", t=as.Date("2025-01-01"))), 20L)
expect_equal(length(countries(group="EA", t=as.Date("2026-01-01"))), 21L)
expect_equal(length(countries(group="EU", t=as.Date("2026-01-01"))), 27L)
expect_equal(length(countries(group="EEA", t=as.Date("2026-01-01"))), 29L)
expect_error(countries(group="XXX", t=as.Date("2026-01-01")))
expect_error(countries(group="All", t=as.Date(c("2026-01-01","2029-01-01"))))

# is.ea():
expect_true(all(is.ea(id=names(countries(group="EA")))))
expect_equal(is.ea(id=c("HR","HR"), t=as.Date(c("2022-12-31","2023-01-01"))), c(F,T))

# is.eu():
expect_true(all(is.eu(id=names(countries(group="EU")))))
expect_equal(is.eu(id=c("UK","UK"), t=as.Date(c("2020-01-31","2020-02-01"))), c(T,F))

# is.eea():
expect_true(all(is.eea(id=names(countries(group="EEA")))))
expect_equal(is.eea(id=c("IS","NO"), t=as.Date("2026-01-01")), c(T,T))

# END
