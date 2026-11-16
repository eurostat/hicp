# START

# countries():
expect_length(countries(), 39L)
expect_named(countries())

# european.agg():
expect_length(european.agg(id="EA", t=as.Date("2025-01-01")), 20L)
expect_length(european.agg(id="EA", t=as.Date("2026-01-01")), 21L)
expect_length(european.agg(id="EU", t=as.Date("2026-01-01")), 27L)
expect_length(european.agg(id="EEA", t=as.Date("2026-01-01")), 29L)
expect_error(european.agg(id="XXX", t=as.Date("2026-01-01")))
expect_error(european.agg(id="EA", t=as.Date(c("2026-01-01","2029-01-01"))))

# is.ea():
expect_all_true(is.ea(id=european.agg(id="EA")))
expect_equal(is.ea(id=c("HR","HR"), t=as.Date(c("2022-12-31","2023-01-01"))), c(F,T))

# is.eu():
expect_all_true(is.eu(id=european.agg(id="EU")))
expect_equal(is.eu(id=c("UK","UK"), t=as.Date(c("2020-01-31","2020-02-01"))), c(T,F))

# is.eea():
expect_all_true(is.eea(id=european.agg(id="EEA")))
expect_equal(is.eea(id=c("IS","NO"), t=as.Date("2026-01-01")), c(T,T))

# END
