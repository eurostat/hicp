# START

# set global options:
options(hicp.chatty=FALSE)
options(hicp.coicop.prefix="") # no prefix
options(hicp.coicop.version="ecoicop2.hicp")
options(hicp.all.items.code="TOTAL")


# is.coicop() -------------------------------------------------------------


expect_equal(is.coicop(character()), logical())
expect_false(is.coicop(NA_character_))
expect_false(is.coicop("TOTAL")) # all-items is no valid COICOP code
expect_all_true(is.coicop(c("01","011","0111","0943")))
expect_all_false(is.coicop(c("CP01","CP011","CP0111","CP0943")))
expect_all_true(is.coicop(c("CP01","CP011","CP0111","CP0943"), settings=list(coicop.prefix="CP")))
expect_false(is.coicop("0943", settings=list(coicop.version="ecoicop1.hicp")))


# is.bundle() -------------------------------------------------------------


expect_equal(is.bundle(character()), logical())
expect_false(is.bundle(NA_character_))
expect_false(is.bundle("TOTAL"))
expect_all_false(is.bundle(c("CP08X","08X")))
expect_true(is.bundle("08X", settings=list(coicop.version="ecoicop1.hicp")))
expect_true(is.bundle("CP08X", settings=list(coicop.version="ecoicop1.hicp", coicop.prefix="CP")))


# is.spec.agg() -----------------------------------------------------------


expect_equal(is.spec.agg(character()), logical())
expect_false(is.spec.agg(NA_character_))
expect_false(is.spec.agg("TOTAL"))
expect_all_true(is.spec.agg(names(spec.agg(id=NULL))))
expect_all_false(is.spec.agg(names(spec.agg(id=NULL)), settings=list(coicop.version="ecoicop2")))
expect_all_false(is.spec.agg(c("food","01","CP01")))


# level() -----------------------------------------------------------------


expect_equal(level(character()), integer())
expect_equal(level(NA_character_), NA_integer_)
expect_equal(level("TOTAL"), 1L)
expect_equal(level(c("01","011","0111","0943")), as.integer(c(2,3,4,4)))
expect_equal(level(c("CP01","CP011","CP0111","CP0943")), as.integer(c(NA,NA,NA,NA)))
expect_equal(level(c("CP01","CP011","CP0111","CP0943"), settings=list(coicop.prefix="CP")), as.integer(c(2,3,4,4)))
expect_equal(level("0943", settings=list(coicop.version="ecoicop1.hicp")), NA_integer_)
expect_equal(level("08X", settings=list(coicop.version="ecoicop1.hicp")), 3L)


# label() -----------------------------------------------------------------


expect_equal(label(character()), character())
expect_equal(label(NA_character_), NA_character_)
expect_equal(label("TOTAL"), "All-items")
expect_equal(label("TOTAL", settings=list(all.items.code=c("test"="TOTAL"))), "test")
expect_all_true(!is.na(label(c("01","02","AP","FOOD"))))


# spec.agg() --------------------------------------------------------------


expect_length(spec.agg(character()), 0L)
expect_type(spec.agg(character()), "list")
expect_named(spec.agg(character()), character())
expect_length(spec.agg(NA_character_), 0L)
expect_type(spec.agg(NA_character_), "list")
expect_named(spec.agg(NA_character_), character())
expect_length(spec.agg("FOOD"), 1L)
expect_type(spec.agg("FOOD"), "list")
expect_named(spec.agg("FOOD"), "FOOD")
expect_all_true(startsWith(spec.agg("FOOD", settings=list(coicop.prefix="CP"))[[1]], "CP"))
expect_length(spec.agg("FOOD", settings=list(coicop.version="ecoicop2")), 0L)
expect_length(spec.agg(c("FOOD","NRG","FOOD","test")), 2L)
expect_named(spec.agg(c("FOOD","NRG","FOOD","test")), c("FOOD","NRG"))


# END
