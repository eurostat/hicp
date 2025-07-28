# START

options("hicp.chatty"=FALSE)


# Function is.coicop() ----------------------------------------------------


expect_equal(is.coicop(character()), logical())
expect_true(all(is.coicop(c("01","011","0111","01111"))))
expect_false(all(is.coicop(c("01","011","0111","01111","0943"))))
expect_true(all(is.coicop(c("01","011","0111","01111","0943"), settings=list(coicop.version="ecoicop"))))
expect_false(is.coicop("08X")) # bundle codes are no valid coicop codes


# Function level() --------------------------------------------------------


expect_equal(level(character()), integer())
expect_equal(level(id=c("00","01","011","0111","01111","0943")), c(1:5,NA))
expect_equal(level(id=c("00","01","011","0111","01111","0943"), settings=list(coicop.version="ecoicop")), c(1:5,4))
expect_equal(level(id="08X"), 3L)


# Function is.bundle() ----------------------------------------------------


# empty character:
expect_equal(
  is.bundle(character()), 
  logical()
)

expect_equal(
  c(F,F,F,T,T,F),
  is.bundle(id=c(NA,"01","08","08X","1212_1213","1212"))
)

expect_error(
  is.bundle(id=c("01_02"), settings=list("coicop.bundles"=c("01_02")))
)

expect_error(
  is.bundle(id=c("01_02"), settings=list("coicop.bundles"=list("01_02")))
)

expect_equal(
  T,
  is.bundle(id=c("01_02"), settings=list("coicop.bundles"=list("01_02"=c("01","02"))))
)


# Function unbundle() -----------------------------------------------------


# empty character:
expect_equal(
  unbundle(character()), 
  list()
)

res <- c(NA,"01","08","082","083","1212","1213","1212")
names(res) <- c(NA,"01","08","08X","08X","1212_1213","1212_1213","1212")
expect_equal(
  res,
  unbundle(id=c(NA,"01","08","08X","1212_1213","1212"), settings=list(simplify=TRUE))
)

res <- c("081","082","083","082","083")
names(res) <- c("081","08X","08X","082_083","082_083")
expect_equal(
  res,
  unbundle(id=c("081","08X","082_083"), settings=list(simplify=TRUE))
)

expect_error(
  unbundle(id=c("01_02"), settings=list("coicop.bundles"=c("01_02")))
)

expect_error(
  unbundle(id=c("01_02"), settings=list("coicop.bundles"=list("01_02")))
)

expect_equal(
  list(c("01","02")),
  unbundle(id=c("01_02"), settings=list("coicop.bundles"=list("01_02"=c("01","02"))))
)


# Function parent() -------------------------------------------------------


# empty character:
expect_equal(
  parent(id=character()),
  list()
)

# only NA present:
expect_equal(
  parent(id=NA_character_), 
  list(NA_character_)
)

# only non-valid codes present:
expect_equal(
  parent(id="99"), 
  list(NA_character_)
)

# all-items code has no parent:
expect_equal(
  parent(id="00"), 
  list(NULL)
)

# all-items code is the parent of another code:
expect_equal(
  parent(id="01", usedict=TRUE), 
  list("00")
)

# user-defined all-items code is the parent of another code:
expect_equal(
  parent(id="01", usedict=TRUE, settings=list(all.items.code="X")), 
  list("X")
)

# usual codes:
expect_equal(
  parent(id=c("01","011","0111","0112")),
  list("00","01","011","011")
)

# simplification of output to vector:
expect_equal(
  parent(id=c("121","1212","1212_1213","12121","12122","12131"), settings=list(simplify=TRUE)), 
  c("12","121","121","1212","1212","1213")
)

# all coicop codes valid in ecoicop:
expect_equal(
  parent(id=c("094","0943","09430"), settings=list(coicop.version="ecoicop")), 
  list("09","094","0943")
)

# some coicop codes not valid in ecoicop-hicp:
expect_equal(
  parent(c("094","0943","09430"), settings=list(coicop.version="ecoicop.hicp")), 
  list("09",NA_character_,NA_character_)
)

# input including bundle codes:
id <- c("05","0531_2","0531","05311","05321")

# use dictionary and derive closest parent:
expect_equal(
  parent(id=id, usedict=TRUE),
  list("00","053","053","0531","0532")
)

# use dictionary and derive direct parent:
expect_equal(
  parent(id=id, usedict=TRUE, closest=FALSE, k=1),
  list("00","053","053","0531","0532")
)

# use dictionary and derive direct parent and grandparent:
expect_equal(
  parent(id=id, usedict=TRUE, closest=FALSE, k=1:2),
  list("00",c("05","053"),c("05","053"),c("053","0531"),c("053","0532"))
)

# use no coicop dictionary and derive closest parent:
expect_equal(
  parent(id=id, usedict=FALSE, closest=TRUE),
  list(NULL,"05","05",c("0531_2","0531"),"0531_2")
)

# use no dictionary and derive direct parent:
expect_equal(
  parent(id=id, usedict=FALSE, closest=FALSE, k=1),
  list(NULL,NULL,NULL,c("0531_2","0531"),"0531_2")
)

# use dictionary and derive direct parent and grandparent:
expect_equal(
  parent(id=id, usedict=FALSE, closest=FALSE, k=1:2),
  list(NULL,"05","05",c("0531_2","0531"),"0531_2")
)


# Function child() --------------------------------------------------------


# empty character:
expect_equal(
  child(id=character()),
  list()
)

# only NA present:
expect_equal(
  child(id=NA_character_), 
  list(NA_character_)
)

# only non-valid codes present:
expect_equal(
  child(id="99"), 
  list(NA_character_)
)

# lowest level code has no child:
expect_equal(
  child(id="01111"), 
  list(NULL)
)

# all-items code has childs:
expect_equal(
  child(id="00", usedict=TRUE), 
  list(c("01","02","03","04","05","06","07","08","09","10","11","12"))
)

# user-defined all-items code has childs:
expect_equal(
  child(id="X", usedict=TRUE, settings=list(all.items.code="X")), 
  list(c("01","02","03","04","05","06","07","08","09","10","11","12"))
)

# usual codes:
expect_equal(
  child(id=c("01","02")),
  list(c("011","012"), c("021","022"))
)

# simplification of output to vector:
expect_equal(
  child(id=c("01","011","0111"), usedict=FALSE, settings=list(simplify=TRUE)), 
  c("011","0111",NA)
)

# all coicop codes valid in ecoicop:
expect_equal(
  child(id=c("094","0943","09430"), settings=list(coicop.version="ecoicop")), 
  list(c("0941","0942","0943"),"09430",NULL)
)

# some coicop codes not valid in ecoicop-hicp:
expect_equal(
  child(c("094","0943","09430"), settings=list(coicop.version="ecoicop.hicp")), 
  list(c("0941","0942"),NA_character_,NA_character_)
)

# input including bundle codes:
id <- c("05","0531_2","0531","05311","05321")

# use dictionary and derive closest children:
expect_equal(
  child(id=id, usedict=TRUE, closest=TRUE),
  list(
    c("051","052","053","054","055","056"),
    c("05311","05312","05313","05314","05315","05319","05321","05322","05323","05324","05329"),
    c("05311","05312","05313","05314","05315","05319"),
    NULL,
    NULL)
)

# use dictionary and derive direct children:
expect_equal(
  child(id=id, usedict=TRUE, closest=FALSE, k=1),
  list(
    c("051","052","053","054","055","056"),
    c("05311","05312","05313","05314","05315","05319","05321","05322","05323","05324","05329"),
    c("05311","05312","05313","05314","05315","05319"),
    NULL,
    NULL)
)

# use dictionary and derive direct children and grandchildren:
expect_equal(
  child(id=id, usedict=TRUE, closest=FALSE, k=1:2),
  list(
    c("051","0511","0512","0513","052","0520","053","0531","0532","0533","054","0540",
      "055","0551","0552","056","0561","0562"),
    c("05311","05312","05313","05314","05315","05319","05321","05322","05323","05324","05329"),
    c("05311","05312","05313","05314","05315","05319"),
    NULL,
    NULL)
)

# use no coicop dictionary and derive closest children:
expect_equal(
  child(id=id, usedict=FALSE),
  list(c("0531_2","0531"),c("05311","05321"),"05311",NULL,NULL)
)

# use no dictionary and derive direct children:
expect_equal(
  child(id=id, usedict=FALSE, closest=FALSE, k=1),
  list(NULL,c("05311","05321"),"05311",NULL,NULL)
)

# use dictionary and derive direct children and grandchildren:
expect_equal(
  child(id=id, usedict=FALSE, closest=FALSE, k=1:2),
  list(c("0531_2","0531"),c("05311","05321"),"05311",NULL,NULL)
)

# END
