# START

# set global options:
options(hicp.chatty=FALSE)
options(hicp.coicop.prefix="") # no prefix
options(hicp.coicop.version="ecoicop2.hicp")
options(hicp.all.items.code="TOTAL")


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
  parent(id="TOTAL"), 
  list(NULL)
)

# all-items code is the parent of another code:
expect_equal(
  parent(id="01", usedict=TRUE), 
  list("TOTAL")
)

# user-defined all-items code is the parent of another code:
expect_equal(
  parent(id="01", usedict=TRUE, settings=list(all.items.code="X")), 
  list("X")
)

# valid codes only:
expect_equal(
  parent(id=c("01","011","0111","0112")),
  list("TOTAL","01","011","011")
)

# codes with prefix not expected:
expect_equal(
  parent(id=c("CP01","CP011","CP0111","CP0112")),
  as.list(rep(NA_character_,4))
)

# codes with prefix expected:
expect_equal(
  parent(id=c("CP01","CP011","CP0111","CP0112"), settings=list(coicop.prefix="CP")),
  list("TOTAL","CP01","CP011","CP011")
)

# simplification of output to vector:
expect_equal(
  parent(id=c("01","011","0111","0112"), settings=list(simplify=TRUE)),
  c("TOTAL","01","011","011")
)

# all coicop codes valid in ecoicop:
expect_equal(
  parent(id=c("094","0943","09430"), settings=list(coicop.version="ecoicop1")), 
  list("09","094","0943")
)

# some coicop codes not valid in ecoicop-hicp:
expect_equal(
  parent(id=c("094","0943","09430"), settings=list(coicop.version="ecoicop1.hicp")), 
  list("09",NA_character_,NA_character_)
)

# input including bundle codes:
id <- c("05","0531_2","0531","05311","05321")

# use dictionary and derive closest parent:
expect_equal(
  parent(
    id=id, usedict=TRUE, 
    settings=list(coicop.version="ecoicop1.hicp")),
  list("TOTAL","053","053","0531","0532")
)

# use dictionary and derive direct parent:
expect_equal(
  parent(
    id=id, usedict=TRUE, closest=FALSE, k=1, 
    settings=list(coicop.version="ecoicop1.hicp")),
  list("TOTAL","053","053","0531","0532")
)

# use dictionary and derive direct parent and grandparent:
expect_equal(
  parent(
    id=id, usedict=TRUE, closest=FALSE, k=1:2, 
    settings=list(coicop.version="ecoicop1.hicp")),
  list("TOTAL",c("05","053"),c("05","053"),c("053","0531"),c("053","0532"))
)

# use no coicop dictionary and derive closest parent:
expect_equal(
  parent(
    id=id, usedict=FALSE, closest=TRUE, 
    settings=list(coicop.version="ecoicop1.hicp")),
  list(NULL,"05","05",c("0531_2","0531"),"0531_2")
)

# use no dictionary and derive direct parent:
expect_equal(
  parent(
    id=id, usedict=FALSE, closest=FALSE, k=1,
    settings=list(coicop.version="ecoicop1.hicp")),
  list(NULL,NULL,NULL,c("0531_2","0531"),"0531_2")
)

# use dictionary and derive direct parent and grandparent:
expect_equal(
  parent(
    id=id, usedict=FALSE, closest=FALSE, k=1:2, 
    settings=list(coicop.version="ecoicop1.hicp")),
  list(NULL,"05","05",c("0531_2","0531"),"0531_2")
)

# simplification of output to vector:
expect_equal(
  parent(
    id=c("121","1212","1212_1213","12121","12122","12131"), 
    settings=list(simplify=TRUE, coicop.version="ecoicop1.hicp")), 
  c("12","121","121","1212","1212","1213")
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

# all-items code has children:
expect_equal(
  child(id="TOTAL", usedict=TRUE), 
  list(formatC(1:13, width=2, flag="0"))
)

# user-defined all-items code has children:
expect_equal(
  child(id="X", usedict=TRUE, settings=list(all.items.code="X")), 
  list(formatC(1:13, width=2, flag="0"))
)

# valid codes:
expect_equal(
  child(id=c("01","02")),
  list(c("011","012","013"), c("021","022","023"))
)

# codes with prefix not expected:
expect_equal(
  child(id=c("CP01","CP02")),
  as.list(rep(NA_character_,2))
)

# codes with prefix expected:
expect_equal(
  child(id=c("CP01","CP02"), settings=list(coicop.prefix="CP")),
  list(c("CP011","CP012","CP013"), c("CP021","CP022","CP023"))
)

# simplification of output to vector:
expect_equal(
  child(id=c("01","011","0111"), usedict=FALSE, settings=list(simplify=TRUE)), 
  c("011","0111",NA)
)

# all coicop codes valid in ecoicop:
expect_equal(
  child(id=c("094","0943","09430"), settings=list(coicop.version="ecoicop1")), 
  list(c("0941","0942","0943"),"09430",NULL)
)

# some coicop codes not valid in ecoicop-hicp:
expect_equal(
  child(c("094","0943","09430"), settings=list(coicop.version="ecoicop1.hicp")), 
  list(c("0941","0942"),NA_character_,NA_character_)
)

# input including bundle codes:
id <- c("05","0531_2","0531","05311","05321")

# use dictionary and derive closest children:
expect_equal(
  child(
    id=id, usedict=TRUE, closest=TRUE, 
    settings=list(coicop.version="ecoicop1.hicp")),
  list(
    c("051","052","053","054","055","056"),
    c("05311","05312","05313","05314","05315","05319","05321","05322","05323","05324","05329"),
    c("05311","05312","05313","05314","05315","05319"),
    NULL,
    NULL)
)

# use dictionary and derive direct children:
expect_equal(
  child(
    id=id, usedict=TRUE, closest=FALSE, k=1, 
    settings=list(coicop.version="ecoicop1.hicp")),
  list(
    c("051","052","053","054","055","056"),
    c("05311","05312","05313","05314","05315","05319","05321","05322","05323","05324","05329"),
    c("05311","05312","05313","05314","05315","05319"),
    NULL,
    NULL)
)

# use dictionary and derive direct children and grandchildren:
expect_equal(
  child(
    id=id, usedict=TRUE, closest=FALSE, k=1:2, 
    settings=list(coicop.version="ecoicop1.hicp")),
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
  child(id=id, usedict=FALSE, settings=list(coicop.version="ecoicop1.hicp")),
  list(c("0531_2","0531"),c("05311","05321"),"05311",NULL,NULL)
)

# use no dictionary and derive direct children:
expect_equal(
  child(id=id, usedict=FALSE, closest=FALSE, k=1, settings=list(coicop.version="ecoicop1.hicp")),
  list(NULL,c("05311","05321"),"05311",NULL,NULL)
)

# use dictionary and derive direct children and grandchildren:
expect_equal(
  child(id=id, usedict=FALSE, closest=FALSE, k=1:2, settings=list(coicop.version="ecoicop1.hicp")),
  list(c("0531_2","0531"),c("05311","05321"),"05311",NULL,NULL)
)

# END
