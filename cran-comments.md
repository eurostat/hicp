## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
❯ On windows-x86_64-devel (r-devel)
  checking CRAN incoming feasibility ... [12s] NOTE
  Maintainer: 'Sebastian Weinand <sebastian.weinand@ec.europa.eu>'
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Eurostat's (9:92)
    HICP (9:81)

These are no misspelled words.

❯ On windows-x86_64-devel (r-devel)
  checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

This seems to be related to the test environment and can be ignored.

❯ On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

This seems to be related to the test environment and can be ignored.

❯ On ubuntu-gcc-release (r-release)
  checking CRAN incoming feasibility ... [6s/37s] NOTE
  Maintainer: ‘Sebastian Weinand <sebastian.weinand@ec.europa.eu>’
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Eurostat's (9:92)
    HICP (9:81)

These are no misspelled words.

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

This seems to be related to the test environment and can be ignored.

❯ On fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... [7s/35s] NOTE
  Maintainer: ‘Sebastian Weinand <sebastian.weinand@ec.europa.eu>’
  
  New submission
  
  Possibly misspelled words in DESCRIPTION:
    Eurostat's (9:92)
    HICP (9:81)

These are no misspelled words.

0 errors ✔ | 0 warnings ✔ | 6 notes ✖