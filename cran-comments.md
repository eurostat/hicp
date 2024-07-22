## Resubmission
This is a submission of an updated package version. In this version I have:

* Fixed a bug in the creation of the vignette on the `r-release-windows-x86_64`-flavor

As this error unfolded only for the Windows-release flavor while all other flavors did not show an error, this resubmission has been submitted only a few days after the previous submission.

## Test environments
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
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

❯ On ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found

This seems to be related to the test environment and can be ignored.

0 errors ✔ | 0 warnings ✔ | 3 notes ✖
