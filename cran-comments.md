This is a resubmission to address the issues on the new CRAN checks for noRemap. It also includes some improvements and bugfixes. 

The warning on the CRAN checks was addressed by removing the C++ code from the package. The code was legacy and not really suggested for use anyway. The base R code is faster and more reliable. 

This version has been tested on all major platforms, using Rhub and win-builder.

