This is a resubmission to address the additional issues that came up 
on https://cran.r-project.org/web/checks/check_results_causaloptim.html 
for the ASAN and valgrind checks. I made the necessary changes to fix
Mismatched free() / delete / delete [] errors and I checked it using
valgrind running on rocker/r-devel-san. I was able to reproduce the 
original errors and fix them all on that platform. 