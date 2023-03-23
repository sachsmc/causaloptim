This is a resubmission that fixes most of the remaining warnings reported by gcc13. 

The exceptions are as follows: 
    - warning: variable ‘bResult’ set but not used -- This may be a false positive, bResult is used and it is necessary for the code to function. Without it, it compiles, but I get the wrong coefficients for a known test case. 
    - Code will not compile without defining these. Pretty sure they are false positives :
    * Tableau.cpp:263:8: warning: variable ‘bMinimize’ set but not used [-Wunused-but-set-variable]
  263 |  char  bMinimize;
      |        ^~~~~~~~~
    * Tableau.cpp:274:8: warning: variable ‘objGet’ set but not used [-Wunused-but-set-variable]
  274 |  char* objGet;