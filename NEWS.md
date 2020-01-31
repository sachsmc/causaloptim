# causaloptim 0.6.1

## Minor updates

- Allow user interrupt in long-running c++ loops
- Increase maximum number of vertices
- Added progress indicator to shiny app

# causaloptim 0.6.0

## New features

- Allow for observed variables in the causal effect. E.g., for the treatment effect among the treated `p{Y(X = 1) = 1; X = 1} - p{Y(X = 0) = 1; X = 1}`

# causaloptim 0.5.3

## Bugfixes

- Fixed warnings on CRAN check
- Added travis ci
- Lowercase min and max in latex
- Additional documentation on shiny app


# causaloptim 0.5.2

## Bugfixes

- Better reset functionality in shiny app

# causaloptim 0.5.1

## New features

- button to print latex code in shiny app
- Checking for intervention set children to be on rightside
- Other error checks and bugfixes

# causaloptim 0.5.0

## New features

- latex printing of bounds
    
# causaloptim 0.4.1

## Bug fixes

+ Bugfixes in parsing bounds
+ Cran checks

## New features 

+ Error checking in parse effect and parse constraints in shiny app

# causaloptim 0.3.0

Rebuilt interface, text based now. 