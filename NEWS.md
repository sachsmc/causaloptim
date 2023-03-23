# causaloptim 0.9.7

- More fixes to snprintf and LEN_LABEL
- Fixed warnings from Balke's old cpp code
- update bibentry

# causaloptim 0.9.3

- Minor bug fixes in Balke's old code (sprintf -> snprintf)
- removed methods vignette and linked to published article instead (it is open access, please email me if you can't access it)
- bugfix to ensure parameters get removed if constraints make them impossible

# causaloptim 0.9.2

## bug fixes

- Fixed parsing issue in latex_bounds

# causaloptim 0.9.2

## bug fixes

- Fixed c++ compiler warning about length of buffer in Tableau.cpp 

# causaloptim 0.9.1

## More big new features!

- categorical variables are now handled
- Update shiny UI to enable specification of categorical variables
- Update draft of methods paper in vignette

## bug fixes

- Fixed propagation of intervention sets downstream when there are crossed effects

# causaloptim 0.8.2

## Big new features!

- Major update to the vertex enumeration algorithm (thanks to @gjo11). There is a new function called `optimize_effect_2` which uses the `rcdd` package to enumerate the vertices. This is significantly faster than Balke's C++ code. This function is used by default in the shiny app. There are some minor differences in how the bounds are displayed, and note that the display of the vertices within MIN and MAX may differ from `optimize_effect`. The bounds are equivalent (just in a different order) in all of the test cases that we have looked at. 
- New vignette explaining how the optimization was implemented (also thanks to @gjo11)

## Small features

- interpret_bounds returns function that will now give a warning if bounds are invalid. 
- Pull steps of analyze_graph out into separate modules.

## Bugfixes

- Make sure that qs are unique in large problems

# causaloptim 0.7.1

## Bugfixes

- Propagation of intervention set when not all paths are defined
- Checking for violations of condition 6 in effect

## New features

- Additional information returned from analyze_graph
- Better printing for analyze_graph objects 


# causaloptim 0.6.5

## Bugfixes

- Fixed mismatched new/delete/delete[] in C++ code


# causaloptim 0.6.4

## Bugfixes

- Parse effect fixed for joint outcome probabilities.
- Added examples
- Update description

# causaloptim 0.6.3

## Bugfixes

- Refactor algorithm 1 so that it works correctly for unobserved variables
- Fix probability printing in shiny app for unobserved variables

# causaloptim 0.6.2

## Bugfixes

- Fixed repeated qs bug that affects graphs with multiple variables on left side
- Added citation file

# causaloptim 0.6.1

## Minor updates

- Allow user interrupt in long-running c++ loops
- Increase maximum number of vertices
- Added progress indicator to shiny app
- Update dependencies

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
+ CRAN checks

## New features 

+ Error checking in parse effect and parse constraints in shiny app

# causaloptim 0.3.0

Rebuilt interface, text based now. 