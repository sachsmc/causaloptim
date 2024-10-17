[![CRAN Status](https://www.r-pkg.org/badges/version/causaloptim)](https://cran.r-project.org/package=causaloptim)
[![R-CMD-check](https://github.com/sachsmc/causaloptim/workflows/R-CMD-check/badge.svg)](https://github.com/sachsmc/causaloptim/actions)


# causaloptim: An Interface to Specify Causal Graphs and Compute Bounds on Causal Effects

When causal quantities are not identifiable from the observed data, it still may be possible to bound these quantities using the observed data. We outline a class of problems for which the derivation of tight bounds is always a linear programming problem and can therefore, at least theoretically, be solved using a symbolic linear optimizer. We provide a user friendly graphical interface for setting up such problems via DAGs, which only allow for problems within this class to be depicted. The user can then define linear constraints to further refine their assumptions to meet their specific problem, and then specify a causal query using a text interface. The program converts this user defined DAG, query, and constraints, and returns tight bounds. The bounds can be converted to R functions to evaluate them for specific datasets, and to latex code for publication. 

## Development status

This package is in stable development. The interface is unlikely to have major changes at this time. New features may be added over time.  

## Installation

``` r
install.packages("causaloptim")
# or
remotes::install_github("sachsmc/causaloptim")
```

Or use the web application: https://sachsmc.shinyapps.io/causaloptimweb/


## Usage

Launch the shiny app to get started, results are saved in the `results` object: 

``` r
results <- specify_graph()
```

## References

Sachs, M. C., Jonzon, G., Sjölander, A., & Gabriel, E. E. (2023). A general method for deriving tight symbolic bounds on causal effects. Journal of Computational and Graphical Statistics, 32(2), 567-576. https://www.tandfonline.com/doi/full/10.1080/10618600.2022.2071905/ 

Jonzon, G., Sachs, M. C., & Gabriel, E. E. (2023). Accessible Computation of Tight Symbolic Bounds on Causal Effects using an Intuitive Graphical Interface. R Journal, 15(4). https://journal.r-project.org/articles/RJ-2023-083/

A. Balke and J. Pearl, "Counterfactual Probabilities: Computational Methods,Bounds, and Applications"  UCLA Cognitive Systems Laboratory, Technical Report (R-213-B). 
In R. Lopez de Mantaras and D. Poole (Eds.), Proceedings of the Conference on Uncertainty in Artificial Intelligence (UAI-94), Morgan Kaufmann, San Mateo, CA, 46-54, July 29-31, 1994. 
https://ftp.cs.ucla.edu/pub/stat_ser/R213-B.pdf .
