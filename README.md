[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN Status](https://www.r-pkg.org/badges/version/causaloptim)](https://cran.r-project.org/package=causaloptim)

# causaloptim: An Interface to Specify Causal Graphs and Compute Balke Bounds

When causal quantities are not identifiable from the observed data, it still may be possible to bound these quantities using the observed data. We outline a class of problems for which the derivation of tight bounds is always a linear programming problem and can therefore, at least theoretically, be solved using Balke's optimizer. Using this class, we provide a user friendly graphical interface for setting up such problems via DAGs, which only allow for problems within this class to be depicted. The user can then define linear constraints to further refine their DAG to meet their specific problem. The program converts this user defined DAG and constraints, and, using Balke's algorithm, returns tight bounds.

## Installation

```{r}
remotes::install_github("sachsmc/causaloptim")
```

## Usage

Launch the shiny app to get started, results are saved in the `results` object: 

```{r}
results <- specify_graph()
```

## References

A. Balke and J. Pearl, "Counterfactual Probabilities: Computational Methods,Bounds, and Applications"  UCLA Cognitive Systems Laboratory, Technical Report (R-213-B). 
In R. Lopez de Mantaras and D. Poole (Eds.), Proceedings of the Conference on Uncertainty in Artificial Intelligence (UAI-94), Morgan Kaufmann, San Mateo, CA, 46-54, July 29-31, 1994. 
\url{https://ftp.cs.ucla.edu/pub/stat_ser/R213-B.pdf}.
