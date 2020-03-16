This is a resubmission. I addressed the following issues: 


## Comment 1 from CRAN: 

Please always add all authors and copyright holders in the Authors@R
field with the appropriate roles.
 From CRAN policies you agreed to:
"Where copyrights are held by an entity other than the package authors,
this should preferably be indicated via ‘cph’ roles in the ‘Authors@R’
field, or using a ‘Copyright’ field (if necessary referring to an
inst/COPYRIGHTS file)."
e.g.: Dirk Eddelbuettel and Romain Francois
Please explain in the submission comments what you did about this issue.

## Response:

The files that reference the copyright held by the above mentioned people
were included in my package by my mistake, so there is no reason to add them
as copyright holders. I removed those files because they were being loaded,
but not acually being used by my code at all. I had used the stdVector.cpp 
file from the Rcpp modules example to figure out how to create a module, 
but it is not actually part of my project, so I removed it. The other file 
(rcpp_module.cpp) with the copyright header was replaced with the version 
that I wrote (coptim_module.cpp). Sorry for missing that!

## Comment 2 from CRAN:

If there are references describing the methods in your package, please
add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year) <arXiv:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'arXiv:', 'https:' and angle brackets for
auto-linking.
(If you want to add a title as well please put it in quotes: "Title")

## Response:

The DESCRIPTION was modified to include the following statements: 
"We extend and generalize the approach of Balke and Pearl (1994) <doi:10.1016/B978-1-55860-332-5.50011-0> and ..."
and
"The methods and proofs of tightness and validity of the bounds are described in
a preprint by Sachs, Gabriel, and Sjölander (2020) 
<https:sachsmc.github.io/causaloptim/articles/CausalBoundsMethods.pdf>". I also
included the latter preprint as a vignette.

## Comment 3 from CRAN:
Please add \value to all .Rd files for exported functions and explain
the functions results in the documentation.
f.i.: plot_graphres.Rd
If a function does not return a value, please document that too, e.g.
\value{None}.
Also please be more specific in what the return value is. Don't just
describe the data type but also what it contains.

## Response: 

I added the value field for the following Rd files:

plot_graphres
find_cycles
parse_effect was modified to be more informative
parse_constraints was modified to be more informative
latex_bounds

## Comment 4 from CRAN: 

F.i. parse_constraints.Rd

Found a typo in parse_constraints.Rd: --> Parse text that defines the
constrains

## Response

Typo (and other typos) fixed.

