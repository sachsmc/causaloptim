
devtools::build_vignettes()
## build
file.copy("doc/shinyapp.html", "inst/shiny/interface/www/shinyapp.html", overwrite = TRUE)
devtools::build()

library(rhub)
ch <- check_for_cran(".", show_status = FALSE)
ch$update()

pkgdown::build_site(install = FALSE)
