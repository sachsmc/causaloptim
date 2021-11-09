
## build
file.copy("doc/shinyapp.html", "inst/shiny/interface/www/shinyapp.html", overwrite = TRUE)
devtools::build()

library(rhub)
ch <- check_for_cran("package", show_status = FALSE)
ch$update()

pkgdown::build_site(install = FALSE)
## fix path to images
lines <- readLines("docs/articles/shinyapp.html")
lines <- gsub("../../../../OneDrive%20-%20KI.SE/Code/causaloptim/vignettes/", "", lines)
writeLines(lines, con = "docs/articles/shinyapp.html")

lines <- readLines("docs/articles/vertexenum-speed.html")
lines <- gsub("../../../../OneDrive%20-%20KI.SE/Code/causaloptim/vignettes/", "", lines)
writeLines(lines, con = "docs/articles/vertexenum-speed.html")
