test_that("## regular and contaminated IV case", {
    
    
    b <- graph_from_literal(Z -+ X, X -+ Y, Ur -+ X, Z -+ Y, Ur -+ Y)
    V(b)$leftside <- c(1,0,0,0)
    V(b)$latent <- c(0, 0,0,1)
    V(b)$nvals <- c(2,2,2,2)
    E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0, 0, 0)
    
    graph <- b
    
    observed.variables <- V(graph)[V(graph)$latent == 0]
    var.values <- lapply(names(observed.variables), 
                         function(varname) seq(from = 0, to = causaloptim:::numberOfValues(graph, varname) - 1))
    names(var.values) <- names(observed.variables)
    p.vals <- do.call(expand.grid, var.values)
    
    respvars <- create_response_function(graph)
    
    prob.form <- list(out = c("Y", "X"), cond = "Z")
    
    expect_true(check_linear_constraints(respvars, p.vals, prob.form))
    
    effectt <- "p{Y(X = 1) = 1}"
    
    expect_false(check_linear_objective(respvars, effectt, prob.form))
    
    ## regular IV case
    
    b <- graph_from_literal(Z -+ X, X -+ Y, Ur -+ X, Ur -+ Y)
    V(b)$leftside <- c(1,0,0,0)
    V(b)$latent <- c(0, 0,0,1)
    V(b)$nvals <- c(2,2,2,2)
    E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0, 0)
    
    graph <- b
    
    observed.variables <- V(graph)[V(graph)$latent == 0]
    var.values <- lapply(names(observed.variables), 
                         function(varname) seq(from = 0, to = causaloptim:::numberOfValues(graph, varname) - 1))
    names(var.values) <- names(observed.variables)
    p.vals <- do.call(expand.grid, var.values)
    
    respvars <- create_response_function(graph)
    
    prob.form <- list(out = c("Y", "X"), cond = "Z")
    
    expect_true(check_linear_constraints(respvars, p.vals, prob.form))
    
    effectt <- "p{Y(X = 1) = 1}"
    
    expect_true(check_linear_objective(respvars, effectt, prob.form))
    

})


test_that("## interventional direct effects", {
    
    
    b <- graph_from_literal(A -+ Am, A -+ Ay, Ul -+ A, Ul -+ Am, Ul -+ Ay, 
                            Am -+ M, Ay -+ Y, M -+ Y, Ur -+ M, Ur -+ Y)
    V(b)$leftside <-c(rep(1, 4), rep(0, 3))
    V(b)$latent <- c(0, 1,1,1, 0, 0, 1)
    V(b)$nvals <- rep(2, length(V(b)))
    E(b)$rlconnect <- E(b)$edge.monotone <- rep(0, length(E(b)))
    
    graph <- b
   
    leftind <- vertex_attr(graph)$leftside
    cond.vars <- V(graph)[leftind == 1 & names(V(graph)) != "Ul"]
    right.vars <- V(graph)[leftind == 0 & names(V(graph)) != "Ur"]
    obsvars <- c(right.vars, cond.vars)
    observed.variables <- V(graph)[V(graph)$latent == 0]
    var.values <- lapply(names(observed.variables), function(varname) seq(from = 0, 
                                                                          to = causaloptim:::numberOfValues(graph, varname) - 1))
    names(var.values) <- names(observed.variables)
    p.vals <- do.call(expand.grid, var.values)
    
    jd <- do.call(paste0, p.vals[, names(right.vars[right.vars$latent == 
                                                        0]), drop = FALSE])
    cond <- do.call(paste0, p.vals[, names(cond.vars[cond.vars$latent == 
                                                         0]), drop = FALSE])
    parameters <- paste0("p", paste(jd, cond, sep = "_"))
    
    
    parameters.key <- paste(paste(names(right.vars[right.vars$latent ==
                                                       0]), collapse = ""),
                            paste(names(cond.vars[cond.vars$latent ==
                                                      0]), collapse = ""), sep = "_")
    
    respvars <- create_response_function(graph)
    respvars$Ay$index <- respvars$Ay$index[1]
    respvars$Ay$values <- respvars$Ay$values[3]
    respvars$Am$index <- respvars$Am$index[1]
    respvars$Am$values <- respvars$Am$values[3]
    
    
    prob.form <- list(out = c("Y", "M"), cond = c("A"))
    expect_true(check_linear_constraints(respvars, p.vals, prob.form))
    
    effectt <- "p{Y(Ay = 1, Am = 1) = 1} - p{Y(Ay = 0, Am = 1) = 1}"
    
    expect_true(check_linear_objective(respvars, effectt, prob.form))
    
    restest <- optimize_effect_2(linearcausalproblem_from_response_functions(respvars, p.vals, 
                                                                             prob.form, effectt, 
                                                                             constraints = NULL) )
        
    
})
