test_that("## regular and contaminated IV case", {
    
    
    graph <- initialize_graph(graph_from_literal(Z -+ X, X -+ Y, Ur -+ X, Z -+ Y, Ur -+ Y))
    
    p.vals <- expand.grid(Z = 0:1, X = 0:1, Y = 0:1)
    
    respvars <- create_response_function(graph)
    
    prob.form <- list(out = c("X", "Y"), cond = "Z")
    
    contamiv <- create_causalmodel(respvars = respvars, p.vals = p.vals, prob.form = prob.form)
    
    expect_true(contamiv$counterfactual_constraints$linear.if.true)
    
    effectt <- "p{Y(X = 1) = 1}"
    
    expect_false(check_linear_objective(contamiv, effectt))
    
    ## regular IV case
    
    graph <- initialize_graph(graph_from_literal(Z -+ X, X -+ Y, Ur -+ X, Ur -+ Y))
    p.vals <- expand.grid(Z = 0:1, X = 0:1, Y = 0:1)
    
    prob.form <- list(out = c("Y", "X"), cond = "Z")
    
    regiv <- create_causalmodel(graph = graph, p.vals = p.vals, prob.form = prob.form)
    expect_true(regiv$counterfactual_constraints$linear.if.true)
    
    effectt <- "p{Y(X = 1) = 1}"
    
    expect_true(check_linear_objective(regiv, effectt))
    

})


test_that("## interventional direct effects", {
    
    
    graph <- initialize_graph(graph_from_literal(A -+ Am, A -+ Ay, Ul -+ A, Ul -+ Am, Ul -+ Ay, 
                            Am -+ M, Ay -+ Y, M -+ Y, Ur -+ M, Ur -+ Y))
    
    
    V(graph)$latent <- c(0, 1,1,1, 0, 0, 1)
    
    p.vals <- expand.grid(A = 0:1, M = 0:1, Y = 0:1)
   
    respvars <- create_response_function(graph)
    respvars$Ay$index <- respvars$Ay$index[1]
    respvars$Ay$values <- respvars$Ay$values[3]
    respvars$Am$index <- respvars$Am$index[1]
    respvars$Am$values <- respvars$Am$values[3]
    
    
    prob.form <- list(out = c("Y", "M"), cond = c("A"))
    medmod <- create_causalmodel(respvars = respvars, p.vals = p.vals, prob.form = prob.form, 
                                 right.vars = c("M", "Y"))
    expect_true(medmod$counterfactual_constraints$linear.if.true)
    
    effectt <- "p{Y(Ay = 1, Am = 1) = 1} - p{Y(Ay = 0, Am = 1) = 1}"
    
    expect_true(check_linear_objective(medmod, effectt))
    
    restest <- optimize_effect_2(create_linearcausalproblem(medmod, effectt))
    
    
})
