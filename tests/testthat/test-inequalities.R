test_that("## iv inequalities and confounded", {
    
    
    graph <- initialize_graph(graph_from_literal(Z -+ X, X -+ Y, Ur -+ X, Ur -+ Y))
    p.vals <- expand.grid(Z = 0:1, X = 0:1, Y = 0:1)
    prob.form <- list(out = c("X", "Y"), cond = "Z")
    
    iv_mod <- create_causalmodel(graph = graph, p.vals = p.vals, prob.form = prob.form)
    
    expect_true(length(iv_mod$observable_constraints$character) == 4)
    
    graph <- initialize_graph(graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y))
    p.vals <- expand.grid(X = 0:1, Y = 0:1)
    prob.form <- list(out = c("X", "Y"), cond = NULL)
    
    conf_mod <- create_causalmodel(graph = graph, p.vals = p.vals, prob.form = prob.form)
    expect_true(length(conf_mod$observable_constraints$character) == 0)
    
    
    
})