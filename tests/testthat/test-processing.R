test_that("text processing", {
    
    
    
    b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
    V(b)$leftside <- c(0,0,0)
    V(b)$latent <- c(0,0,1)
    V(b)$nvals <- c(3,2,2)
    E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0)
    
    obj2 <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 2) = 1} - p{Y(X = 0) = 1}")
    optimize_effect_2(obj2)
    result <- latex_bounds(bounds = optimize_effect_2(obj2)$bounds, parameters = obj2$parameters)
    
    result_correct <- "\\[ \n \\mbox{Lower bound} =   -P(X = 1, Y = 0) - P(X = 2, Y = 0) - P(X = 0, Y = 1) - P(X = 1, Y = 1) \n \\] \n \\[ \n \\mbox{Upper bound} =   1 - P(X = 2, Y = 0) - P(X = 0, Y = 1) \n \\] \n"
    
    expect_true(identical(result, result_correct))
    
    b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y, Z-+ X, Ul -+ Z)
    V(b)$leftside <- c(0,0,0, 1, 1)
    V(b)$latent <- c(0,0,1, 0, 1)
    V(b)$nvals <- c(2,2,2, 3, 2)
    E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0, 0 ,0)
    
    obj2 <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
    obp <- optimize_effect_2(obj2)
    result <- latex_bounds(bounds = obp$bounds, parameters = obj2$parameters)
    
    expect_true(length(grep("p00_2", result)) == 0)
    
    
    
    graph <- graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y, X -+ M, M -+ Y, Ur -+ M)
    V(graph)$leftside <- c(1, 0, 0, 1, 0, 0)
    V(graph)$latent   <- c(0, 0, 0, 1, 1, 0)
    V(graph)$nvals    <- c(2, 2, 2, 2, 2, 2)
    E(graph)$rlconnect     <- c(0, 0, 0, 0, 0, 0, 0, 0)
    E(graph)$edge.monotone <- c(0, 0, 0, 0, 0, 0, 0, 0)
    V(graph)$outcome <- rep(0, 6)
    V(graph)$outcome[3] <- 1
    V(graph)$exposure <- rep(0, 6)
    V(graph)$exposure[2] <- 1
    
    constraints <- NULL
    
    default <- get_default_effect(graph)
    
    expect_true(length(parse_effect(default)$vars) == 2)
    
    b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y, Z-+ X, Ul -+ Z)
    V(b)$leftside <- c(0,0,0, 1, 1)
    V(b)$latent <- c(0,0,1, 0, 1)
    V(b)$nvals <- c(2,2,2, 3, 2)
    E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0, 0 ,0)
    V(b)$exposure <- c(1, 0, 0, 0, 0)
    V(b)$outcome <- c(0, 1, 0, 0, 0)
    
    get_default_effect(b)
    
    graph <- graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y, X -+ M, M -+ Y, Ur -+ M)
    V(graph)$leftside <- c(1, 0, 0, 1, 0, 0)
    V(graph)$latent   <- c(0, 0, 0, 1, 1, 0)
    V(graph)$nvals    <- c(2, 2, 2, 2, 2, 2)
    E(graph)$rlconnect     <- c(0, 0, 0, 0, 0, 0, 0, 0)
    E(graph)$edge.monotone <- c(0, 0, 0, 0, 0, 0, 0, 0)
    V(graph)$outcome <- rep(0, 6)
    V(graph)$outcome[3] <- 1
    V(graph)$exposure <- rep(0, 6)
    V(graph)$exposure[1] <- 1
    
    constraints <- NULL
    
    default <- get_default_effect(graph)
    expect_true(length(parse_effect(default)$vars) == 2)
    
    
    
})