library(causaloptim)

test <- COptimization_$new()
test$ParseFileWrap("tests/BinaryACE.tbl")
test$CategorizeConstraints()
test$GaussianElimination()
test$EnumerateVertices()
test$OutputOptimum()
test$Display()




graph <- readRDS("tests/test-graphs/instrument.RData")
obj <- igraph_to_response_function(graph)

balke_optimize(obj)
