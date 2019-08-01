library(causaloptim)

test <- COptimization_$new()
test$ParseFileWrap("test.tbl")
test$CategorizeConstraints()
test$GaussianElimination()
test$EnumerateVertices()
test$OutputOptimum()
test$Display()

