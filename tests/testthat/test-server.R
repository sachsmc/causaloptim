appDir <- system.file("shiny", "interface", package = "causaloptim")
test_that("dag input 1", {
    testServer(appDir, {
        session$setInputs(
            edges = c(
                id = "2.3",
                source = "Z",
                target = "X",
                sourceLeftside = "TRUE",
                targetLeftside = "FALSE",
                rlconnect = "FALSE",
                sourceLatent = "0",
                targetLatent = "0",
                sourceOutcome = "0",
                targetOutcome = "0",
                sourceExposure = "0",
                targetExposure = "1",
                sourceNvals = "2",
                targetNvals = "2",
                edgeMonotone = "0",
                sourceX = "477",
                sourceY = "229.399993896484",
                targetX = "784",
                targetY = "259.399993896484",
                id = "3.4",
                source = "X",
                target = "Y",
                sourceLeftside = "FALSE",
                targetLeftside = "FALSE",
                rlconnect = "FALSE",
                sourceLatent = "0",
                targetLatent = "0",
                sourceOutcome = "0",
                targetOutcome = "1",
                sourceExposure = "1",
                targetExposure = "0",
                sourceNvals = "2",
                targetNvals = "2",
                edgeMonotone = "0",
                sourceX = "784",
                sourceY = "259.399993896484",
                targetX = "917",
                targetY = "190.399993896484"
            )
        )
        expect_equal(edgeList(),
                     structure(
                         list(
                             id = c("e2.3", "e3.4"),
                             source = c("Z", "X"),
                             target = c("X", "Y"),
                             source.leftside = c(1, 0),
                             target.leftside = c(0, 0),
                             rlconnect = c(0, 0),
                             source.latent = c(0, 0),
                             target.latent = c(0, 0),
                             source.outcome = c(0, 0),
                             target.outcome = c(0, 1),
                             source.exposure = c(0, 1),
                             target.exposure = c(1, 0),
                             source.nvals = c(2, 2),
                             target.nvals = c(2, 2),
                             edge.monotone = c(0, 0),
                             source.x = c(477, 784),
                             source.y = c(229.399993896484, 259.399993896484),
                             target.x = c(784, 917),
                             target.y = c(259.399993896484, 190.399993896484)
                         ),
                         class = "data.frame",
                         row.names = c(NA,-2L)
                     ))
    })
})

test_that("dag input 2", {
    testServer(appDir, {
        session$setInputs(
            edges = c(
                id = "2.3",
                source = "Z",
                target = "X",
                sourceLeftside = "TRUE",
                targetLeftside = "FALSE",
                rlconnect = "FALSE",
                sourceLatent = "0",
                targetLatent = "0",
                sourceOutcome = "0",
                targetOutcome = "0",
                sourceExposure = "0",
                targetExposure = "1",
                sourceNvals = "2",
                targetNvals = "2",
                edgeMonotone = "0",
                sourceX = "555",
                sourceY = "244.399993896484",
                targetX = "779",
                targetY = "289.399993896484",
                id = "3.4",
                source = "X",
                target = "Y",
                sourceLeftside = "FALSE",
                targetLeftside = "FALSE",
                rlconnect = "FALSE",
                sourceLatent = "0",
                targetLatent = "0",
                sourceOutcome = "0",
                targetOutcome = "1",
                sourceExposure = "1",
                targetExposure = "0",
                sourceNvals = "2",
                targetNvals = "2",
                edgeMonotone = "0",
                sourceX = "779",
                sourceY = "289.399993896484",
                targetX = "879",
                targetY = "206.399993896484"
            )
        )
        graphres <- graph_from_literal(Z -+ X, X -+ Y, Ur -+ X, Ur -+ Y)
        V(graphres)$leftside <- c(1,0,0,0)
        V(graphres)$latent <- c(0,0,0,1)
        V(graphres)$outcome <- c(0,0,1,0)
        V(graphres)$exposure <- c(0,1,0,0)
        V(graphres)$nvals <- c(2,2,2,2)
        E(graphres)$rlconnect <- c(0,0,0,0)
        E(graphres)$edge.monotone <- c(0,0,0,0)
        V(graphres)$x <- c(555, 779, 879, 979)
        V(graphres)$y <- c(-244.399993896484,-289.399993896484,-206.399993896484,-269.399993896484)
        returned_igraphobject <- igraphFromList()
        graphres[[10]] <- NULL # ignore environment
        returned_igraphobject[[10]] <- NULL # ignore environment
        expect_equal(returned_igraphobject, graphres)
    })
})

# Example digraph input from GUI:
myin <-
    c(
        id = "2.3",
        source = "Z",
        target = "X",
        sourceLeftside = "TRUE",
        targetLeftside = "FALSE",
        rlconnect = "FALSE",
        sourceLatent = "0",
        targetLatent = "0",
        sourceOutcome = "0",
        targetOutcome = "0",
        sourceExposure = "0",
        targetExposure = "1",
        sourceNvals = "2",
        targetNvals = "2",
        edgeMonotone = "0",
        sourceX = "477",
        sourceY = "229.399993896484",
        targetX = "784",
        targetY = "259.399993896484",
        id = "3.4",
        source = "X",
        target = "Y",
        sourceLeftside = "FALSE",
        targetLeftside = "FALSE",
        rlconnect = "FALSE",
        sourceLatent = "0",
        targetLatent = "0",
        sourceOutcome = "0",
        targetOutcome = "1",
        sourceExposure = "1",
        targetExposure = "0",
        sourceNvals = "2",
        targetNvals = "2",
        edgeMonotone = "0",
        sourceX = "784",
        sourceY = "259.399993896484",
        targetX = "917",
        targetY = "190.399993896484"
    )

# Resulting data frame:
myout0 <-
    structure(
        list(
            id = c("e2.3", "e3.4"),
            source = c("Z", "X"),
            target = c("X", "Y"),
            source.leftside = c(1, 0),
            target.leftside = c(0, 0),
            rlconnect = c(0, 0),
            source.latent = c(0, 0),
            target.latent = c(0, 0),
            source.outcome = c(0, 0),
            target.outcome = c(0, 1),
            source.exposure = c(0, 1),
            target.exposure = c(1, 0),
            source.nvals = c(2, 2),
            target.nvals = c(2, 2),
            edge.monotone = c(0, 0),
            source.x = c(477, 784),
            source.y = c(229.399993896484, 259.399993896484),
            target.x = c(784, 917),
            target.y = c(259.399993896484, 190.399993896484)
        ),
        class = "data.frame",
        row.names = c(NA,-2L)
    )

# source('inst/shiny/interface/global.R')
source(file.path(appDir, "global.R"))
myout1 <- edges_from_input(myin)

test_that("edges_from_input works", {
    test0 <- all(myout0 == myout1)
    expect_equal(object = test0, expected = TRUE)
})

# source('R/graph-utilities.R')
test_that("rlcheck0 works", {
    test1 <- rlcheck0(myout1)
    expect_equal(object = test1, expected = TRUE)
})

myin0 <-
    c(
        id = "2.3",
        source = "Z",
        target = "X",
        sourceLeftside = "TRUE",
        targetLeftside = "FALSE",
        rlconnect = "FALSE",
        sourceLatent = "0",
        targetLatent = "0",
        sourceOutcome = "0",
        targetOutcome = "0",
        sourceExposure = "0",
        targetExposure = "1",
        sourceNvals = "2",
        targetNvals = "2",
        edgeMonotone = "0",
        sourceX = "555",
        sourceY = "244.399993896484",
        targetX = "779",
        targetY = "289.399993896484",
        id = "3.4",
        source = "X",
        target = "Y",
        sourceLeftside = "FALSE",
        targetLeftside = "FALSE",
        rlconnect = "FALSE",
        sourceLatent = "0",
        targetLatent = "0",
        sourceOutcome = "0",
        targetOutcome = "1",
        sourceExposure = "1",
        targetExposure = "0",
        sourceNvals = "2",
        targetNvals = "2",
        edgeMonotone = "0",
        sourceX = "779",
        sourceY = "289.399993896484",
        targetX = "879",
        targetY = "206.399993896484"
    )

edges <-
    structure(
        list(
            id = c("e2.3", "e3.4"),
            source = c("Z", "X"),
            target = c("X", "Y"),
            source.leftside = c(1, 0),
            target.leftside = c(0, 0),
            rlconnect = c(0, 0),
            source.latent = c(0, 0),
            target.latent = c(0, 0),
            source.outcome = c(0, 0),
            target.outcome = c(0, 1),
            source.exposure = c(0, 1),
            target.exposure = c(1, 0),
            source.nvals = c(2, 2),
            target.nvals = c(2, 2),
            edge.monotone = c(0, 0),
            source.x = c(555, 779),
            source.y = c(244.399993896484, 289.399993896484),
            target.x = c(779, 879),
            target.y = c(289.399993896484, 206.399993896484)
        ),
        class = "data.frame",
        row.names = c(NA,-2L)
    )

graphres0 <- graph_from_literal(Z -+ X, X -+ Y, Ur -+ X, Ur -+ Y)
V(graphres0)$leftside <- c(1,0,0,0)
V(graphres0)$latent <- c(0,0,0,1)
V(graphres0)$outcome <- c(0,0,1,0)
V(graphres0)$exposure <- c(0,1,0,0)
V(graphres0)$nvals <- c(2,2,2,2)
V(graphres0)$x <- c(555, 779, 879, 979)
V(graphres0)$y <- c(-244.399993896484,-289.399993896484,-206.399993896484,-269.399993896484)
E(graphres0)$rlconnect <- c(0,0,0,0)
E(graphres0)$edge.monotone <- c(0,0,0,0)

# # source('inst/shiny/interface/global.R')
graphres1 <- graphres_from_edges(edges)

test_that("graphres_from_edges works", {
    test0 <- identical_graphs(graphres0, graphres1)
    expect_equal(object = test0, expected = TRUE)
})

test_that("rlcheck works", {
    test1 <- rlcheck(graphres1)
    expect_equal(object = test1, expected = TRUE)
})

test_that("vertexnamecheck works", {
    graphres_0 <- graphres1
    test0 <- vertexnamecheck(graphres_0)
    expect_equal(object = test0, expected = TRUE)
    graphres_1 <- graph_from_literal(aB1-+C2d)
    test1 <- vertexnamecheck(graphres_1)
    expect_equal(object = test1, expected = TRUE)
    graphres_2 <- graph_from_literal(A-+2)
    expect_message(test2 <- vertexnamecheck(graphres_2))
    expect_equal(object = test2, expected = FALSE)
    graphres_3 <- graph_from_literal(A-+B_2)
    expect_message(test3 <- vertexnamecheck(graphres_3))
    expect_equal(object = test3, expected = FALSE)
})

test_that("cyclecheck works", {
    test0 <- cyclecheck(graphres = graphres1)
    expect_equal(object = test0, expected = TRUE)
    graphres_bad <- graph_from_literal(A-+B, B-+C, C-+A)
    expect_message(test1 <- cyclecheck(graphres = graphres_bad))
    expect_equal(object = test1, expected = FALSE)
})

test_that("graphrescheck works", {
    test0 <- graphrescheck(graphres = graphres1)
    expect_equal(object = test0, expected = TRUE)
})

# Queries:
query0 <- "p{Y(X = 1)=1} - p{Y(X = 0)=1}"
query1 <- "p{{bad_query=+(()}"
query2 <- "p{Y(X(Z=3,A(B=5))) = 12;C=5;C(B=6)=7} - p{Y(X = 0)=1} + p{A=4;B(C=5)=0}"

# source('R/utils.R')
test_that(desc = "parsing query works",
          code = {
              queryparsetest0 <- queryparsecheck(effecttext = query0)
              queryparsetest1 <- queryparsecheck(effecttext = query1)
              queryparsetest2 <- queryparsecheck(effecttext = query2)
              expect_equal(object = queryparsetest0,
                           expected = TRUE)
              expect_equal(object = queryparsetest1,
                           expected = FALSE)
              expect_equal(object = queryparsetest2,
                           expected = TRUE)
          })

# DAGs:

# 0: Two ternary IVs:
graphres0 <- graph_from_literal(Z1-+Z2, Z1-+X, Z2-+X, X-+Y,
                                Ul-+Z1, Ul-+Z2, Ur-+X, Ur-+Y)
V(graphres0)$leftside <- c(1, 1, 0, 0, 1, 0)
V(graphres0)$latent <- c(0, 0, 0, 0, 1, 1)
V(graphres0)$nvals <- c(3, 3, 2, 2, 2, 2)
V(graphres0)$exposure <- c(0, 0, 1, 0, 0, 0)
V(graphres0)$outcome <- c(0, 0, 0, 1, 0, 0)
E(graphres0)$rlconnect <-
    c(0, 0, 0, 0, 0, 0, 0, 0)
E(graphres0)$edge.monotone <-
    c(0, 0, 0, 0, 0, 0, 0, 0)

# 1: Mediation:
graphres1 <- graph_from_literal(X-+Y, X-+M, M-+Y,
                                Ul-+X, Ur-+M, Ur-+Y)
V(graphres1)$leftside <- c(1, 0, 0, 1, 0)
V(graphres1)$latent <- c(0, 0, 0, 1, 1)
V(graphres1)$nvals <- c(2, 2, 2, 2, 2)
V(graphres1)$exposure <- c(0, 0, 0, 0, 0)
V(graphres1)$outcome <- c(0, 0, 0, 0, 0)
E(graphres1)$rlconnect <- c(0, 0, 0, 0, 0, 0)
E(graphres1)$edge.monotone <- c(0, 0, 0, 0, 0, 0)

query10 <- "p{Y(M(X = 1, X = 1))=1} - p{Y(M(X = 0, X = 0))=1}"
# Queries on controlled and natural direct effects:
CDE0_query <- "p{Y(M = 0, X = 1) = 1} - p{Y(M = 0, X = 0) = 1}"
CDE1_query <- "p{Y(M = 1, X = 1) = 1} - p{Y(M = 1, X = 0) = 1}"
NDE0_query <-
    "p{Y(M(X = 0), X = 1) = 1} - p{Y(M(X = 0), X = 0) = 1}"
NDE1_query <-
    "p{Y(M(X = 1), X = 1) = 1} - p{Y(M(X = 1), X = 0) = 1}"

test_that(desc = "checking query conditions works for good queries",
          code = {
              test00 <- querycheck(effecttext = query0,
                                   graphres = graphres0)
              test10 <- querycheck(effecttext = query10,
                                   graphres = graphres1)
              test1C0 <- querycheck(effecttext = CDE0_query,
                                    graphres = graphres1)
              test1C1 <- querycheck(effecttext = CDE1_query,
                                    graphres = graphres1)
              test1N0 <- querycheck(effecttext = NDE0_query,
                                    graphres = graphres1)
              test1N1 <- querycheck(effecttext = NDE1_query,
                                    graphres = graphres1)
              expect_equal(object = test00,
                           expected = TRUE)
              expect_equal(object = test10,
                           expected = TRUE)
              expect_equal(object = test1C0,
                           expected = TRUE)
              expect_equal(object = test1C1,
                           expected = TRUE)
              expect_equal(object = test1N0,
                           expected = TRUE)
              expect_equal(object = test1N1,
                           expected = TRUE)
          })

# Queries that violate conditions:
query3 <- "p{Y(M = 1)=1} - p{Y(M = 0)=1}"
# Cannot intervene on Z1 because it has children on the leftside!
queryB1 <- "p{Y(X(Z2(Z1 = 1, X(Z1 = 1))))=1} - p{Y(X(Z2(Z1 = 0, X(Z1 = 0))))=1}"
queryB2 <- "p{X(Z2(Z1 = 1, Z1 = 1))=1} - p{X(Z2(Z1 = 0, Z1 = 0))=1}"
# Leftside variables not ancestors of intervention sets. Condition 6 violated.
queryB3 <- "p{Y(X(Z2 = 1))=1} - p{Y(X(Z2 = 0))=1}"
queryB4 <- "p{X(Z2 = 1)=1} - p{X(Z2 = 0)=1}"

test_that(desc = "checking query conditions works for bad queries",
          code = {
              expect_message(test13 <- querycheck(effecttext = query3,
                                   graphres = graphres1))
              expect_equal(object = test13,
                           expected = FALSE)
              expect_message(test01 <- querycheck(effecttext = queryB1,
                                   graphres = graphres0))
              expect_equal(object = test01,
                           expected = FALSE)
              expect_message(test02 <- querycheck(effecttext = queryB2,
                                   graphres = graphres0))
              expect_equal(object = test02,
                           expected = FALSE)
              expect_message(test03 <- querycheck(effecttext = queryB3,
                                   graphres = graphres0))
              expect_equal(object = test03,
                           expected = FALSE)
              expect_message(test04 <- querycheck(effecttext = queryB4,
                                   graphres = graphres0))
              expect_equal(object = test04,
                           expected = FALSE)
          })

graphres <- graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y)
V(graphres)$leftside <- c(1, 0, 0, 1, 0)
V(graphres)$latent <- c(0, 0, 0, 1, 1)
V(graphres)$nvals <- c(3, 2, 2, 2, 2)
V(graphres)$exposure <- c(0, 1, 0, 0, 0)
V(graphres)$outcome <- c(0, 0, 1, 0, 0)
E(graphres)$rlconnect <- c(0, 0, 0, 0, 0)
E(graphres)$edge.monotone <- c(0, 0, 0, 0, 0)
constrainttext_good <- "X(Z = 1) >= X(Z = 0)"
constrainttext_parsefail <- "this is not a valid constraint"
constrainttext_parsefail2 <- "X(() = 1) >= X(Z = 0)"
constrainttext_namefail <- "X(Q = 1) >= A(Z = 0)"
constrainttext_operatorfail <- "X(Z = 1) != X(Z = 0)"

test_that(
    desc = "constraint check works",
    code = {
        test_good <- constraintscheck(
            constrainttext = constrainttext_good,
            graphres = graphres
        )
        expect_equal(test_good, TRUE)
        expect_message(test_parsefail <- constraintscheck(
            constrainttext = constrainttext_parsefail,
            graphres = graphres
        ))
        expect_equal(test_parsefail, FALSE)
        expect_message(test_parsefail2 <- constraintscheck(
            constrainttext = constrainttext_parsefail2,
            graphres = graphres
        ))
        expect_equal(test_parsefail2, FALSE)
        expect_message(test_namefail <- constraintscheck(
            constrainttext = constrainttext_namefail,
            graphres = graphres
        ))
        expect_equal(test_namefail, FALSE)
        expect_message(test_operatorfail <- constraintscheck(
            constrainttext = constrainttext_operatorfail,
            graphres = graphres
        ))
        expect_equal(test_operatorfail, FALSE)
    }
)

# Classic Balke-Pearl:                                                                                   })))
# Values after 'downloadf' is clicked:
# myin: input$edges
myin <- c(id = "2.3", 
          source = "X", 
          target = "Y", 
          sourceLeftside = "FALSE", 
          targetLeftside = "FALSE", 
          rlconnect = "FALSE", 
          sourceLatent = "0", 
          targetLatent = "0", 
          sourceOutcome = "0", 
          targetOutcome = "1", 
          sourceExposure = "1", 
          targetExposure = "0", 
          sourceNvals = "2", 
          targetNvals = "2", 
          edgeMonotone = "0", 
          sourceX = "814", 
          sourceY = "226.399993896484", 
          targetX = "982", 
          targetY = "191.399993896484", 
          id = "4.2", 
          source = "Z", 
          target = "X", 
          sourceLeftside = "TRUE", 
          targetLeftside = "FALSE", 
          rlconnect = "FALSE", 
          sourceLatent = "0", 
          targetLatent = "0", 
          sourceOutcome = "0", 
          targetOutcome = "0", 
          sourceExposure = "0", 
          targetExposure = "1", 
          sourceNvals = "2", 
          targetNvals = "2", 
          edgeMonotone = "0", 
          sourceX = "532", 
          sourceY = "199.399993896484", 
          targetX = "814", 
          targetY = "226.399993896484")
# edges: edgeList()
edges <- structure(list(id = c("e2.3", "e4.2"), 
                        source = c("X", "Z"), 
                        target = c("Y", "X"), 
                        source.leftside = c(0, 1), 
                        target.leftside = c(0, 0), 
                        rlconnect = c(0, 0), 
                        source.latent = c(0, 0), 
                        target.latent = c(0, 0), 
                        source.outcome = c(0, 0), 
                        target.outcome = c(1, 0), 
                        source.exposure = c(1, 0), 
                        target.exposure = c(0, 1), 
                        source.nvals = c(2, 2), 
                        target.nvals = c(2, 2), 
                        edge.monotone = c(0, 0), 
                        source.x = c(814, 532), 
                        source.y = c(226.399993896484, 199.399993896484), 
                        target.x = c(982, 814), 
                        target.y = c(191.399993896484, 226.399993896484)), 
                   class = "data.frame", 
                   row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       -2L))
# graphres: igraphFromList()
# graphres <- structure(list(4,
#                            TRUE,
#                            c(0, 1, 3, 3),
#                            c(2, 0, 0, 2),
#                            c(0, 1, 2, 3),
#                            c(1, 2, 0, 3),
#                            c(0, 1, 2, 2, 4),
#                            c(0, 2, 2, 4, 4),
#                            list(c(1, 0, 1),
#                                 structure(list(),
#                                           names = character(0)),
#                                 list(name = c("X", "Z", "Y", "Ur"),
#                                      leftside = c(0, 1, 0, 0),
#                                      latent = c(0, 0, 0, 1),
#                                      outcome = c(0, 0, 1, 0),
#                                      exposure = c(1, 0, 0, 0),
#                                      nvals = c(2, 2, 2, 2),
#                                      x = c(814, 532, 982, 1082),
#                                      y = c(-226.399993896484, -199.399993896484, -191.399993896484, -206.399993896484)),
#                                 list(rlconnect = c(0, 0, 0, 0),
#                                      edge.monotone = c(0, 0, 0, 0))),
#                            "<environment>"),
#                       class = "igraph")
graphres <- make_empty_graph()
graphres <- add_vertices(graphres, nv = 4)
graphres <- add_edges(graph = graphres, edges = c(1,3, 2,1, 4,1, 4,3))
V(graphres)$name <- c("X", "Z", "Y", "Ur")
V(graphres)$leftside <- c(0,1,0,0)
V(graphres)$latent <- c(0,0,0,1)
V(graphres)$outcome <- c(0,0,1,0)
V(graphres)$exposure <- c(1,0,0,0)
V(graphres)$nvals <- c(2,2,2,2)
V(graphres)$x <- c(814, 532, 982, 1082)
V(graphres)$y <- c(-226.399993896484, -199.399993896484, -191.399993896484, -206.399993896484)
E(graphres)$rlconnect <- c(0,0,0,0)
E(graphres)$edge.monotone <- c(0,0,0,0)

# effecttext: input$effect
effecttext <- "p{Y(X = 1)=1} - p{Y(X = 0)=1}"
# resultslist: optimizeGraph()
resultslist <- list(graphres = graphres, 
                    obj = structure(list(variables = c("q0_0", "q0_1", "q0_2", "q0_3", "q1_0", "q1_1", "q1_2", "q1_3", "q2_0", "q2_1", "q2_2", "q2_3", "q3_0", "q3_1", "q3_2", "q3_3"), 
                                         parameters = structure(c("p00_0", "p10_0", "p00_1", "p10_1", "p01_0", "p11_0", "p01_1", "p11_1"), 
                                                                key = "XY_Z", 
                                                                rightvars = c("X", "Y"), 
                                                                condvars = "Z"), 
                                         constraints = structure(c("q0_0 + q0_1 + q0_2 + q0_3 + q1_0 + q1_1 + q1_2 + q1_3 + q2_0 + q2_1 + q2_2 + q2_3 + q3_0 + q3_1 + q3_2 + q3_3  = 1", 
                                                                   "p00_0 = q0_0 + q0_2 + q2_0 + q2_2", "p10_0 = q1_0 + q1_1 + q3_0 + q3_1", 
                                                                   "p00_1 = q0_0 + q0_2 + q1_0 + q1_2", "p10_1 = q2_0 + q2_1 + q3_0 + q3_1", 
                                                                   "p01_0 = q0_1 + q0_3 + q2_1 + q2_3", "p11_0 = q1_2 + q1_3 + q3_2 + q3_3", 
                                                                   "p01_1 = q0_1 + q0_3 + q1_1 + q1_3", "p11_1 = q2_2 + q2_3 + q3_2 + q3_3"), 
                                                                 baseconstr = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)), 
                                         objective = "q0_2 + q1_2 + q2_2 + q3_2 - q0_1 - q1_1 - q2_1 - q3_1", 
                                         p.vals = structure(list(X = c(0L, 1L, 0L, 1L, 0L, 1L, 0L, 1L), 
                                                                 Z = c(0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L), 
                                                                 Y = c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L)), 
                                                            out.attrs = list(dim = c(X = 2L, Z = 2L, Y = 2L), 
                                                                             dimnames = list(X = c("X=0", "X=1"), 
                                                                                             Z = c("Z=0", "Z=1"), 
                                                                                             Y = c("Y=0", "Y=1"))), 
                                                            row.names = c(NA, 8L), 
                                                            class = "data.frame"), 
                                         q.vals = structure(list(X = c(0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L), 
                                                                 Y = c(0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L)), 
                                                            out.attrs = list(dim = c(X = 4L, Y = 4L), 
                                                                             dimnames = list(X = c("X=0", "X=1", "X=2", "X=3"), 
                                                                                             Y = c("Y=0", "Y=1", "Y=2", "Y=3"))), 
                                                            class = "data.frame", 
                                                            row.names = c(NA, -16L)), 
                                         parsed.query = list(vars = list(list(Y = list(X = 1)), list(Y = list(X = 0))), 
                                                             oper = list("-"), 
                                                             values = list(list(Y = 1), list(Y = 1)), 
                                                             pcheck = list(TRUE, TRUE)), 
                                         unparsed.query = "p{Y(X = 1)=1} - p{Y(X = 0)=1}", 
                                         user.constraints = NULL, 
                                         objective.nonreduced = list(c("q0_2", "q1_2", "q2_2", "q3_2"), c("q0_1", "q1_1", "q2_1", "q3_1")), 
                                         response.functions = list(X = list(index = 0:3, 
                                                                            values = list(
                                                                  function (Z = NULL) 
                                                                  {
                                                                      switch(paste0(Z), `0` = 0, `1` = 0)
                                                                  }, function (Z = NULL) 
                                                                  {
                                                                      switch(paste0(Z), `0` = 1, `1` = 0)
                                                                  }, function (Z = NULL) 
                                                                  {
                                                                      switch(paste0(Z), `0` = 0, `1` = 1)
                                                                  }, function (Z = NULL) 
                                                                  {
                                                                      switch(paste0(Z), `0` = 1, `1` = 1)
                                                                  })), Y = list(index = 0:3, values = list(function (X = NULL) 
                                                                  {
                                                                      switch(paste0(X), `0` = 0, `1` = 0)
                                                                  }, function (X = NULL) 
                                                                  {
                                                                      switch(paste0(X), `0` = 1, `1` = 0)
                                                                  }, function (X = NULL) 
                                                                  {
                                                                      switch(paste0(X), `0` = 0, `1` = 1)
                                                                  }, function (X = NULL) 
                                                                  {
                                                                      switch(paste0(X), `0` = 1, `1` = 1)
                                                                  })), Z = list(index = 0:1, values = list(function () 
                                                                  {
                                                                      0
                                                                  }, function () 
                                                                  {
                                                                      1
                                                                  }))), 
                                         graph = graphres, 
       R = structure(c(1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 
                       1, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 
                       0, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 
                       1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 
                       0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 
                       1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 
                       0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 
                       0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1), 
                     dim = c(9L, 16L)), 
       c0 = structure(c(0, -1, 1, 0, 0, -1, 1, 0, 0, -1, 1, 0, 0, -1, 1, 0), 
                      dim = c(16L, 1L))), 
       class = "linearcausalproblem"), 
       bounds.obs = structure(list(bounds = c(lower = "\nMAX {\n  p00_0 - p00_1 - p10_1 - p01_1,\n  p00_0 - p10_0 - p00_1 - p10_1 - p01_0,\n  p00_0 + p10_0 - p00_1 - 2p10_1 - 2p01_1,\n  -p10_1 - p01_1,\n  -p10_0 - p01_0,\n  -p00_0 - 2p10_0 + p00_1 + p10_1 - 2p01_0,\n  -p00_0 - p10_0 + p00_1 - p10_1 - p01_1,\n  -p00_0 - p10_0 + p00_1 - p01_0\n}\n", 
                                              upper = "\nMIN {\n  1 - p10_1 - p01_0,\n  1 + p00_0 + p10_0 - 2p10_1 - p01_1,\n  2 - p10_0 - p00_1 - p10_1 - 2p01_0,\n  1 - p10_1 - p01_1,\n  1 - p10_0 - p01_0,\n  1 - 2p10_0 + p00_1 + p10_1 - p01_0,\n  2 - p00_0 - p10_0 - p10_1 - 2p01_1,\n  1 - p10_0 - p01_1\n}\n"), 
                                   logs = list(lower = list(output = structure(c(0, 0, 0, 
                                                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 
                                                                     1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -1, -1, 0, 0, 0, 
                                                                     0, 0, 0, 0, 0, -1, -1, 1, 1, 1, 0, 0, -1, -1, -1, 0, 0, 1, 
                                                                     1, 0, 0, -1, -1, 0, 0, 0, 0, 1, 0, 0, -1, 1, 0, -1, -2, -1, 
                                                                     -1, 0, 0, 0, 1, 0, 1, -1, 0, 0, 0, 0, -1, 1, 0, -1, -1, -1, 
                                                                     0, 0, 1, 1, 1, 1, 1, 0, 0, -1, -1, 0, 0, 0, 0, 0, 0, 0, 1, 
                                                                     -1, -1, -2, -1, 0, 1, -1, 0, 1, 0, 0, 0, 0, -1, 1, 0, 0, 
                                                                     0, -1, 0, 0, 1, 0, -1, 0, 0, -1, -2, 0, -1, 0, 1, 0, 1, 0, 
                                                                     0, -1, 0, 0, -1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, -1, 0, -2, -1, 0, 0, 
                                                                     -1, 0, 1, 0, 1, 0, 0, -1, 0, 0, -1, 0, 0, 0, 0, 1, 0, 0, 
                                                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
                                                                     dim = c(22L, 11L), 
                                                                     representation = "V"), 
                                                            adjacency = list(c(2L, 3L, 4L, 5L, 12L, 13L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(1L, 5L, 11L, 13L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(1L, 4L, 12L, 14L, 17L, 18L, 19L, 21L, 22L), 
                                                                             c(1L, 3L, 7L, 8L, 12L, 16L, 17L, 19L, 21L, 22L), 
                                                                             c(1L, 2L, 6L, 8L, 9L, 13L, 18L, 20L, 21L, 22L), 
                                                                             c(5L, 8L, 9L, 15L, 17L, 18L, 20L, 21L, 22L), 
                                                                             c(4L, 8L, 10L, 16L, 17L, 19L, 20L, 21L, 22L), 
                                                                             c(4L, 5L, 6L, 7L, 9L, 16L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(5L, 6L, 8L, 10L, 11L, 12L, 13L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(7L, 9L, 12L, 16L, 17L, 19L, 20L, 21L, 22L), 
                                                                             c(2L, 9L, 12L, 13L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(1L, 3L, 4L, 9L, 10L, 11L, 13L, 14L, 16L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(1L, 2L, 5L, 9L, 11L, 12L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(3L, 12L, 13L, 16L, 17L, 18L, 19L, 21L, 22L), 
                                                                             c(6L, 9L, 13L, 16L, 17L, 18L, 20L, 21L, 22L), 
                                                                             c(4L, 7L, 8L, 9L, 10L, 12L, 13L, 14L, 15L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(1L, 3L, 4L, 6L, 7L, 8L, 9L, 10L, 12L, 13L, 14L, 15L, 16L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(1L, 2L, 3L, 5L, 6L, 8L, 9L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 19L, 20L, 21L, 22L), 
                                                                             c(1L, 2L, 3L, 4L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 16L, 17L, 18L, 20L, 21L, 22L), 
                                                                             c(1L, 2L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 15L, 16L, 17L, 18L, 19L, 21L, 22L), 1:22, 1:22), 
                                                            inputadjacency = list(c(2L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 14L, 16L, 17L), 
                                                                                  c(1L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 16L, 17L), 
                                                                                  integer(0), 
                                                                                  integer(0), 
                                                                                  c(1L, 2L, 6L, 7L, 10L, 11L, 12L, 14L, 16L, 17L), 
                                                                                  c(1L, 2L, 5L, 8L, 9L, 11L, 12L, 14L, 16L, 17L), 
                                                                                  c(1L, 2L, 5L, 8L, 9L, 10L, 12L, 14L, 16L, 17L), 
                                                                                  c(1L, 2L, 6L, 7L, 9L, 10L, 11L, 14L, 16L, 17L), 
                                                                                  c(1L, 2L, 6L, 7L, 8L, 10L, 11L, 14L, 16L, 17L), 
                                                                                  c(1L, 2L, 5L, 7L, 8L, 9L, 12L, 14L, 16L, 17L), 
                                                                                  c(1L, 2L, 5L, 6L, 8L, 9L, 12L, 14L, 16L, 17L), 
                                                                                  c(1L, 2L, 5L, 6L, 7L, 10L, 11L, 14L, 16L, 17L), 
                                                                                  integer(0), 
                                                                                  c(1L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 16L, 17L), 
                                                                                  integer(0), 
                                                                                  c(1L, 2L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 14L, 17L), 
                                                                                  c(1L, 2L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 14L, 16L)), 
                                                            incidence = list(c(1L, 2L, 6L, 9L, 10L, 11L, 12L, 14L, 16L), 
                                                                             c(1L, 2L, 6L, 8L, 9L, 11L, 16L), 
                                                                             c(1L, 5L, 6L, 11L, 12L, 14L, 16L), 
                                                                             c(1L, 2L, 5L, 6L, 10L, 12L, 14L, 16L), 
                                                                             c(1L, 2L, 6L, 8L, 9L, 10L, 14L, 16L), 
                                                                             c(1L, 7L, 8L, 9L, 10L, 14L, 16L), 
                                                                             c(1L, 2L, 5L, 7L, 10L, 12L, 16L), 
                                                                             c(1L, 2L, 5L, 6L, 7L, 8L, 10L, 14L, 16L), 
                                                                             c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 13L, 14L, 17L), 
                                                                             c(1L, 2L, 3L, 4L, 5L, 7L, 10L, 12L, 17L), 
                                                                             c(1L, 2L, 3L, 4L, 6L, 8L, 9L, 11L, 17L), 
                                                                             c(1L, 2L, 3L, 4L, 5L, 6L, 9L, 10L, 11L, 12L, 13L, 14L, 17L), 
                                                                             c(2L, 4L, 6L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L), 
                                                                             c(5L, 6L, 11L, 12L, 13L, 14L, 15L, 16L, 17L), 
                                                                             c(7L, 8L, 9L, 10L, 13L, 14L, 15L, 16L, 17L), 
                                                                             c(2L, 4L, 5L, 6L, 7L, 8L, 10L, 12L, 13L, 14L, 15L, 16L, 17L), 
                                                                             c(1L, 3L, 5L, 7L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L), 
                                                                             c(1L, 3L, 5L, 6L, 7L, 8L, 9L, 11L, 13L, 14L, 15L, 16L, 17L), 
                                                                             c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 11L, 12L, 15L, 16L, 17L), 
                                                                             c(1L, 2L, 3L, 4L, 7L, 8L, 9L, 10L, 11L, 12L, 15L, 16L, 17L), 1:17, 1:17), 
                                                            inputincidence = list(c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                                  c(1L, 2L, 4L, 5L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 16L, 19L, 20L, 21L, 22L), 
                                                                                  c(9L, 10L, 11L, 12L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                                  c(9L, 10L, 11L, 12L, 13L, 16L, 19L, 20L, 21L, 22L), 
                                                                                  c(3L, 4L, 7L, 8L, 9L, 10L, 12L, 14L, 16L, 17L, 18L, 19L, 21L, 22L), 
                                                                                  c(1L, 2L, 3L, 4L, 5L, 8L, 9L, 11L, 12L, 13L, 14L, 16L, 18L, 19L, 21L, 22L), 
                                                                                  c(6L, 7L, 8L, 9L, 10L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                                  c(2L, 5L, 6L, 8L, 9L, 11L, 13L, 15L, 16L, 18L, 19L, 20L, 21L, 22L), 
                                                                                  c(1L, 2L, 5L, 6L, 9L, 11L, 12L, 13L, 15L, 17L, 18L, 20L, 21L, 22L), 
                                                                                  c(1L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 12L, 13L, 15L, 16L, 17L, 20L, 21L, 22L), 
                                                                                  c(1L, 2L, 3L, 11L, 12L, 13L, 14L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                                  c(1L, 3L, 4L, 7L, 10L, 12L, 13L, 14L, 16L, 17L, 19L, 20L, 21L, 22L), 
                                                                                  c(9L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 21L, 22L), 
                                                                                  c(1L, 3L, 4L, 5L, 6L, 8L, 9L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 21L, 22L), 13:22, 
                                                                                  c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L), 9:22), 
                                                            input = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, 0, -1, 1, 0, 0, -1, 1, 0, 0, -1, 1, 0, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, -1, 0, 0, 0, 0, 0, -1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 0, 0, 0, 0, 0, -1, -1, 0, 0, -1, 0, -1, 0, -1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 0, -1, -1, 0, 0, 0, -1, 0, -1, 0, 0, 0, 0, 0, -1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 0, 0, 0, 0, 0, -1, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 0, -1, -1), 
                                                                              dim = c(16L, 11L), 
                                                                              representation = "H")), 
                                               upper = list(output = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 2, 1, 1, 1, 2, 0, 1, 0, 1, 0, 0, 0, 1, 1, -1, -1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, -1, 0, -1, 0, 0, 0, 0, 1, 0, -1, 1, 0, 1, 0, 0, 0, 1, 1, -1, 0, -1, -2, -1, -1, 0, 0, -1, 0, 0, 0, 0, -1, 1, 0, 0, 0, 0, -1, 0, 0, -1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, -1, 0, 0, 1, 0, -1, 0, 0, -1, -2, -1, -1, 0, 1, -1, 1, 0, 1, 0, 0, 0, 0, -1, 0, 0, 1, 0, -1, 0, -1, 1, 0, -2, 0, -1, -1, 0, 0, 0, 0, 0, 1, 0, 0, 0, -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, -1, 0, -1, 0, 0, -2, 1, -1, 0, -1, 0, 0, 0, -1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), 
                                                                               dim = c(22L, 11L), 
                                                                               representation = "V"), 
                                                            adjacency = list(c(2L, 3L, 4L, 5L, 6L, 8L, 13L, 14L, 16L, 17L, 18L, 19L, 20L, 21L, 22L), c(1L, 3L, 6L, 7L, 8L, 9L, 17L, 18L, 19L, 20L, 21L, 22L), c(1L, 2L, 4L, 5L, 7L, 9L, 12L, 14L, 16L, 17L, 18L, 19L, 20L, 21L, 22L), c(1L, 3L, 7L, 14L, 18L, 19L, 20L, 21L, 22L), c(1L, 3L, 6L, 16L, 17L, 18L, 19L, 21L, 22L), c(1L, 2L, 5L, 8L, 17L, 18L, 19L, 21L, 22L), 
                                                                             c(2L, 3L, 4L, 9L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(1L, 2L, 6L, 11L, 15L, 16L, 17L, 19L, 21L, 22L), 
                                                                             c(2L, 3L, 7L, 10L, 14L, 15L, 18L, 20L, 21L, 22L), 
                                                                             c(9L, 12L, 14L, 15L, 17L, 18L, 20L, 21L, 22L), 
                                                                             c(8L, 13L, 15L, 16L, 17L, 19L, 20L, 21L, 22L), 
                                                                             c(3L, 10L, 14L, 16L, 17L, 18L, 20L, 21L, 22L), 
                                                                             c(1L, 11L, 14L, 16L, 17L, 19L, 20L, 21L, 22L), 
                                                                             c(1L, 3L, 4L, 9L, 10L, 12L, 13L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(8L, 9L, 10L, 11L, 14L, 16L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(1L, 3L, 5L, 8L, 11L, 12L, 13L, 14L, 15L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(1L, 2L, 3L, 5L, 6L, 8L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 18L, 19L, 20L, 21L, 22L), 
                                                                             c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 9L, 10L, 12L, 14L, 15L, 16L, 17L, 19L, 20L, 21L, 22L), 
                                                                             c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 11L, 13L, 14L, 15L, 16L, 17L, 18L, 20L, 21L, 22L), 
                                                                             c(1L, 2L, 3L, 4L, 7L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 21L, 22L), 1:22, 1:22), 
                                                            inputadjacency = list(integer(0), 
                                                                                  integer(0), 
                                                                                  c(4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 17L), 
                                                                                  c(3L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 15L, 17L), 
                                                                                  c(3L, 4L, 6L, 7L, 10L, 11L, 12L, 13L, 15L, 17L), 
                                                                                  c(3L, 4L, 5L, 8L, 9L, 11L, 12L, 13L, 15L, 17L), 
                                                                                  c(3L, 4L, 5L, 8L, 9L, 10L, 12L, 13L, 15L, 17L), 
                                                                                  c(3L, 4L, 6L, 7L, 9L, 10L, 11L, 13L, 15L, 17L), 
                                                                                  c(3L, 4L, 6L, 7L, 8L, 10L, 11L, 13L, 15L, 17L), 
                                                                                  c(3L, 4L, 5L, 7L, 8L, 9L, 12L, 13L, 15L, 17L), 
                                                                                  c(3L, 4L, 5L, 6L, 8L, 9L, 12L, 13L, 15L, 17L), 
                                                                                  c(3L, 4L, 5L, 6L, 7L, 10L, 11L, 13L, 15L, 17L), 
                                                                                  c(3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 15L, 17L), 
                                                                                  integer(0), 
                                                                                  c(4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 17L), 
                                                                                  integer(0), 
                                                                                  c(3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 15L)), 
                                                            incidence = list(c(1L, 2L, 3L, 4L, 7L, 8L, 9L, 10L, 11L, 12L, 15L, 16L, 17L), 
                                                                             c(3L, 4L, 7L, 9L, 10L, 11L, 12L, 13L, 15L), 
                                                                             c(1L, 3L, 5L, 7L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L), 
                                                                             c(1L, 2L, 3L, 4L, 5L, 7L, 10L, 12L, 17L), 
                                                                             c(7L, 8L, 9L, 10L, 13L, 14L, 15L, 16L, 17L), 
                                                                             c(4L, 7L, 8L, 9L, 10L, 13L, 15L), 
                                                                             c(3L, 4L, 5L, 7L, 10L, 12L, 13L), 
                                                                             c(3L, 4L, 7L, 8L, 9L, 11L, 13L, 15L), 
                                                                             c(3L, 4L, 5L, 7L, 11L, 12L, 13L, 15L), 
                                                                             c(4L, 5L, 6L, 11L, 12L, 13L, 15L), 
                                                                             c(3L, 4L, 6L, 8L, 9L, 11L, 13L), 
                                                                             c(5L, 6L, 11L, 12L, 13L, 14L, 15L, 16L, 17L), 
                                                                             c(1L, 2L, 3L, 4L, 6L, 8L, 9L, 11L, 17L), 
                                                                             c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 11L, 12L, 15L, 16L, 17L), 
                                                                             c(3L, 4L, 5L, 6L, 7L, 8L, 11L, 13L, 15L), 
                                                                             c(1L, 3L, 5L, 6L, 7L, 8L, 9L, 11L, 13L, 14L, 15L, 16L, 17L), 
                                                                             c(2L, 4L, 6L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L), 
                                                                             c(2L, 4L, 5L, 6L, 7L, 8L, 10L, 12L, 13L, 14L, 15L, 16L, 17L), 
                                                                             c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 13L, 14L, 17L), 
                                                                             c(1L, 2L, 3L, 4L, 5L, 6L, 9L, 10L, 11L, 12L, 13L, 14L, 17L), 
                                                                             1:17, 
                                                                             1:17), 
                                                            inputincidence = list(c(1L, 3L, 4L, 13L, 14L, 16L, 19L, 20L, 21L, 22L), 
                                                                                  c(1L, 4L, 13L, 14L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                                  c(1L, 2L, 3L, 4L, 7L, 8L, 9L, 11L, 13L, 14L, 15L, 16L, 19L, 20L, 21L, 22L), 
                                                                                  c(1L, 2L, 4L, 6L, 7L, 8L, 9L, 10L, 11L, 13L, 14L, 15L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                                  c(3L, 4L, 7L, 9L, 10L, 12L, 14L, 15L, 16L, 18L, 19L, 20L, 21L, 22L), 
                                                                                  10:22, 
                                                                                  c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 14L, 15L, 16L, 18L, 19L, 21L, 22L), 
                                                                                  c(1L, 5L, 6L, 8L, 11L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 21L, 22L), 
                                                                                  c(1L, 2L, 3L, 5L, 6L, 8L, 11L, 13L, 16L, 17L, 19L, 20L, 21L, 22L), 
                                                                                  c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                                  c(1L, 2L, 3L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 20L, 21L, 22L), 
                                                                                  c(1L, 2L, 3L, 4L, 7L, 9L, 10L, 12L, 14L, 17L, 18L, 20L, 21L, 22L), 
                                                                                  c(2L, 3L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                                  c(3L, 5L, 12L, 16L, 17L, 18L, 19L, 20L, 21L, 22L), 
                                                                                  c(1L, 2L, 3L, 5L, 6L, 8L, 9L, 10L, 12L, 14L, 15L, 16L, 17L, 18L, 21L, 22L), 
                                                                                  c(1L, 3L, 5L, 12L, 14L, 16L, 17L, 18L, 21L, 22L), 
                                                                                  c(1L, 3L, 4L, 5L, 12L, 13L, 14L, 16L, 17L, 18L, 19L, 20L, 21L, 22L)), 
                                                            input = structure(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 1, -1, 0, 0, 1, -1, 0, 0, 1, -1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1), 
                                                                              dim = c(16L, 11L), 
                                                                              representation = "H")))), 
                              class = "balkebound"), 
       constraints = NULL, 
       effect = "p{Y(X = 1)=1} - p{Y(X = 0)=1}")

test_that(
    desc = "Classic Balke-Pearl works 1",
    code = {
        testServer(
            app = appDir,
            expr = {
                session$setInputs(
                    edges = myin
                )
                edges_expected <- edges
                edges_object <- edgeList()
                expect_equal(
                    object = edges_object,
                    expected = edges_expected
                )
                graphres_expected <- graphres
                graphres_object <- igraphFromList()
                # # Remove environment entry:
                # graphres_expected[[10]] <- NULL
                # graphres_object[[10]] <- NULL
                expect_true(object = igraph::identical_graphs(graphres_object, graphres_expected))
                # expect_equal(
                #     object = graphres_object,
                #     expected = graphres_expected
                # )
            }
        )
    }
)

test_that(
    desc = "Classic Balke-Pearl works 2",
    code = {
        testServer(
            app = appDir,
            expr = {
                session$setInputs(
                    edges = myin
                )
                # edges_expected <- edges
                edges_object <- edgeList()
                # expect_equal(
                #     object = edges_object,
                #     expected = edges_expected
                # )
                graphres_expected <- graphres
                graphres_object <- igraphFromList()
                # # Remove environment entry:
                # graphres_expected[[10]] <- NULL
                # graphres_object[[10]] <- NULL
                # expect_equal(
                #     object = graphres_object,
                #     expected = graphres_expected
                # )
                expect_true(object = igraph::identical_graphs(graphres_object, graphres_expected))
                session$setInputs(
                    effect = effecttext
                    )
                resultslist_expected <- resultslist
                effectFixed$effectt <- effecttext
                resultslist_object <- optimizeGraph()
                # Remove environment info:
                resultslist_expected$graphres[[10]] <- NULL
                resultslist_object$graphres[[10]] <- NULL
                resultslist_expected$obj$graph[[10]] <- NULL
                resultslist_object$obj$graph[[10]] <- NULL
                resultslist_expected$obj$response.functions <- NULL
                resultslist_object$obj$response.functions <- NULL
                # expect_equal(
                #     object = resultslist_object,
                #     expected = resultslist_expected
                # )
            }
        )
    }
)

# Balke-Pearl with monotone IV
constrainttext <- "X(Z = 1) >= X(Z = 0)"
# Values after 'downloadf' is clicked:
# myin: input$edges
myin <- c(id = "2.3", source = "X", target = "Y", sourceLeftside = "FALSE", 
          targetLeftside = "FALSE", rlconnect = "FALSE", sourceLatent = "0", 
          targetLatent = "0", sourceOutcome = "0", targetOutcome = "1", 
          sourceExposure = "1", targetExposure = "0", sourceNvals = "2", 
          targetNvals = "2", edgeMonotone = "0", sourceX = "810", sourceY = "280.399993896484", 
          targetX = "970", targetY = "213.399993896484", id = "4.2", source = "Z", 
          target = "X", sourceLeftside = "TRUE", targetLeftside = "FALSE", 
          rlconnect = "FALSE", sourceLatent = "0", targetLatent = "0", 
          sourceOutcome = "0", targetOutcome = "0", sourceExposure = "0", 
          targetExposure = "1", sourceNvals = "2", targetNvals = "2", edgeMonotone = "0", 
          sourceX = "510", sourceY = "238.399993896484", targetX = "810", 
          targetY = "280.399993896484")
# edges: edgeList()
edges <- structure(list(id = c("e2.3", "e4.2"), source = c("X", "Z"), 
                        target = c("Y", "X"), source.leftside = c(0, 1), 
                        target.leftside = c(0, 0), rlconnect = c(0, 0), 
                        source.latent = c(0, 0), target.latent = c(0, 0), 
                        source.outcome = c(0, 0), target.outcome = c(1, 0), 
                        source.exposure = c(1, 0), target.exposure = c(0, 1), 
                        source.nvals = c(2, 2), target.nvals = c(2, 2), 
                        edge.monotone = c(0, 0), source.x = c(810, 510), 
                        source.y = c(280.399993896484, 238.399993896484), 
                        target.x = c(970, 810), target.y = c(213.399993896484, 280.399993896484)), 
                   class = "data.frame", row.names = c(NA, -2L))
# graphres: igraphFromList()
# graphres <- structure(list(4,
#                            TRUE,
#                            c(0, 1, 3, 3),
#                            c(2, 0, 0, 2),
#                            c(0, 1, 2, 3),
#                            c(1, 2, 0, 3),
#                            c(0, 1, 2, 2, 4),
#                            c(0, 2, 2, 4, 4),
#                            list(c(1, 0, 1),
#                                 structure(list(),
#                                           names = character(0)),
#                                 list(name = c("X", "Z", "Y", "Ur"),
#                                      leftside = c(0, 1, 0, 0),
#                                      latent = c(0, 0, 0, 1),
#                                      outcome = c(0, 0, 1, 0),
#                                      exposure = c(1, 0, 0, 0),
#                                      nvals = c(2, 2, 2, 2),
#                                      x = c(810, 510, 970, 1070),
#                                      y = c(-280.399993896484, -238.399993896484, -213.399993896484, -260.399993896484)),
#                                 list(rlconnect = c(0, 0, 0, 0),
#                                      edge.monotone = c(0, 0, 0, 0))),
#                            "<environment>"),
#                       class = "igraph")
graphres <- make_empty_graph()
graphres <- add_vertices(graphres, nv = 4)
graphres <- add_edges(graph = graphres, edges = c(1,3, 2,1, 4,1, 4,3))
V(graphres)$name <- c("X", "Z", "Y", "Ur")
V(graphres)$leftside <- c(0,1,0,0)
V(graphres)$latent <- c(0,0,0,1)
V(graphres)$outcome <- c(0,0,1,0)
V(graphres)$exposure <- c(1,0,0,0)
V(graphres)$nvals <- c(2,2,2,2)
V(graphres)$x <- c(810, 510, 970, 1070)
V(graphres)$y <- c(-280.399993896484, -238.399993896484, -213.399993896484, -260.399993896484)
E(graphres)$rlconnect <- c(0,0,0,0)
E(graphres)$edge.monotone <- c(0,0,0,0)

# effecttext: input$effect
effecttext <- "p{Y(X = 1)=1} - p{Y(X = 0)=1}"
# resultslist: optimizeGraph()
resultslist <- list(graphres = graphres, obj = structure(list(
                                                                                                                                                                                                                                                                                                                                                       variables = c("q0_0", "q0_1", "q0_2", "q0_3", "q2_0", "q2_1", 
                                                                                                                                                                                                                                                                                                                                                                     "q2_2", "q2_3", "q3_0", "q3_1", "q3_2", "q3_3"), parameters = structure(c("p00_0", 
                                                                                                                                                                                                                                                                                                                                                                                                                                               "p10_0", "p00_1", "p10_1", "p01_0", "p11_0", "p01_1", "p11_1"
                                                                                                                                                                                                                                                                                                                                                                     ), key = "XY_Z", rightvars = c("X", "Y"), condvars = "Z"), 
                                                                                                                                                                                                                                                                                                                                                       constraints = structure(c("q0_0 + q0_1 + q0_2 + q0_3 + q2_0 + q2_1 + q2_2 + q2_3 + q3_0 + q3_1 + q3_2 + q3_3  = 1", 
                                                                                                                                                                                                                                                                                                                                                                                 "p00_0 = q0_0 + q0_2 + q2_0 + q2_2", "p10_0 = q3_0 + q3_1", 
                                                                                                                                                                                                                                                                                                                                                                                 "p00_1 = q0_0 + q0_2", "p10_1 = q2_0 + q2_1 + q3_0 + q3_1", 
                                                                                                                                                                                                                                                                                                                                                                                 "p01_0 = q0_1 + q0_3 + q2_1 + q2_3", "p11_0 = q3_2 + q3_3", 
                                                                                                                                                                                                                                                                                                                                                                                 "p01_1 = q0_1 + q0_3", "p11_1 = q2_2 + q2_3 + q3_2 + q3_3"
                                                                                                                                                                                                                                                                                                                                                       ), baseconstr = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                                                                                                                                                                                                                                                                                                                                                                         TRUE, FALSE)), objective = "q0_2 + q2_2 + q3_2 - q0_1 - q2_1 - q3_1", 
                                                                                                                                                                                                                                                                                                                                                       p.vals = structure(list(X = c(0L, 1L, 0L, 1L, 0L, 1L, 0L, 
                                                                                                                                                                                                                                                                                                                                                                                     1L), Z = c(0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L), Y = c(0L, 0L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                       0L, 0L, 1L, 1L, 1L, 1L)), out.attrs = list(dim = c(X = 2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Z = 2L, Y = 2L), dimnames = list(X = c("X=0", "X=1"), Z = c("Z=0", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      "Z=1"), Y = c("Y=0", "Y=1"))), row.names = c(NA, 8L), class = "data.frame"), 
                                                                                                                                                                                                                                                                                                                                                       q.vals = structure(list(X = c(0L, 2L, 3L, 0L, 2L, 3L, 0L, 
                                                                                                                                                                                                                                                                                                                                                                                     2L, 3L, 0L, 2L, 3L), Y = c(0L, 0L, 0L, 1L, 1L, 1L, 2L, 2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                2L, 3L, 3L, 3L)), out.attrs = list(dim = structure(3:4, names = c("X", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "Y")), dimnames = list(X = c("X=0", "X=2", "X=3"), Y = c("Y=0", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           "Y=1", "Y=2", "Y=3"))), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       -12L)), parsed.query = list(vars = list(list(Y = list(X = 1)), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               list(Y = list(X = 0))), oper = list("-"), values = list(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   list(Y = 1), list(Y = 1)), pcheck = list(TRUE, TRUE)), 
                                                                                                                                                                                                                                                                                                                                                       unparsed.query = "p{Y(X = 1)=1} - p{Y(X = 0)=1}", user.constraints = "X(Z = 1) >= X(Z = 0)", 
                                                                                                                                                                                                                                                                                                                                                       objective.nonreduced = list(c("q0_2", "q2_2", "q3_2"), c("q0_1", 
                                                                                                                                                                                                                                                                                                                                                                                                                "q2_1", "q3_1")), response.functions = list(X = list(index = 0:3, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                     values = list(function (Z = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                         switch(paste0(Z), `0` = 0, `1` = 0), function (Z = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(Z), `0` = 1, `1` = 0), function (Z = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 switch(paste0(Z), `0` = 0, `1` = 1), function (Z = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     switch(paste0(Z), `0` = 1, `1` = 1))), Y = list(index = 0:3, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     values = list(function (X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         switch(paste0(X), `0` = 0, `1` = 0), function (X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(X), `0` = 1, `1` = 0), function (X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 switch(paste0(X), `0` = 0, `1` = 1), function (X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     switch(paste0(X), `0` = 1, `1` = 1))), Z = list(index = 0:1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     values = list(function () 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         0, function () 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             1))), graph = graphres, R = structure(c(1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 1, 1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1), dim = c(9L, 12L)), c0 = structure(c(0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     -1, 1, 0, 0, -1, 1, 0, 0, -1, 1, 0), dim = c(12L, 1L))), class = "linearcausalproblem"), 
                    bounds.obs = structure(list(bounds = c(lower = "\nMAX {\n  p00_0 - p00_1 - p10_1 - p01_1\n}\n", 
                                                           upper = "\nMIN {\n  1 - p10_1 - p01_0\n}\n"), logs = list(
                                                               lower = list(output = structure(c(0, 0, 0, 0, 0, 0, 0, 
                                                                                                 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, 
                                                                                                 0, 0, 0, 0, 0, 0, -1, -1, 1, 1, -1, -1, 0, 0, 0, 0, 0, 
                                                                                                 1, 0, 0, 1, -1, 0, 1, 0, 0, 0, -1, 1, 0, -1, 0, 1, 1, 
                                                                                                 0, 0, -1, 0, 0, 0, 1, -1, 0, 1, 0, -1, 0, 0, 0, 0, 0, 
                                                                                                 1, 0, 1, -1, 0, 0, -1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 
                                                                                                 0, 0, 0, 0, 1, 0, -1, 0, 1, 0, 0, 1, 0, -1, 0, 0, 1, 
                                                                                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(11L, 11L), representation = "V"), 
                                                                            adjacency = list(2:11, c(1L, 3L, 4L, 5L, 6L, 7L, 
                                                                                                     8L, 9L, 10L, 11L), c(1L, 2L, 4L, 6L, 7L, 8L, 9L, 
                                                                                                                          10L, 11L), c(1L, 2L, 3L, 5L, 7L, 8L, 9L, 10L, 11L
                                                                                                                          ), c(1L, 2L, 4L, 6L, 7L, 8L, 9L, 10L, 11L), c(1L, 
                                                                                                                                                                        2L, 3L, 5L, 7L, 8L, 9L, 10L, 11L), c(1L, 2L, 3L, 
                                                                                                                                                                                                             4L, 5L, 6L, 8L, 9L, 10L, 11L), c(1L, 2L, 3L, 4L, 
                                                                                                                                                                                                                                              5L, 6L, 7L, 9L, 10L, 11L), c(1L, 2L, 3L, 4L, 5L, 
                                                                                                                                                                                                                                                                           6L, 7L, 8L, 10L, 11L), 1:11, 1:11), inputadjacency = list(
                                                                                                                                                                                                                                                                               c(2L, 5L, 6L, 7L, 8L, 10L, 12L, 13L), c(1L, 5L, 
                                                                                                                                                                                                                                                                                                                       6L, 7L, 8L, 10L, 12L, 13L), integer(0), integer(0), 
                                                                                                                                                                                                                                                                               c(1L, 2L, 6L, 7L, 10L, 12L, 13L), c(1L, 2L, 5L, 
                                                                                                                                                                                                                                                                                                                   8L, 10L, 12L, 13L), c(1L, 2L, 5L, 8L, 10L, 12L, 
                                                                                                                                                                                                                                                                                                                                         13L), c(1L, 2L, 6L, 7L, 10L, 12L, 13L), integer(0), 
                                                                                                                                                                                                                                                                               c(1L, 2L, 5L, 6L, 7L, 8L, 12L, 13L), integer(0), 
                                                                                                                                                                                                                                                                               c(1L, 2L, 5L, 6L, 7L, 8L, 10L, 13L), c(1L, 2L, 
                                                                                                                                                                                                                                                                                                                      5L, 6L, 7L, 8L, 10L, 12L)), incidence = list(
                                                                                                                                                                                                                                                                                                                          c(1L, 2L, 5L, 6L, 7L, 8L, 10L, 12L), c(1L, 2L, 
                                                                                                                                                                                                                                                                                                                                                                 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 13L), c(1L, 
                                                                                                                                                                                                                                                                                                                                                                                                          2L, 3L, 4L, 5L, 6L, 9L, 10L, 11L, 12L, 13L), 
                                                                                                                                                                                                                                                                                                                          c(1L, 2L, 3L, 4L, 6L, 8L, 9L, 10L, 11L, 12L, 
                                                                                                                                                                                                                                                                                                                            13L), c(1L, 2L, 3L, 4L, 7L, 8L, 9L, 10L, 11L, 
                                                                                                                                                                                                                                                                                                                                    12L, 13L), c(1L, 2L, 3L, 4L, 5L, 7L, 9L, 10L, 
                                                                                                                                                                                                                                                                                                                                                 11L, 12L, 13L), c(2L, 4L, 5L, 6L, 7L, 8L, 9L, 
                                                                                                                                                                                                                                                                                                                                                                   10L, 11L, 12L, 13L), c(1L, 3L, 5L, 6L, 7L, 8L, 
                                                                                                                                                                                                                                                                                                                                                                                          9L, 10L, 11L, 12L, 13L), c(1L, 2L, 3L, 4L, 5L, 
                                                                                                                                                                                                                                                                                                                                                                                                                     6L, 7L, 8L, 11L, 12L, 13L), 1:13, 1:13), inputincidence = list(
                                                                                                                                                                                                                                                                                                                                                                                                                         c(1L, 2L, 3L, 4L, 5L, 6L, 8L, 9L, 10L, 11L), 
                                                                                                                                                                                                                                                                                                                                                                                                                         c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 9L, 10L, 11L), 
                                                                                                                                                                                                                                                                                                                                                                                                                         c(2L, 3L, 4L, 5L, 6L, 8L, 9L, 10L, 11L), c(2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                    3L, 4L, 5L, 6L, 7L, 9L, 10L, 11L), c(1L, 2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         3L, 6L, 7L, 8L, 9L, 10L, 11L), c(1L, 2L, 3L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          4L, 7L, 8L, 9L, 10L, 11L), c(1L, 2L, 5L, 6L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       7L, 8L, 9L, 10L, 11L), c(1L, 2L, 4L, 5L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                8L, 9L, 10L, 11L), c(2L, 3L, 4L, 5L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     8L, 10L, 11L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      8L, 10L, 11L), 3:11, c(1L, 3L, 4L, 5L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             8L, 9L, 10L, 11L), 2:11), input = structure(c(0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 1, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           -1, 1, 0, 0, -1, 1, 0, -1, -1, -1, -1, -1, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           -1, -1, -1, -1, -1, -1, 0, -1, 0, -1, 0, -1, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 0, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           -1, 0, 0, -1, -1, 0, 0, 0, -1, 0, -1, 0, -1, 0, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           0, -1, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           0, 0, -1, -1, 0, 0, -1, -1), dim = 12:11, representation = "H")), 
                                                               upper = list(output = structure(c(0, 0, 0, 0, 0, 0, 0, 
                                                                                                 0, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                 0, 0, 1, 0, 1, 0, -1, -1, 0, 0, 1, 1, 0, 0, 0, -1, 0, 
                                                                                                 1, 0, 0, -1, 0, 1, 0, 0, 0, -1, 1, 1, 0, 0, 0, -1, -1, 
                                                                                                 0, 0, 1, 0, 0, 0, 1, 0, 1, 0, -1, 0, -1, 0, 0, 0, 0, 
                                                                                                 1, 1, 0, 0, 1, 0, -1, 0, -1, 0, 1, 0, 0, 0, 0, 0, 0, 
                                                                                                 0, 0, 0, 0, 1, 0, -1, 0, 0, -1, 1, 0, 0, 0, 0, 0, 1, 
                                                                                                 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(11L, 11L), representation = "V"), 
                                                                            adjacency = list(c(2L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 
                                                                                               11L), c(1L, 3L, 5L, 6L, 7L, 8L, 9L, 10L, 11L), c(2L, 
                                                                                                                                                4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L), c(1L, 3L, 5L, 
                                                                                                                                                                                     6L, 7L, 8L, 9L, 10L, 11L), c(1L, 2L, 3L, 4L, 6L, 
                                                                                                                                                                                                                  7L, 8L, 9L, 10L, 11L), c(1L, 2L, 3L, 4L, 5L, 7L, 
                                                                                                                                                                                                                                           8L, 9L, 10L, 11L), c(1L, 2L, 3L, 4L, 5L, 6L, 8L, 
                                                                                                                                                                                                                                                                9L, 10L, 11L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 9L, 
                                                                                                                                                                                                                                                                                 10L, 11L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 10L, 
                                                                                                                                                                                                                                                                                              11L), 1:11, 1:11), inputadjacency = list(integer(0), 
                                                                                                                                                                                                                                                                                                                                       integer(0), c(4L, 5L, 6L, 7L, 8L, 9L, 11L, 13L
                                                                                                                                                                                                                                                                                                                                       ), c(3L, 5L, 6L, 7L, 8L, 9L, 11L, 13L), c(3L, 
                                                                                                                                                                                                                                                                                                                                                                                 4L, 6L, 7L, 9L, 11L, 13L), c(3L, 4L, 5L, 8L, 
                                                                                                                                                                                                                                                                                                                                                                                                              9L, 11L, 13L), c(3L, 4L, 5L, 8L, 9L, 11L, 13L
                                                                                                                                                                                                                                                                                                                                                                                                              ), c(3L, 4L, 6L, 7L, 9L, 11L, 13L), c(3L, 4L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                    5L, 6L, 7L, 8L, 11L, 13L), integer(0), c(3L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             4L, 5L, 6L, 7L, 8L, 9L, 13L), integer(0), c(3L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         4L, 5L, 6L, 7L, 8L, 9L, 11L)), incidence = list(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             c(1L, 2L, 3L, 4L, 5L, 7L, 9L, 10L, 11L, 12L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               13L), c(1L, 2L, 3L, 4L, 7L, 8L, 9L, 10L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       12L, 13L), c(1L, 2L, 3L, 4L, 6L, 8L, 9L, 10L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    11L, 12L, 13L), c(1L, 2L, 3L, 4L, 5L, 6L, 9L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      10L, 11L, 12L, 13L), c(1L, 3L, 5L, 6L, 7L, 8L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             9L, 10L, 11L, 12L, 13L), c(3L, 4L, 5L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        8L, 9L, 11L), c(2L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        11L, 12L, 13L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          8L, 9L, 10L, 13L), c(1L, 2L, 3L, 4L, 5L, 6L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               7L, 8L, 11L, 12L, 13L), 1:13, 1:13), inputincidence = list(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   c(1L, 2L, 3L, 4L, 5L, 8L, 9L, 10L, 11L), c(1L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              2L, 3L, 4L, 7L, 8L, 9L, 10L, 11L), c(1L, 2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   3L, 4L, 5L, 6L, 8L, 9L, 10L, 11L), c(1L, 2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        3L, 4L, 6L, 7L, 8L, 9L, 10L, 11L), c(1L, 4L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             5L, 6L, 7L, 8L, 9L, 10L, 11L), 3:11, c(1L, 2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    5L, 6L, 7L, 8L, 9L, 10L, 11L), c(2L, 3L, 5L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     6L, 7L, 8L, 9L, 10L, 11L), c(1L, 2L, 3L, 4L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  5L, 6L, 7L, 8L, 10L, 11L), c(1L, 2L, 3L, 4L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               5L, 7L, 8L, 10L, 11L), c(1L, 2L, 3L, 4L, 5L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        6L, 7L, 9L, 10L, 11L), c(1L, 2L, 3L, 4L, 5L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 7L, 9L, 10L, 11L), c(1L, 2L, 3L, 4L, 5L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      8L, 9L, 10L, 11L)), input = structure(c(0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -1, 0, 0, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              -1, 0, 0, 1, -1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1), dim = 12:11, representation = "H")))), class = "balkebound"), 
                    constraints = "X(Z = 1) >= X(Z = 0)", effect = "p{Y(X = 1)=1} - p{Y(X = 0)=1}")
# constrainttext: strsplit(x = input$constraintfield, split = "\n", fixed = TRUE)[[1]]
# constrainttext: input$constraintfield
constrainttext <- "X(Z = 1) >= X(Z = 0)"
test_that(
    desc = "Classic Balke-Pearl works with monotone IV",
    code = {
        testServer(
            app = appDir,
            expr = {
                session$setInputs(
                    edges = myin
                )
                edges_expected <- edges
                edges_object <- edgeList()
                expect_equal(
                    object = edges_object,
                    expected = edges_expected
                )
                graphres_expected <- graphres
                graphres_object <- igraphFromList()
                # # Remove environment entry:
                # graphres_expected[[10]] <- NULL
                # graphres_object[[10]] <- NULL
                # expect_equal(
                #     object = graphres_object,
                #     expected = graphres_expected
                # )
                expect_true(object = igraph::identical_graphs(g1 = graphres_object, g2 = graphres_expected))
                session$setInputs(
                    effect = effecttext
                )
                effectFixed$effectt <- effecttext
                session$setInputs(
                    constraints = constrainttext
                )
                fixedConstraints$constraintst <- constrainttext
                resultslist_expected <- resultslist
                resultslist_object <- optimizeGraph()
                # Remove environment info:
                resultslist_expected$graphres[[10]] <- NULL
                resultslist_object$graphres[[10]] <- NULL
                resultslist_expected$obj$graph[[10]] <- NULL
                resultslist_object$obj$graph[[10]] <- NULL
                resultslist_expected$obj$response.functions <- NULL
                resultslist_object$obj$response.functions <- NULL
                # expect_equal(
                #     object = resultslist_object,
                #     expected = resultslist_expected
                # )
            }
        )
    }
)

# Natural direct effect:
# Values after 'downloadf' is clicked:
# myin: input$edges
myin <- c(id = "2.3", source = "X", target = "Y", sourceLeftside = "TRUE", 
          targetLeftside = "FALSE", rlconnect = "FALSE", sourceLatent = "0", 
          targetLatent = "0", sourceOutcome = "0", targetOutcome = "0", 
          sourceExposure = "0", targetExposure = "0", sourceNvals = "2", 
          targetNvals = "2", edgeMonotone = "0", sourceX = "499", sourceY = "297.399993896484", 
          targetX = "973", targetY = "377.399993896484", id = "2.4", source = "X", 
          target = "M", sourceLeftside = "TRUE", targetLeftside = "FALSE", 
          rlconnect = "FALSE", sourceLatent = "0", targetLatent = "0", 
          sourceOutcome = "0", targetOutcome = "0", sourceExposure = "0", 
          targetExposure = "0", sourceNvals = "2", targetNvals = "2", edgeMonotone = "0", 
          sourceX = "499", sourceY = "297.399993896484", targetX = "793", 
          targetY = "138.399993896484", id = "4.3", source = "M", target = "Y", 
          sourceLeftside = "FALSE", targetLeftside = "FALSE", rlconnect = "FALSE", 
          sourceLatent = "0", targetLatent = "0", sourceOutcome = "0", 
          targetOutcome = "0", sourceExposure = "0", targetExposure = "0", 
          sourceNvals = "2", targetNvals = "2", edgeMonotone = "0", sourceX = "793", 
          sourceY = "138.399993896484", targetX = "973", targetY = "377.399993896484"
)
# edges: edgeList()
edges <- structure(list(id = c("e2.3", "e2.4", "e4.3"), 
                        source = c("X", "X", "M"), 
                        target = c("Y", "M", "Y"), 
                        source.leftside = c(1, 1, 0), 
                        target.leftside = c(0, 0, 0), 
                        rlconnect = c(0, 0, 0), 
                        source.latent = c(0, 0, 0), 
                        target.latent = c(0, 0, 0), 
                        source.outcome = c(0, 0, 0), 
                        target.outcome = c(0, 0, 0), 
                        source.exposure = c(0, 0, 0), 
                        target.exposure = c(0, 0, 0), 
                        source.nvals = c(2, 2, 2), 
                        target.nvals = c(2, 2, 2), 
                        edge.monotone = c(0, 0, 0), 
                        source.x = c(499, 499, 793), 
                        source.y = c(297.399993896484, 297.399993896484, 138.399993896484), 
                        target.x = c(973, 793, 973), 
                        target.y = c(377.399993896484, 138.399993896484, 377.399993896484)), 
                   class = "data.frame", 
                   row.names = c(NA, -3L))
# graphres: igraphFromList()
# graphres <- structure(list(4, 
#                            TRUE, 
#                            c(0, 0, 1, 3, 3), 
#                            c(2, 1, 2, 1, 2), 
#                            c(1, 0, 2, 3, 4), 
#                            c(1, 3, 0, 2, 4), 
#                            c(0, 2, 3, 3, 5), 
#                            c(0, 0, 2, 5, 5), 
#                            list(c(1, 0, 1), 
#                                 structure(list(), 
#                                           names = character(0)), 
#                                 list(name = c("X", "M", "Y", "Ur"), 
#                                      leftside = c(1, 0, 0, 0), 
#                                      latent = c(0, 0, 0, 1), 
#                                      outcome = c(0, 0, 0, 0), 
#                                      exposure = c(0, 0, 0, 0), 
#                                      nvals = c(2, 2, 2, 2), 
#                                      x = c(499, 793, 973, 1073), 
#                                      y = c(-297.399993896484, -138.399993896484, -377.399993896484, -357.399993896484)), 
#                                 list(rlconnect = c(0, 0, 0, 0, 0), 
#                                      edge.monotone = c(0, 0, 0, 0, 0))), 
#                            "<environment>"), 
#                       class = "igraph")
# effecttext: input$effect
effecttext <- "p{Y(M(X = 0), X = 1) = 1} - p{Y(M(X = 0), X = 0) = 1}"
# resultslist: optimizeGraph()
graphres2 <- make_empty_graph()
graphres2 <- add_vertices(graphres2, nv = 4)
graphres2 <- add_edges(graph = graphres2, edges = c(1,3, 1,2, 2,3, 4,2, 4,3))
V(graphres2)$name <- c("X", "M", "Y", "Ur")
V(graphres2)$leftside <- c(1,0,0,0)
V(graphres2)$latent <- c(0,0,0,1)
V(graphres2)$outcome <- c(0,0,0,0)
V(graphres2)$exposure <- c(0,0,0,0)
V(graphres2)$nvals <- c(2,2,2,2)
V(graphres2)$x <- c(499, 793, 973, 1073)
V(graphres2)$y <- c(-297.399993896484, -138.399993896484, -377.399993896484, -357.399993896484)
E(graphres2)$rlconnect <- c(0,0,0,0,0)
E(graphres2)$edge.monotone <- c(0,0,0,0,0)
resultslist <- list(graphres = graphres2, 
                    obj = structure(list(
                                                                                                                                                                                                                                                                                                                                                                   variables = c("q0_0", "q0_1", "q0_10", "q0_11", "q0_12", 
                                                                                                                                                                                                                                                                                                                                                                                 "q0_13", "q0_14", "q0_15", "q0_2", "q0_3", "q0_4", "q0_5", 
                                                                                                                                                                                                                                                                                                                                                                                 "q0_6", "q0_7", "q0_8", "q0_9", "q1_0", "q1_1", "q1_10", 
                                                                                                                                                                                                                                                                                                                                                                                 "q1_11", "q1_12", "q1_13", "q1_14", "q1_15", "q1_2", "q1_3", 
                                                                                                                                                                                                                                                                                                                                                                                 "q1_4", "q1_5", "q1_6", "q1_7", "q1_8", "q1_9", "q2_0", "q2_1", 
                                                                                                                                                                                                                                                                                                                                                                                 "q2_10", "q2_11", "q2_12", "q2_13", "q2_14", "q2_15", "q2_2", 
                                                                                                                                                                                                                                                                                                                                                                                 "q2_3", "q2_4", "q2_5", "q2_6", "q2_7", "q2_8", "q2_9", "q3_0", 
                                                                                                                                                                                                                                                                                                                                                                                 "q3_1", "q3_10", "q3_11", "q3_12", "q3_13", "q3_14", "q3_15", 
                                                                                                                                                                                                                                                                                                                                                                                 "q3_2", "q3_3", "q3_4", "q3_5", "q3_6", "q3_7", "q3_8", "q3_9"
                                                                                                                                                                                                                                                                                                                                                                   ), parameters = structure(c("p00_0", "p00_1", "p10_0", "p10_1", 
                                                                                                                                                                                                                                                                                                                                                                                               "p01_0", "p01_1", "p11_0", "p11_1"), key = "MY_X", rightvars = c("M", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                "Y"), condvars = "X"), constraints = structure(c("q0_0 + q0_1 + q0_10 + q0_11 + q0_12 + q0_13 + q0_14 + q0_15 + q0_2 + q0_3 + q0_4 + q0_5 + q0_6 + q0_7 + q0_8 + q0_9 + q1_0 + q1_1 + q1_10 + q1_11 + q1_12 + q1_13 + q1_14 + q1_15 + q1_2 + q1_3 + q1_4 + q1_5 + q1_6 + q1_7 + q1_8 + q1_9 + q2_0 + q2_1 + q2_10 + q2_11 + q2_12 + q2_13 + q2_14 + q2_15 + q2_2 + q2_3 + q2_4 + q2_5 + q2_6 + q2_7 + q2_8 + q2_9 + q3_0 + q3_1 + q3_10 + q3_11 + q3_12 + q3_13 + q3_14 + q3_15 + q3_2 + q3_3 + q3_4 + q3_5 + q3_6 + q3_7 + q3_8 + q3_9  = 1", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "p00_0 = q0_0 + q0_10 + q0_12 + q0_14 + q0_2 + q0_4 + q0_6 + q0_8 + q2_0 + q2_10 + q2_12 + q2_14 + q2_2 + q2_4 + q2_6 + q2_8", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "p00_1 = q0_0 + q0_1 + q0_10 + q0_11 + q0_2 + q0_3 + q0_8 + q0_9 + q1_0 + q1_1 + q1_10 + q1_11 + q1_2 + q1_3 + q1_8 + q1_9", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "p10_0 = q1_0 + q1_1 + q1_12 + q1_13 + q1_4 + q1_5 + q1_8 + q1_9 + q3_0 + q3_1 + q3_12 + q3_13 + q3_4 + q3_5 + q3_8 + q3_9", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "p10_1 = q2_0 + q2_1 + q2_2 + q2_3 + q2_4 + q2_5 + q2_6 + q2_7 + q3_0 + q3_1 + q3_2 + q3_3 + q3_4 + q3_5 + q3_6 + q3_7", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "p01_0 = q0_1 + q0_11 + q0_13 + q0_15 + q0_3 + q0_5 + q0_7 + q0_9 + q2_1 + q2_11 + q2_13 + q2_15 + q2_3 + q2_5 + q2_7 + q2_9", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "p01_1 = q0_12 + q0_13 + q0_14 + q0_15 + q0_4 + q0_5 + q0_6 + q0_7 + q1_12 + q1_13 + q1_14 + q1_15 + q1_4 + q1_5 + q1_6 + q1_7", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "p11_0 = q1_10 + q1_11 + q1_14 + q1_15 + q1_2 + q1_3 + q1_6 + q1_7 + q3_10 + q3_11 + q3_14 + q3_15 + q3_2 + q3_3 + q3_6 + q3_7", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 "p11_1 = q2_10 + q2_11 + q2_12 + q2_13 + q2_14 + q2_15 + q2_8 + q2_9 + q3_10 + q3_11 + q3_12 + q3_13 + q3_14 + q3_15 + q3_8 + q3_9"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                ), baseconstr = c(TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  TRUE, FALSE)), objective = "q0_12 + q0_14 + q0_4 + q0_6 + q1_12 + q1_13 + q1_8 + q1_9 + q2_12 + q2_14 + q2_4 + q2_6 + q3_12 + q3_13 + q3_8 + q3_9 - q0_1 - q0_11 - q0_3 - q0_9 - q1_2 - q1_3 - q1_6 - q1_7 - q2_1 - q2_11 - q2_3 - q2_9 - q3_2 - q3_3 - q3_6 - q3_7", 
                                                                                                                                                                                                                                                                                                                                                                   p.vals = structure(list(X = c(0L, 1L, 0L, 1L, 0L, 1L, 0L, 
                                                                                                                                                                                                                                                                                                                                                                                                 1L), M = c(0L, 0L, 1L, 1L, 0L, 0L, 1L, 1L), Y = c(0L, 0L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                   0L, 0L, 1L, 1L, 1L, 1L)), out.attrs = list(dim = c(X = 2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      M = 2L, Y = 2L), dimnames = list(X = c("X=0", "X=1"), M = c("M=0", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  "M=1"), Y = c("Y=0", "Y=1"))), row.names = c(NA, 8L), class = "data.frame"), 
                                                                                                                                                                                                                                                                                                                                                                   q.vals = structure(list(M = c(0L, 1L, 2L, 3L, 0L, 1L, 2L, 
                                                                                                                                                                                                                                                                                                                                                                                                 3L, 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L, 0L, 1L, 
                                                                                                                                                                                                                                                                                                                                                                                                 2L, 3L, 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L, 0L, 
                                                                                                                                                                                                                                                                                                                                                                                                 1L, 2L, 3L, 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L, 
                                                                                                                                                                                                                                                                                                                                                                                                 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L, 0L, 1L, 2L, 3L), Y = c(0L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                        0L, 0L, 0L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                        4L, 4L, 4L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 7L, 7L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                        7L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 10L, 10L, 10L, 10L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                        11L, 11L, 11L, 12L, 12L, 12L, 12L, 13L, 13L, 13L, 13L, 14L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                        14L, 14L, 14L, 15L, 15L, 15L, 15L)), out.attrs = list(dim = c(M = 4L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Y = 16L), dimnames = list(M = c("M=0", "M=1", "M=2", "M=3"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ), Y = c("Y= 0", "Y= 1", "Y= 2", "Y= 3", "Y= 4", "Y= 5", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Y= 6", "Y= 7", "Y= 8", "Y= 9", "Y=10", "Y=11", "Y=12", "Y=13", 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               "Y=14", "Y=15"))), class = "data.frame", row.names = c(NA, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      -64L)), parsed.query = list(vars = list(list(Y = list(M = list(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          X = 0), X = 1)), list(Y = list(M = list(X = 0), X = 0))), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          oper = list("-"), values = list(list(Y = 1), list(Y = 1)), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          pcheck = list(TRUE, TRUE)), unparsed.query = "p{Y(M(X = 0), X = 1) = 1} - p{Y(M(X = 0), X = 0) = 1}", 
                                                                                                                                                                                                                                                                                                                                                                   user.constraints = NULL, objective.nonreduced = list(c("q0_12", 
                                                                                                                                                                                                                                                                                                                                                                                                                          "q0_14", "q0_4", "q0_6", "q1_12", "q1_13", "q1_8", "q1_9", 
                                                                                                                                                                                                                                                                                                                                                                                                                          "q2_12", "q2_14", "q2_4", "q2_6", "q3_12", "q3_13", "q3_8", 
                                                                                                                                                                                                                                                                                                                                                                                                                          "q3_9"), c("q0_1", "q0_11", "q0_3", "q0_9", "q1_2", "q1_3", 
                                                                                                                                                                                                                                                                                                                                                                                                                                     "q1_6", "q1_7", "q2_1", "q2_11", "q2_3", "q2_9", "q3_2", 
                                                                                                                                                                                                                                                                                                                                                                                                                                     "q3_3", "q3_6", "q3_7")), response.functions = list(M = list(
                                                                                                                                                                                                                                                                                                                                                                                                                                         index = 0:3, values = list(function (X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(X), `0` = 0, `1` = 0)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(X), `0` = 1, `1` = 0)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(X), `0` = 0, `1` = 1)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(X), `0` = 1, `1` = 1)
                                                                                                                                                                                                                                                                                                                                                                                                                                         })), Y = list(index = 0:15, values = list(function (M = NULL, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 0, `10` = 0, `01` = 0, `11` = 0)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 1, `10` = 0, `01` = 0, `11` = 0)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 0, `10` = 1, `01` = 0, `11` = 0)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 1, `10` = 1, `01` = 0, `11` = 0)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 0, `10` = 0, `01` = 1, `11` = 0)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 1, `10` = 0, `01` = 1, `11` = 0)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 0, `10` = 1, `01` = 1, `11` = 0)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 1, `10` = 1, `01` = 1, `11` = 0)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 0, `10` = 0, `01` = 0, `11` = 1)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 1, `10` = 0, `01` = 0, `11` = 1)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 0, `10` = 1, `01` = 0, `11` = 1)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 1, `10` = 1, `01` = 0, `11` = 1)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 0, `10` = 0, `01` = 1, `11` = 1)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 1, `10` = 0, `01` = 1, `11` = 1)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 0, `10` = 1, `01` = 1, `11` = 1)
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function (M = NULL, X = NULL) 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             switch(paste0(M, X), `00` = 1, `10` = 1, `01` = 1, `11` = 1)
                                                                                                                                                                                                                                                                                                                                                                                                                                         })), X = list(index = 0:1, values = list(function () 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             0
                                                                                                                                                                                                                                                                                                                                                                                                                                         }, function () 
                                                                                                                                                                                                                                                                                                                                                                                                                                         {
                                                                                                                                                                                                                                                                                                                                                                                                                                             1
                                                                                                                                                                                                                                                                                                                                                                                                                                         }))), graph = graphres2, 
                                                                                                                                                                                                                                                                                                                                                                   R = structure(c(1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   1, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 
                                                                                                                                                                                                                                                                                                                                                                                   1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                   1, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 1, 
                                                                                                                                                                                                                                                                                                                                                                                   1, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 0, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   1, 1, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   1, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   1, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 0, 1, 1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 
                                                                                                                                                                                                                                                                                                                                                                                   1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                   0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 1), dim = c(9L, 64L)), c0 = structure(c(0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                         -1, 0, -1, 1, 0, 1, 0, 0, -1, 1, 0, 1, 0, 0, -1, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                         0, 1, 1, 0, 0, -1, -1, 0, 0, -1, -1, 1, 1, 0, -1, 0, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                         1, 0, 1, 0, 0, -1, 1, 0, 1, 0, 0, -1, 0, 0, 0, 0, 1, 1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                         0, -1, -1, 0, 0, -1, -1, 1, 1), dim = c(64L, 1L))), class = "linearcausalproblem"), 
                    bounds.obs = structure(list(bounds = c(lower = "\nMAX {\n  -p00_1 + p10_0 - p10_1 - p01_0 - p01_1,\n  -2 + 2p00_0 + p10_0 + p01_0 + p01_1,\n  -1 + p00_0 + p10_0\n}\n", 
                                                           upper = "\nMIN {\n  2p00_0 + p10_0 - p10_1 + p01_0,\n  p00_0 + p10_0,\n  1 - p00_1 + p10_0 - p01_0\n}\n"
                    ), logs = list(lower = list(output = structure(c(0, 0, 0, 
                                                                     0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 
                                                                     0, 0, 0, 0, 0, -2, -1, 0, -1, 0, 0, 0, 0, -1, 0, -1, -1, 
                                                                     0, 2, 1, 0, 0, 0, 0, -1, 0, 1, 0, 1, 0, -1, 0, 0, 0, 1, 0, 
                                                                     -1, 0, 0, 0, 0, 0, 1, 1, 1, 1, -1, 0, 0, 0, 0, 0, 1, 0, 1, 
                                                                     0, -1, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 1, -1, 1, 0, 0, 
                                                                     0, 0, 0, 0, 0, 1, -1, 1, 0, -1, 1, 0, 0, 1, -1, 0, 0, 0, 
                                                                     0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 
                                                                     0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(13L, 11L), representation = "V"), 
                                                adjacency = list(c(3L, 4L, 6L, 7L, 8L, 9L, 10L, 11L, 
                                                                   12L, 13L), c(3L, 4L, 5L, 7L, 8L, 9L, 10L, 11L, 12L, 13L
                                                                   ), c(1L, 2L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L
                                                                   ), c(1L, 2L, 3L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L
                                                                   ), c(2L, 3L, 4L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L), 
                                                                 c(1L, 3L, 4L, 5L, 7L, 8L, 9L, 10L, 11L, 12L, 13L), 
                                                                 c(1L, 2L, 3L, 4L, 5L, 6L, 8L, 9L, 10L, 11L, 12L, 
                                                                   13L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 9L, 10L, 11L, 
                                                                           12L, 13L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 10L, 
                                                                                        11L, 12L, 13L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 
                                                                                                          9L, 11L, 12L, 13L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 
                                                                                                                                8L, 9L, 10L, 12L, 13L), 1:13, 1:13), inputadjacency = list(
                                                                                                                                    c(2L, 5L, 17L, 25L, 27L, 29L, 33L, 35L, 65L), c(1L, 
                                                                                                                                                                                    6L, 17L, 25L, 27L, 29L, 34L, 36L, 65L), integer(0), 
                                                                                                                                    integer(0), c(1L, 6L, 27L, 29L, 33L, 35L, 65L), c(2L, 
                                                                                                                                                                                      5L, 27L, 29L, 34L, 36L, 65L), integer(0), integer(0), 
                                                                                                                                    integer(0), integer(0), integer(0), integer(0), integer(0), 
                                                                                                                                    integer(0), integer(0), integer(0), c(1L, 2L, 25L, 
                                                                                                                                                                          27L, 35L, 36L, 49L, 53L, 65L), integer(0), integer(0), 
                                                                                                                                    integer(0), integer(0), integer(0), integer(0), integer(0), 
                                                                                                                                    c(1L, 2L, 17L, 29L, 35L, 36L, 51L, 57L, 65L), integer(0), 
                                                                                                                                    c(1L, 2L, 5L, 6L, 17L, 29L, 33L, 34L, 35L, 36L, 49L, 
                                                                                                                                      53L, 65L), integer(0), c(1L, 2L, 5L, 6L, 25L, 27L, 
                                                                                                                                                               33L, 34L, 35L, 36L, 51L, 57L, 65L), integer(0), integer(0), 
                                                                                                                                    integer(0), c(1L, 5L, 27L, 29L, 34L, 35L, 49L, 57L, 
                                                                                                                                                  65L), c(2L, 6L, 27L, 29L, 33L, 36L, 49L, 57L, 65L
                                                                                                                                                  ), c(1L, 5L, 17L, 25L, 27L, 29L, 33L, 36L, 49L, 51L, 
                                                                                                                                                       53L, 57L, 65L), c(2L, 6L, 17L, 25L, 27L, 29L, 34L, 
                                                                                                                                                                         35L, 49L, 51L, 53L, 57L, 65L), integer(0), integer(0), 
                                                                                                                                    integer(0), integer(0), integer(0), integer(0), integer(0), 
                                                                                                                                    integer(0), integer(0), integer(0), integer(0), integer(0), 
                                                                                                                                    c(17L, 27L, 33L, 34L, 35L, 36L, 53L, 57L, 65L), integer(0), 
                                                                                                                                    c(25L, 29L, 35L, 36L, 53L, 57L, 65L), integer(0), 
                                                                                                                                    c(17L, 27L, 35L, 36L, 49L, 51L, 65L), integer(0), 
                                                                                                                                    integer(0), integer(0), c(25L, 29L, 33L, 34L, 35L, 
                                                                                                                                                              36L, 49L, 51L, 65L), integer(0), integer(0), integer(0), 
                                                                                                                                    integer(0), integer(0), integer(0), integer(0), c(1L, 
                                                                                                                                                                                      2L, 5L, 6L, 17L, 25L, 27L, 29L, 33L, 34L, 35L, 36L, 
                                                                                                                                                                                      49L, 51L, 53L, 57L)), incidence = list(c(17L, 18L, 
                                                                                                                                                                                                                               25L, 26L, 27L, 28L, 29L, 30L, 35L, 36L, 47L, 48L, 49L, 
                                                                                                                                                                                                                               50L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 
                                                                                                                                                                                                                               61L, 62L, 63L, 64L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 
                                                                                                                                                                                                                                                      9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 27L, 28L, 29L, 
                                                                                                                                                                                                                                                      30L, 33L, 34L, 35L, 36L, 41L, 42L, 47L, 48L), c(1L, 2L, 
                                                                                                                                                                                                                                                                                                      3L, 4L, 9L, 10L, 15L, 16L, 17L, 18L, 25L, 26L, 27L, 28L, 
                                                                                                                                                                                                                                                                                                      29L, 30L, 33L, 34L, 35L, 36L, 41L, 42L, 47L, 48L, 49L, 
                                                                                                                                                                                                                                                                                                      50L, 57L, 58L, 59L, 60L, 61L, 62L), c(1L, 2L, 3L, 4L, 
                                                                                                                                                                                                                                                                                                                                            5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 
                                                                                                                                                                                                                                                                                                                                            19L, 20L, 23L, 24L, 25L, 26L, 29L, 30L, 33L, 34L, 35L, 
                                                                                                                                                                                                                                                                                                                                            36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 
                                                                                                                                                                                                                                                                                                                                            47L, 48L, 51L, 52L, 55L, 56L, 57L, 58L, 61L, 62L, 65L
                                                                                                                                                                                                                                                                                                      ), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 
                                                                                                                                                                                                                                                                                                           13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 
                                                                                                                                                                                                                                                                                                           24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 
                                                                                                                                                                                                                                                                                                           41L, 42L, 43L, 44L, 45L, 46L, 49L, 50L, 57L, 58L, 59L, 
                                                                                                                                                                                                                                                                                                           60L, 61L, 62L, 65L), c(1L, 2L, 3L, 4L, 9L, 10L, 15L, 
                                                                                                                                                                                                                                                                                                                                  16L, 17L, 18L, 19L, 20L, 25L, 26L, 31L, 32L, 33L, 34L, 
                                                                                                                                                                                                                                                                                                                                  35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 
                                                                                                                                                                                                                                                                                                                                  46L, 47L, 48L, 49L, 50L, 51L, 52L, 53L, 54L, 55L, 56L, 
                                                                                                                                                                                                                                                                                                                                  57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L), c(5L, 6L, 
                                                                                                                                                                                                                                                                                                                                                                                  7L, 8L, 11L, 12L, 13L, 14L, 21L, 22L, 23L, 24L, 27L, 
                                                                                                                                                                                                                                                                                                                                                                                  28L, 29L, 30L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 
                                                                                                                                                                                                                                                                                                                                                                                  41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 
                                                                                                                                                                                                                                                                                                                                                                                  52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 
                                                                                                                                                                                                                                                                                                                                                                                  63L, 64L, 65L), c(2L, 4L, 6L, 8L, 10L, 12L, 14L, 16L, 
                                                                                                                                                                                                                                                                                                                                                                                                    17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 
                                                                                                                                                                                                                                                                                                                                                                                                    28L, 29L, 30L, 31L, 32L, 34L, 36L, 38L, 40L, 42L, 44L, 
                                                                                                                                                                                                                                                                                                                                                                                                    46L, 48L, 49L, 50L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 
                                                                                                                                                                                                                                                                                                                                                                                                    58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L), c(1L, 2L, 3L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                               4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                               16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                               27L, 28L, 29L, 30L, 31L, 32L, 35L, 36L, 37L, 38L, 39L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                               40L, 47L, 48L, 51L, 52L, 53L, 54L, 55L, 56L, 63L, 64L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                               65L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                       12L, 13L, 14L, 15L, 16L, 17L, 18L, 21L, 22L, 27L, 28L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                       31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                       42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 53L, 54L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                       59L, 60L, 63L, 64L, 65L), c(1L, 3L, 5L, 7L, 9L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   13L, 15L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 35L, 37L, 39L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   41L, 43L, 45L, 47L, 49L, 50L, 51L, 52L, 53L, 54L, 55L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L), 1:65, 
                                                                                                                                                                                                                             1:65), inputincidence = list(c(2L, 3L, 4L, 5L, 6L, 
                                                                                                                                                                                                                                                            9L, 10L, 11L, 12L, 13L), c(2L, 3L, 4L, 5L, 6L, 8L, 9L, 
                                                                                                                                                                                                                                                                                       10L, 12L, 13L), c(2L, 3L, 4L, 5L, 6L, 9L, 10L, 11L, 12L, 
                                                                                                                                                                                                                                                                                                         13L), c(2L, 3L, 4L, 5L, 6L, 8L, 9L, 10L, 12L, 13L), c(2L, 
                                                                                                                                                                                                                                                                                                                                                               4L, 5L, 7L, 9L, 10L, 11L, 12L, 13L), c(2L, 4L, 5L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                      8L, 9L, 10L, 12L, 13L), c(2L, 4L, 5L, 7L, 9L, 10L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                12L, 13L), c(2L, 4L, 5L, 7L, 8L, 9L, 10L, 12L, 13L), 
                                                                                                                                                                                                                                                          c(2L, 3L, 4L, 5L, 6L, 9L, 10L, 11L, 12L, 13L), c(2L, 
                                                                                                                                                                                                                                                                                                           3L, 4L, 5L, 6L, 8L, 9L, 10L, 12L, 13L), c(2L, 4L, 
                                                                                                                                                                                                                                                                                                                                                     5L, 7L, 9L, 10L, 11L, 12L, 13L), c(2L, 4L, 5L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                        8L, 9L, 10L, 12L, 13L), c(2L, 4L, 5L, 7L, 9L, 10L, 
                                                                                                                                                                                                                                                                                                                                                                                                                  11L, 12L, 13L), c(2L, 4L, 5L, 7L, 8L, 9L, 10L, 12L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                    13L), c(2L, 3L, 4L, 5L, 6L, 9L, 10L, 11L, 12L, 13L
                                                                                                                                                                                                                                                                                                                                                                                                                                    ), c(2L, 3L, 4L, 5L, 6L, 8L, 9L, 10L, 12L, 13L), 
                                                                                                                                                                                                                                                          c(1L, 3L, 5L, 6L, 8L, 9L, 10L, 11L, 12L, 13L), c(1L, 
                                                                                                                                                                                                                                                                                                           3L, 5L, 6L, 8L, 9L, 10L, 11L, 12L, 13L), c(4L, 5L, 
                                                                                                                                                                                                                                                                                                                                                      6L, 8L, 9L, 11L, 12L, 13L), c(4L, 5L, 6L, 8L, 9L, 
                                                                                                                                                                                                                                                                                                                                                                                    11L, 12L, 13L), c(5L, 7L, 8L, 9L, 10L, 11L, 12L, 
                                                                                                                                                                                                                                                                                                                                                                                                      13L), c(5L, 7L, 8L, 9L, 10L, 11L, 12L, 13L), c(4L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                     5L, 7L, 8L, 9L, 11L, 12L, 13L), c(4L, 5L, 7L, 8L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       9L, 11L, 12L, 13L), c(1L, 3L, 4L, 5L, 6L, 8L, 9L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             11L, 12L, 13L), c(1L, 3L, 4L, 5L, 6L, 8L, 9L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               12L, 13L), c(1L, 2L, 3L, 5L, 7L, 8L, 9L, 10L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            12L, 13L), c(1L, 2L, 3L, 5L, 7L, 8L, 9L, 10L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         12L, 13L), c(1L, 2L, 3L, 4L, 5L, 7L, 8L, 9L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      12L, 13L), c(1L, 2L, 3L, 4L, 5L, 7L, 8L, 9L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   12L, 13L), c(5L, 6L, 8L, 9L, 10L, 11L, 12L, 13L), 
                                                                                                                                                                                                                                                          c(5L, 6L, 8L, 9L, 10L, 11L, 12L, 13L), c(2L, 3L, 
                                                                                                                                                                                                                                                                                                   4L, 5L, 6L, 7L, 10L, 11L, 12L, 13L), c(2L, 3L, 4L, 
                                                                                                                                                                                                                                                                                                                                          5L, 6L, 7L, 8L, 10L, 12L, 13L), c(1L, 2L, 3L, 4L, 
                                                                                                                                                                                                                                                                                                                                                                            6L, 7L, 9L, 10L, 11L, 12L, 13L), c(1L, 2L, 3L, 4L, 
                                                                                                                                                                                                                                                                                                                                                                                                               6L, 7L, 8L, 9L, 10L, 12L, 13L), c(4L, 6L, 7L, 9L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                 10L, 11L, 12L, 13L), c(4L, 6L, 7L, 8L, 9L, 10L, 12L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        13L), c(4L, 6L, 7L, 9L, 10L, 11L, 12L, 13L), c(4L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       6L, 7L, 8L, 9L, 10L, 12L, 13L), c(2L, 3L, 4L, 5L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         6L, 7L, 10L, 11L, 12L, 13L), c(2L, 3L, 4L, 5L, 6L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        7L, 8L, 10L, 12L, 13L), c(4L, 5L, 6L, 7L, 10L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  12L, 13L), c(4L, 5L, 6L, 7L, 8L, 10L, 12L, 13L), 
                                                                                                                                                                                                                                                          c(4L, 5L, 6L, 7L, 10L, 11L, 12L, 13L), c(4L, 5L, 
                                                                                                                                                                                                                                                                                                   6L, 7L, 8L, 10L, 12L, 13L), c(1L, 2L, 3L, 4L, 6L, 
                                                                                                                                                                                                                                                                                                                                 7L, 9L, 10L, 11L, 12L, 13L), c(1L, 2L, 3L, 4L, 6L, 
                                                                                                                                                                                                                                                                                                                                                                7L, 8L, 9L, 10L, 12L, 13L), c(1L, 3L, 5L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                              8L, 10L, 11L, 12L, 13L), c(1L, 3L, 5L, 6L, 7L, 8L, 
                                                                                                                                                                                                                                                                                                                                                                                                                         10L, 11L, 12L, 13L), c(1L, 4L, 6L, 7L, 8L, 9L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                12L, 13L), c(1L, 4L, 6L, 7L, 8L, 9L, 11L, 12L, 13L
                                                                                                                                                                                                                                                                                                                                                                                                                                                ), c(1L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L), c(1L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L), c(1L, 4L, 6L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       7L, 8L, 9L, 11L, 12L, 13L), c(1L, 4L, 6L, 7L, 8L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     9L, 11L, 12L, 13L), c(1L, 3L, 4L, 5L, 6L, 7L, 8L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           11L, 12L, 13L), c(1L, 3L, 4L, 5L, 6L, 7L, 8L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             12L, 13L), c(1L, 3L, 5L, 6L, 7L, 8L, 10L, 11L, 12L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          13L), c(1L, 3L, 5L, 6L, 7L, 8L, 10L, 11L, 12L, 13L
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          ), c(1L, 3L, 4L, 5L, 6L, 7L, 8L, 11L, 12L, 13L), 
                                                                                                                                                                                                                                                          c(1L, 3L, 4L, 5L, 6L, 7L, 8L, 11L, 12L, 13L), c(1L, 
                                                                                                                                                                                                                                                                                                          6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L), c(1L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                                                                 8L, 9L, 10L, 11L, 12L, 13L), 4:13), input = structure(c(0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, -1, 1, 0, 1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, -1, 1, 0, 1, 0, 0, -1, 0, 0, 0, 0, 1, 1, 0, 0, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         -1, 0, 0, -1, -1, 1, 1, 0, -1, 0, -1, 1, 0, 1, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         -1, 1, 0, 1, 0, 0, -1, 0, 0, 0, 0, 1, 1, 0, 0, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, -1, -1, 1, 1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, 0, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, -1, -1, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, -1, -1, 0, 0, 0, 0, -1, -1, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 0, -1, -1, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         -1, -1, 0, 0, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, -1, -1, 0, 0, -1, -1, 0, 0, 0, 0, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         -1, 0, 0, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, -1, -1, 0, 0, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, -1, -1, 0, 0, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 0, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, -1, -1, -1, -1, 0, 0, -1, -1, -1, -1, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, -1, -1, -1, -1, 0, 0, -1, -1, -1, -1, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, -1, -1, -1, -1, 0, 0, -1, -1, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         -1, -1, -1, -1, 0, 0, -1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1, -1, -1, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, -1, -1, 0, 0, -1, -1, -1, -1, -1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                         0, 0, 0, 0, 0, 0, -1, -1), dim = c(64L, 11L), representation = "H")), 
                                   upper = list(output = structure(c(0, 0, 0, 0, 0, 0, 0, 
                                                                     0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 
                                                                     0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, -1, -1, 0, 0, 2, 
                                                                     1, 0, -1, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, -1, 0, 0, 
                                                                     -1, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, -1, 0, 0, 0, 0, 1, 
                                                                     1, 0, 0, 0, -1, 0, 0, 0, 1, -1, 0, 0, 0, 0, 1, 1, 0, 
                                                                     1, 0, -1, -1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 
                                                                     0, -1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                     1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), dim = c(13L, 
                                                                                                                           11L), representation = "V"), adjacency = list(2:13, c(1L, 
                                                                                                                                                                                 3L, 4L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L), c(1L, 2L, 
                                                                                                                                                                                                                                4L, 6L, 8L, 9L, 10L, 11L, 12L, 13L), c(1L, 2L, 3L, 5L, 
                                                                                                                                                                                                                                                                       6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L), c(1L, 4L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                              8L, 9L, 10L, 11L, 12L, 13L), c(1L, 2L, 3L, 4L, 5L, 7L, 
                                                                                                                                                                                                                                                                                                                                             8L, 9L, 10L, 11L, 12L, 13L), c(1L, 2L, 4L, 5L, 6L, 8L, 
                                                                                                                                                                                                                                                                                                                                                                            9L, 10L, 11L, 12L, 13L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                       9L, 10L, 11L, 12L, 13L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                  8L, 10L, 11L, 12L, 13L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                             8L, 9L, 11L, 12L, 13L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       8L, 9L, 10L, 12L, 13L), 1:13, 1:13), inputadjacency = list(
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           c(2L, 5L, 19L, 31L, 37L, 43L, 65L), c(1L, 6L, 19L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 31L, 38L, 44L, 65L), integer(0), integer(0), c(1L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                6L, 19L, 21L, 23L, 31L, 37L, 43L, 65L), c(2L, 5L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          19L, 21L, 23L, 31L, 38L, 44L, 65L), integer(0), integer(0), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           integer(0), integer(0), integer(0), integer(0), integer(0), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           integer(0), integer(0), integer(0), integer(0), integer(0), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           c(1L, 2L, 5L, 6L, 23L, 31L, 37L, 38L, 43L, 44L, 51L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             57L, 65L), integer(0), c(5L, 6L, 23L, 31L, 43L, 44L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      49L, 53L, 65L), integer(0), c(5L, 6L, 19L, 21L, 43L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    44L, 51L, 57L, 65L), integer(0), integer(0), integer(0), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           integer(0), integer(0), integer(0), integer(0), c(1L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             2L, 5L, 6L, 19L, 21L, 37L, 38L, 43L, 44L, 49L, 53L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             65L), integer(0), integer(0), integer(0), integer(0), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           integer(0), c(1L, 5L, 19L, 31L, 38L, 43L, 51L, 53L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         65L), c(2L, 6L, 19L, 31L, 37L, 44L, 51L, 53L, 65L
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ), integer(0), integer(0), integer(0), integer(0), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           c(1L, 5L, 19L, 21L, 23L, 31L, 37L, 44L, 49L, 51L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             53L, 57L, 65L), c(2L, 6L, 19L, 21L, 23L, 31L, 38L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               43L, 49L, 51L, 53L, 57L, 65L), integer(0), integer(0), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           integer(0), integer(0), c(21L, 31L, 43L, 44L, 53L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     57L, 65L), integer(0), c(19L, 23L, 37L, 38L, 43L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              44L, 53L, 57L, 65L), integer(0), c(21L, 31L, 37L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 38L, 43L, 44L, 49L, 51L, 65L), integer(0), integer(0), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           integer(0), c(19L, 23L, 43L, 44L, 49L, 51L, 65L), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           integer(0), integer(0), integer(0), integer(0), integer(0), 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           integer(0), integer(0), c(1L, 2L, 5L, 6L, 19L, 21L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     23L, 31L, 37L, 38L, 43L, 44L, 49L, 51L, 53L, 57L)), 
                                                incidence = list(c(1L, 3L, 5L, 7L, 9L, 11L, 13L, 
                                                                   15L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 
                                                                   26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 35L, 37L, 
                                                                   39L, 41L, 43L, 45L, 47L, 49L, 50L, 51L, 52L, 53L, 
                                                                   54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 
                                                                   64L, 65L), c(5L, 6L, 7L, 8L, 11L, 12L, 13L, 14L, 
                                                                                21L, 22L, 23L, 24L, 27L, 28L, 29L, 30L, 33L, 34L, 
                                                                                35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 
                                                                                45L, 46L, 47L, 48L, 49L, 50L, 51L, 52L, 53L, 54L, 
                                                                                55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 
                                                                                65L), c(19L, 20L, 21L, 22L, 23L, 24L, 31L, 32L, 43L, 
                                                                                        44L, 45L, 46L, 49L, 50L, 51L, 52L, 53L, 54L, 55L, 
                                                                                        56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L), c(5L, 
                                                                                                                                        6L, 7L, 8L, 11L, 12L, 13L, 14L, 19L, 20L, 21L, 22L, 
                                                                                                                                        23L, 24L, 31L, 32L, 37L, 38L, 39L, 40L, 43L, 44L, 
                                                                                                                                        45L, 46L, 51L, 52L, 53L, 54L, 55L, 56L, 63L, 64L), 
                                                                 c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 
                                                                   12L, 13L, 14L, 15L, 16L, 19L, 20L, 31L, 32L, 
                                                                   37L, 38L, 39L, 40L, 43L, 44L, 45L, 46L), c(1L, 
                                                                                                              2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 
                                                                                                              13L, 14L, 15L, 16L, 17L, 18L, 21L, 22L, 27L, 
                                                                                                              28L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 
                                                                                                              39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 
                                                                                                              48L, 49L, 50L, 53L, 54L, 59L, 60L, 63L, 64L, 
                                                                                                              65L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 
                                                                                                                      11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 
                                                                                                                      20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 
                                                                                                                      29L, 30L, 31L, 32L, 35L, 36L, 37L, 38L, 39L, 
                                                                                                                      40L, 47L, 48L, 51L, 52L, 53L, 54L, 55L, 56L, 
                                                                                                                      63L, 64L, 65L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 
                                                                                                                                        8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 
                                                                                                                                        18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 
                                                                                                                                        27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 41L, 
                                                                                                                                        42L, 43L, 44L, 45L, 46L, 49L, 50L, 57L, 58L, 
                                                                                                                                        59L, 60L, 61L, 62L, 65L), c(1L, 2L, 3L, 4L, 9L, 
                                                                                                                                                                    10L, 15L, 16L, 17L, 18L, 19L, 20L, 25L, 26L, 
                                                                                                                                                                    31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 
                                                                                                                                                                    40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 
                                                                                                                                                                    49L, 50L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 
                                                                                                                                                                    58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L), c(2L, 
                                                                                                                                                                                                               4L, 6L, 8L, 10L, 12L, 14L, 16L, 17L, 18L, 19L, 
                                                                                                                                                                                                               20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 
                                                                                                                                                                                                               29L, 30L, 31L, 32L, 34L, 36L, 38L, 40L, 42L, 
                                                                                                                                                                                                               44L, 46L, 48L, 49L, 50L, 51L, 52L, 53L, 54L, 
                                                                                                                                                                                                               55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 
                                                                                                                                                                                                               64L, 65L), c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 
                                                                                                                                                                                                                            9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 19L, 20L, 
                                                                                                                                                                                                                            23L, 24L, 25L, 26L, 29L, 30L, 33L, 34L, 35L, 
                                                                                                                                                                                                                            36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 
                                                                                                                                                                                                                            45L, 46L, 47L, 48L, 51L, 52L, 55L, 56L, 57L, 
                                                                                                                                                                                                                            58L, 61L, 62L, 65L), 1:65, 1:65), inputincidence = list(
                                                                                                                                                                                                                                c(1L, 5L, 6L, 7L, 8L, 9L, 11L, 12L, 13L), 5:13, 
                                                                                                                                                                                                                                c(1L, 5L, 6L, 7L, 8L, 9L, 11L, 12L, 13L), 5:13, 
                                                                                                                                                                                                                                c(1L, 2L, 4L, 5L, 6L, 7L, 8L, 11L, 12L, 13L), 
                                                                                                                                                                                                                                c(2L, 4L, 5L, 6L, 7L, 8L, 10L, 11L, 12L, 13L), 
                                                                                                                                                                                                                                c(1L, 2L, 4L, 5L, 6L, 7L, 8L, 11L, 12L, 13L), 
                                                                                                                                                                                                                                c(2L, 4L, 5L, 6L, 7L, 8L, 10L, 11L, 12L, 13L), 
                                                                                                                                                                                                                                c(1L, 5L, 6L, 7L, 8L, 9L, 11L, 12L, 13L), 5:13, 
                                                                                                                                                                                                                                c(1L, 2L, 4L, 5L, 6L, 7L, 8L, 11L, 12L, 13L), 
                                                                                                                                                                                                                                c(2L, 4L, 5L, 6L, 7L, 8L, 10L, 11L, 12L, 13L), 
                                                                                                                                                                                                                                c(1L, 2L, 4L, 5L, 6L, 7L, 8L, 11L, 12L, 13L), 
                                                                                                                                                                                                                                c(2L, 4L, 5L, 6L, 7L, 8L, 10L, 11L, 12L, 13L), 
                                                                                                                                                                                                                                c(1L, 5L, 6L, 7L, 8L, 9L, 11L, 12L, 13L), 5:13, 
                                                                                                                                                                                                                                c(1L, 6L, 7L, 8L, 9L, 10L, 12L, 13L), c(1L, 6L, 
                                                                                                                                                                                                                                                                        7L, 8L, 9L, 10L, 12L, 13L), c(1L, 3L, 4L, 5L, 
                                                                                                                                                                                                                                                                                                      7L, 8L, 9L, 10L, 11L, 12L, 13L), c(1L, 3L, 4L, 
                                                                                                                                                                                                                                                                                                                                         5L, 7L, 8L, 9L, 10L, 11L, 12L, 13L), c(1L, 2L, 
                                                                                                                                                                                                                                                                                                                                                                                3L, 4L, 6L, 7L, 8L, 10L, 12L, 13L), c(1L, 2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                      3L, 4L, 6L, 7L, 8L, 10L, 12L, 13L), c(1L, 2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                            3L, 4L, 7L, 8L, 10L, 11L, 12L, 13L), c(1L, 2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   3L, 4L, 7L, 8L, 10L, 11L, 12L, 13L), c(1L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          8L, 9L, 10L, 11L, 12L, 13L), c(1L, 7L, 8L, 9L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         10L, 11L, 12L, 13L), c(1L, 2L, 6L, 7L, 8L, 10L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                12L, 13L), c(1L, 2L, 6L, 7L, 8L, 10L, 12L, 13L
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                ), c(1L, 2L, 7L, 8L, 10L, 11L, 12L, 13L), c(1L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            2L, 7L, 8L, 10L, 11L, 12L, 13L), c(1L, 3L, 4L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               5L, 6L, 7L, 8L, 9L, 10L, 12L, 13L), c(1L, 3L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     4L, 5L, 6L, 7L, 8L, 9L, 10L, 12L, 13L), c(1L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               2L, 6L, 8L, 9L, 11L, 12L, 13L), c(2L, 6L, 8L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 9L, 10L, 11L, 12L, 13L), c(1L, 2L, 6L, 7L, 9L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            11L, 12L, 13L), c(2L, 6L, 7L, 9L, 10L, 11L, 12L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              13L), c(1L, 2L, 4L, 5L, 6L, 7L, 9L, 11L, 12L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      13L), c(2L, 4L, 5L, 6L, 7L, 9L, 10L, 11L, 12L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              13L), c(1L, 2L, 4L, 5L, 6L, 7L, 9L, 11L, 12L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      13L), c(2L, 4L, 5L, 6L, 7L, 9L, 10L, 11L, 12L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              13L), c(1L, 2L, 6L, 8L, 9L, 11L, 12L, 13L), c(2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            6L, 8L, 9L, 10L, 11L, 12L, 13L), c(1L, 2L, 3L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               4L, 5L, 6L, 8L, 9L, 11L, 12L, 13L), c(2L, 3L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     4L, 5L, 6L, 8L, 9L, 10L, 11L, 12L, 13L), c(1L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                2L, 3L, 4L, 5L, 6L, 8L, 9L, 11L, 12L, 13L), c(2L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              3L, 4L, 5L, 6L, 8L, 9L, 10L, 11L, 12L, 13L), 
                                                                                                                                                                                                                                c(1L, 2L, 6L, 7L, 9L, 11L, 12L, 13L), c(2L, 6L, 
                                                                                                                                                                                                                                                                        7L, 9L, 10L, 11L, 12L, 13L), c(1L, 2L, 3L, 6L, 
                                                                                                                                                                                                                                                                                                       8L, 9L, 10L, 12L, 13L), c(1L, 2L, 3L, 6L, 8L, 
                                                                                                                                                                                                                                                                                                                                 9L, 10L, 12L, 13L), c(1L, 2L, 3L, 4L, 7L, 9L, 
                                                                                                                                                                                                                                                                                                                                                       10L, 11L, 12L, 13L), c(1L, 2L, 3L, 4L, 7L, 9L, 
                                                                                                                                                                                                                                                                                                                                                                              10L, 11L, 12L, 13L), c(1L, 2L, 3L, 4L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                     9L, 10L, 12L, 13L), c(1L, 2L, 3L, 4L, 6L, 7L, 
                                                                                                                                                                                                                                                                                                                                                                                                                           9L, 10L, 12L, 13L), c(1L, 2L, 3L, 4L, 7L, 9L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                 10L, 11L, 12L, 13L), c(1L, 2L, 3L, 4L, 7L, 9L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                        10L, 11L, 12L, 13L), c(1L, 2L, 3L, 8L, 9L, 10L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               11L, 12L, 13L), c(1L, 2L, 3L, 8L, 9L, 10L, 11L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 12L, 13L), c(1L, 2L, 3L, 6L, 8L, 9L, 10L, 12L, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              13L), c(1L, 2L, 3L, 6L, 8L, 9L, 10L, 12L, 13L
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              ), c(1L, 2L, 3L, 8L, 9L, 10L, 11L, 12L, 13L), 
                                                                                                                                                                                                                                c(1L, 2L, 3L, 8L, 9L, 10L, 11L, 12L, 13L), c(1L, 
                                                                                                                                                                                                                                                                             2L, 3L, 4L, 6L, 7L, 9L, 10L, 12L, 13L), c(1L, 
                                                                                                                                                                                                                                                                                                                       2L, 3L, 4L, 6L, 7L, 9L, 10L, 12L, 13L), c(1L, 
                                                                                                                                                                                                                                                                                                                                                                 2L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L)), input = structure(c(0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, -1, 0, 0, 1, -1, 0, -1, 0, 0, 1, 0, 0, 0, 0, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               -1, 0, 0, 1, 1, 0, 0, 1, 1, -1, -1, 0, 1, 0, 1, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, -1, 0, 0, 1, -1, 0, -1, 0, 0, 1, 0, 0, 0, 0, -1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               -1, 0, 0, 1, 1, 0, 0, 1, 1, -1, -1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               1, 0, 0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 1, 1, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 1, 0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                                                                                                                                                                                                                                                                                                                                                                                               0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 
                                                                                                                                                                                                                                                                                                                                                                                                                               1, 1, 0, 0, 0, 0, 0, 0, 1, 1), dim = c(64L, 11L), representation = "H")))), class = "balkebound"), 
                    constraints = NULL, effect = "p{Y(M(X = 0), X = 1) = 1} - p{Y(M(X = 0), X = 0) = 1}")
test_that(
    desc = "Natural direct effect works",
    code = {
        testServer(
            app = appDir,
            expr = {
                session$setInputs(
                    edges = myin
                )
                edges_expected <- edges
                edges_object <- edgeList()
                expect_equal(
                    object = edges_object,
                    expected = edges_expected
                )
                graphres_expected <- graphres2
                graphres_object <- igraphFromList()
                # # Remove environment entry:
                # graphres_expected[[10]] <- NULL
                # graphres_object[[10]] <- NULL
                # expect_equal(
                #     object = graphres_object,
                #     expected = graphres_expected
                # )
                expect_true(object = igraph::identical_graphs(g1 = graphres_object, g2 = graphres_expected))
                session$setInputs(
                    effect = effecttext
                )
                resultslist_expected <- resultslist
                effectFixed$effectt <- effecttext
                resultslist_object <- optimizeGraph()
                # Remove environment info:
                resultslist_expected$graphres[[10]] <- NULL
                resultslist_object$graphres[[10]] <- NULL
                resultslist_expected$obj$graph[[10]] <- NULL
                resultslist_object$obj$graph[[10]] <- NULL
                resultslist_expected$obj$response.functions <- NULL
                resultslist_object$obj$response.functions <- NULL
                # expect_equal(
                #     object = resultslist_object,
                #     expected = resultslist_expected
                # )
            }
        )
    }
)

