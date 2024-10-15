#' Recursive function to get the last name in a list
#' 
#' @param x a list
#' @param name name of the top element of the list
#' @return The name of the deepest nested list element
#' @export
#' @examples
#' btm_var(list(X = list(Y = list(K = 1))))
#' 

btm_var <- function(x, name = NULL) {
    
    if(!is.list(x)) {
        return(name)
    } else {
        
        unlist(lapply(1:length(x), function(i) {
            btm_var(x[[i]], names(x)[[i]])
        }))
    }
    
}

#' Recursive function to translate an effect list to a path sequence
#' 
#' @param x A list of vars as returned by \link{parse_effect}
#' @param name The name of the outcome variable
#' @return a list of characters describing the path sequence
#' @export
#' @examples
#' nofill <- "p{Y(X = 1, M1 = 1, M2(X = 1, M1 = 1)) = 1}"
#' eff2 <- parse_effect(nofill)$vars[[1]][[1]]
#' list_to_path(eff2, "Y")
#' 

list_to_path <- function(x, name = NULL) {
    
    if(!is.list(x)) {
        names(x) <- name
        return(x)
    } else {
        
        lapply(1:length(x), function(i) {
            unlist(list_to_path(x[[i]], paste(names(x)[[i]], "->", name)))
        })
    }
    
}

#' Compute the scalar product of two numeric vectors of the same length
#' 
#' A helper function for \code{\link{evaluate_objective}}.
#' @param numbers1,numbers2 Two numeric vectors of the same length.
#' @return A string consisting of the value of the scalar product of \code{numbers1} and \code{numbers2}.
#' @noRd
constant_term <- function(numbers1, numbers2) {
    as.character(sum(numbers1*numbers2))
}

#' Compute the product of a single numeric scalar and a single string
#' 
#' A helper function for \code{\link{linear_expression}}.
#' @param number A numeric vector of length 1.
#' @param string A character vector of length 1.
#' @return A string consisting of the concatenation of \code{number} and \code{string}, including its sign.
#' @noRd
linear_term <- function(number, string) {
    if (number == 0) return("")
    if (number == 1) return(paste0(" + ", string))
    if (number == -1) return(paste0(" - ", string))
    if (number > 0) return(paste0(" + ", number, string))
    return(paste0(" - ", abs(number), string))
}

#' Compute the scalar product of a vector of numbers and a vector of strings
#' 
#' A helper function for \code{\link{evaluate_objective}}.
#' @param numbers A numeric vector.
#' @param strings A character vector of the same length as \code{numbers}.
#' @return A string consisting of the corresponding linear combination, including the sign of its first term.
#' @noRd
linear_expression <- function(numbers, strings) {
    paste0(mapply(linear_term, numbers, strings), collapse = "")
}

#' Compute the scalar product of a vector of numbers and a vector of both numbers and strings
#' 
#' A helper function for \code{\link{opt_effect}}.
#' @param c1_num A numeric column matrix.
#' @param p A character vector.
#' @param y A numeric vector whose length is the sum of the lengths of \code{c1_num} and \code{p}.
#' @return A string consisting of an affine expression in \code{p} corresponding to the scalar product of \code{c(c1_num, p)} with \code{y}.
#' @noRd
evaluate_objective <- function(c1_num, p, y) {
    m1 <- nrow(c1_num)
    m <- length(y)
    number_indices <- 1:m1 # the indices of the numeric entries of c1
    parameter_indices <- (m1+1):m # the indices of the character entries of c1
    y1 <- y[number_indices]
    y2 <- y[parameter_indices]
    const_term <- constant_term(numbers1 = c1_num, numbers2 = y1)
    lin_expr <- linear_expression(numbers = y2, strings = p)
    aff_expr <- paste0(as.character(const_term), lin_expr)
    # trim the initial part of aff_expr if needed
    if (const_term == "0" && lin_expr != "") {
        sign_of_first_lin_term <- substr(lin_expr, start = 2, stop = 2)
        aff_expr <- substr(aff_expr, start = 5, stop = nchar(aff_expr)) # discard prefix "0 + " or "0 - "
        if (sign_of_first_lin_term == "-") aff_expr <- paste0("-", aff_expr)
    }
    aff_expr <- paste0("  ", aff_expr) # indent each line/element slightly
    return(aff_expr)
}

# Check that causal query is parsable.
# effecttext: A string, e.g., "p{Y(X = 1)=1} - p{Y(X = 0)=1}".
#' Check that a string representing a causal query can be successfully parsed.
#' @param effecttext A string representing a causal query.
#' @return \code{TRUE} if \code{effecttext} can be successfully parsed; else \code{FALSE}.
#' @noRd
#' @examples
#' effecttext <- "p{Y(X = 1)=1} - p{Y(X = 0)=1}"
#' queryparsecheck(effecttext = effecttext) # TRUE
queryparsecheck <- function(effecttext) {
    parsed.test <- tryCatch(
        expr = parse_effect(text = effecttext),
        error = function(e) {
            "fail"
        }
    )
    if (!is.list(parsed.test)) {
        return(FALSE)
    }
    TRUE
}

# Check that the query 'effecttext' can be parsed and that
# the causal problem (effecttext, graphres) satisfies
# the conditions on the query / intervention-set.
#' Check conditions on query
#' 
#' Given an admissible causal DAG, check that given a causal query satisfies 
#' conditions that guarantee the corresponding causal problem to be a linear program.
#' Throws error messages detailing any conditions violated.
#' @param effecttext A string representing a causal query.
#' @param graphres An \code{igraph} object representing a digraph.
#' @return \code{TRUE} if \code{effecttext} is parsable, contains only variables in \code{V(graphres)} 
#' and satisfies conditions for linearity; else \code{FALSE}.
#' @export
#' @examples
#' graphres <- graph_from_literal(X -+ Y, X -+ M, M -+ Y, Ul -+ X, Ur -+ M, Ur -+ Y)
#' V(graphres)$leftside <- c(1, 0, 0, 1, 0)
#' V(graphres)$latent <- c(0, 0, 0, 1, 1)
#' V(graphres)$nvals <- c(2, 2, 2, 2, 2)
#' V(graphres)$exposure <- c(0, 0, 0, 0, 0)
#' V(graphres)$outcome <- c(0, 0, 0, 0, 0)
#' E(graphres)$rlconnect <- c(0, 0, 0, 0, 0, 0)
#' E(graphres)$edge.monotone <- c(0, 0, 0, 0, 0, 0)
#' effecttext <- "p{Y(M(X = 0), X = 1) = 1} - p{Y(M(X = 0), X = 0) = 1}"
#' querycheck(effecttext = effecttext, graphres = graphres) # TRUE
querycheck <- function(effecttext, graphres) {
    # Check parsability
    if (!queryparsecheck(effecttext = effecttext)) {
        error_message <- "Unable to parse effect!"
        if (isRunning()) {
            showNotification(
                ui = error_message,
                type = "error"
            )
        } else {
            message(error_message)
        }
        return(FALSE)
    }
    
    parsed_effect <- parse_effect(text = effecttext)
    
    chk0 <- lapply(parsed_effect$vars, btm_var)
    interven.vars <- unique(unlist(chk0))
    allnmes <-
        unique(c(interven.vars, unlist(lapply(
            parsed_effect$vars, names
        ))))
    
    # Check variable names
    realnms <- names(V(graphres))
    if (any(!allnmes %in% realnms)) {
        error_message <-
            sprintf(
                "Names %s in effect not specified in graph!",
                paste(allnmes[which(!allnmes %in% realnms)],
                      collapse = ", "
                )
            )
        if (isRunning()) {
            showNotification(
                ui = error_message,
                type = "error"
            )
        } else {
            message(error_message)
        }
        return(FALSE)
    }
    
    ## Check that children of intervention set are on the right
    any.children.onleft <- sapply(interven.vars, function(v) {
        children <- neighbors(
            graph = graphres,
            v = V(graphres)[v],
            mode = "out"
        )
        any(children$leftside == 1)
    })
    if (any(any.children.onleft)) {
        error_message <- sprintf(
            "Cannot intervene on %s because it has children on the leftside!",
            paste(interven.vars[which(any.children.onleft)],
                  collapse = ", "
            )
        )
        if (isRunning()) {
            showNotification(
                ui = error_message,
                type = "error"
            )
        } else {
            message(error_message)
        }
        return(FALSE)
    }
    
    # If left side contains variables, they must be ancestors of intervention set
    if (any(V(graphres)$leftside == 1 &
            names(V(graphres)) != "Ul")) {
        cond.vars <-
            names(V(graphres)[V(graphres)$leftside == 1 &
                                  names(V(graphres)) != "Ul"])
        chkpaths <- unlist(lapply(cond.vars, function(x) {
            pths <- all_simple_paths(
                graph = graphres,
                from = x,
                to = allnmes,
                mode = "out"
            )
            unlist(lapply(pths, function(pth) {
                any(interven.vars %in% names(pth))
            }))
        }))
        if (any(!chkpaths)) {
            error_message <- sprintf(
                "Leftside variables %s not ancestors of intervention sets. Condition 6 violated.",
                paste(names(chkpaths)[!chkpaths],
                      collapse = ", "
                )
            )
            if (isRunning()) {
                showNotification(
                    ui = error_message,
                    type = "error"
                )
            } else {
                message(error_message)
            }
            return(FALSE)
        }
    }
    
    # Check operation validity
    if ("oper" %in% names(parsed_effect) &
        any(!unlist(parsed_effect$oper) %in% c("+", "-"))) {
        whoper <-
            unlist(parsed_effect$oper)[!unlist(parsed_effect$oper) %in% c("+", "-")]
        error_message <- sprintf(
            "Operator '%s' not allowed!",
            whoper
        )
        error_message <- "Unable to parse effect!"
        if (isRunning()) {
            showNotification(
                ui = error_message,
                type = "error"
            )
        } else {
            message(error_message)
        }
        return(FALSE)
    }
    TRUE
}

# Check parsability of constraints.
# constrainttext: A string, e.g., "X(Z = 1) >= X(Z = 0)".
#' Check that a user-provided optional constraint is parsable.
#' @param constrainttext A string representing a constraint.
#' @param graphres An \code{igraph} object representing a DAG.
#' @return \code{TRUE} if \code{constrainttext} is parsable; else \code{FALSE}.
#' @noRd
#' @examples
#' graphres <- graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y)
#' V(graphres)$leftside <- c(1, 0, 0, 1, 0)
#' V(graphres)$latent <- c(0, 0, 0, 1, 1)
#' V(graphres)$nvals <- c(3, 2, 2, 2, 2)
#' V(graphres)$exposure <- c(0, 1, 0, 0, 0)
#' V(graphres)$outcome <- c(0, 0, 1, 0, 0)
#' E(graphres)$rlconnect <- c(0, 0, 0, 0, 0)
#' E(graphres)$edge.monotone <- c(0, 0, 0, 0, 0)
#' constrainttext <- "X(Z = 1) >= X(Z = 0)"
#' constraintsparsecheck(constrainttext = constrainttext, graphres = graphres) # TRUE
constraintsparsecheck <- function(constrainttext, graphres) {
    obsnames <-
        names(V(graphres)[!names(V(graphres)) %in% c("Ur", "Ul")])
    parsed.ctest <- tryCatch(
        expr = parse_constraints( # Note: does not validate!
            constraints = constrainttext,
            obsnames = obsnames
        ),
        error = function(e) {
            "fail"
        }
    )
    if (!is.list(parsed.ctest)) {
        error_message <- "Unable to parse constraints!"
        if (isRunning()) {
            showNotification(
                ui = error_message,
                type = "error"
            )
        } else {
            message(error_message)
        }
        return(FALSE)
    }
    TRUE
}

# Check that the variables in a parsed constraint actually appear in the DAG.
# parsed_constraints: A 'data.frame' with 5 columns, e.g.,
# leftout rightout operator leftcond rightcond
# 1       X        X       >=      Z=1       Z=0
# as output by 'parse_constraints' with "X(Z = 1) >= X(Z = 0)".
#' Given a DAG, check that the variables in a parsed constraint are in the vertex set of that DAG.
#' @param parsed_constraints A data frame representing a parsed constraint 
#' (as returned by \code{parse_constraints}).
#' @param graphres An \code{igraph} object representing a DAG.
#' @return \code{TRUE} if the variable names in\code{parsed_constraints} correspond to ones in \code{V(graphres)}; else \code{FALSE}.
#' @noRd
#' @examples
#' graphres <- graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y)
#' V(graphres)$leftside <- c(1, 0, 0, 1, 0)
#' V(graphres)$latent <- c(0, 0, 0, 1, 1)
#' V(graphres)$nvals <- c(3, 2, 2, 2, 2)
#' V(graphres)$exposure <- c(0, 1, 0, 0, 0)
#' V(graphres)$outcome <- c(0, 0, 1, 0, 0)
#' E(graphres)$rlconnect <- c(0, 0, 0, 0, 0)
#' E(graphres)$edge.monotone <- c(0, 0, 0, 0, 0)
#' constrainttext <- "X(Z = 1) >= X(Z = 0)"
#' obsnames <- names(V(graphres)[!names(V(graphres)) %in% c("Ur", "Ul")])
#' parsed_constraints <- parse_constraints(constraints = constrainttext, obsnames = obsnames)
#' constraintsnamecheck(parsed_constraints = parsed_constraints, graphres = graphres) # TRUE
constraintsnamecheck <- function(parsed_constraints, graphres) {
    allnmes <- unique(c(
        parsed_constraints$leftout,
        parsed_constraints$rightout,
        gsub(
            pattern = "=\\d+",
            replacement = "",
            x = c(
                parsed_constraints$leftcond,
                parsed_constraints$rightcond
            )
        )
    ))
    realnms <- names(V(graphres))
    if (any(!allnmes %in% realnms |
            !is.na(suppressWarnings(expr = as.numeric(allnmes))))) {
        error_message <-
            sprintf(
                "Names %s in constraint not specified in graph!",
                paste(allnmes[which(!allnmes %in% realnms)],
                      collapse = ", "
                )
            )
        if (isRunning()) {
            showNotification(
                ui = error_message,
                type = "error"
            )
        } else {
            message(error_message)
        }
        return(FALSE)
    }
    TRUE
}

# Check validity of relations in parsed constraint.
#' Check that the relations in parsed constraints are valid, 
#' i.e., one of "==", "<", ">", "<=", ">=".
#' @param parsed_constraints A data frame representing a parsed constraint 
#' (as returned by \code{parse_constraints}).
#' @return \code{TRUE} if the realations in\code{parsed_constraints} are valid; else \code{FALSE}.
#' @noRd
#' @examples
#' constrainttext <- "X(Z = 1) >= X(Z = 0)"
#' obsnames <- names(V(graphres)[!names(V(graphres)) %in% c("Ur", "Ul")])
#' parsed_constraints <- parse_constraints(constraints = constrainttext, obsnames = obsnames)
#' constraintsoperatorcheck(parsed_constraints = parsed_constraints) # TRUE
constraintsoperatorcheck <- function(parsed_constraints) {
    if (any(!parsed_constraints$operator %in% c("==", "<", ">", "<=", ">="))) {
        error_message <- "Operator not allowed!"
        if (isRunning()) {
            showNotification(
                ui = error_message,
                type = "error"
            )
        } else {
            message(error_message)
        }
        return(FALSE)
    }
    TRUE
}

# A complete check of user-provided constraint.
#' Check constraints
#' 
#' Check that a user-provided constraint is parsable, has valid variables and relations.
#' @param constrainttext A string representing a constraint.
#' @param graphres An \code{igraph} object representing a DAG.
#' @return \code{TRUE} if all check pass; else \code{FALSE}.
#' @export
#' @examples
#' graphres <- graph_from_literal(Z -+ X, X -+ Y, Ul -+ Z, Ur -+ X, Ur -+ Y)
#' V(graphres)$leftside <- c(1, 0, 0, 1, 0)
#' V(graphres)$latent <- c(0, 0, 0, 1, 1)
#' V(graphres)$nvals <- c(3, 2, 2, 2, 2)
#' V(graphres)$exposure <- c(0, 1, 0, 0, 0)
#' V(graphres)$outcome <- c(0, 0, 1, 0, 0)
#' E(graphres)$rlconnect <- c(0, 0, 0, 0, 0)
#' E(graphres)$edge.monotone <- c(0, 0, 0, 0, 0)
#' constrainttext <- "X(Z = 1) >= X(Z = 0)"
#' constraintscheck(constrainttext = constrainttext, graphres = graphres) # TRUE
constraintscheck <- function(constrainttext, graphres) {
    if (constraintsparsecheck(constrainttext = constrainttext, graphres = graphres)) {
        obsnames <-
            names(V(graphres)[!names(V(graphres)) %in% c("Ur", "Ul")])
        parsed_constraints <-
            parse_constraints( # Note: does not validate!
                constraints = constrainttext,
                obsnames = obsnames
            )
        if (constraintsnamecheck(parsed_constraints = parsed_constraints, graphres = graphres)) {
            if (constraintsoperatorcheck(parsed_constraints = parsed_constraints)) {
                return(TRUE)
            }
        }
    }
    FALSE
}

#' Check conditions on causal problem
#'
#' Check that a given causal problem (a causal DAG together with a causal query)
#' satisfies conditions that guarantee that the optimization problem is linear.
#'
#' @param digraph An \code{igraph} object representing a digraph.
#'
#' Expected vertex attributes: \code{leftside}, \code{latent} and \code{nvals}.
#'
#' Optional vertex attributes: \code{exposure} and \code{outcome}.
#'
#' Expected edge attributes: \code{rlconnect} and \code{edge.monotone}.
#'
#' @param query A string representing a causal query / effect.
#'
#' @return \code{TRUE} if conditions are met; \code{FALSE} otherwise.
#'
#' @export
#'
#' @examples
#' b <- graph_from_literal(X - +Y, Ur - +X, Ur - +Y)
#' V(b)$leftside <- c(0, 0, 0)
#' V(b)$latent <- c(0, 0, 1)
#' V(b)$nvals <- c(2, 2, 2)
#' V(b)$exposure <- c(1, 0, 0)
#' V(b)$outcome <- c(0, 1, 0)
#' E(b)$rlconnect <- c(0, 0, 0)
#' E(b)$edge.monotone <- c(0, 0, 0)
#' effectt <- "p{Y(X=1)=1}-p{Y(X=0)=1}"
#' causalproblemcheck(digraph = b, query = effectt)
#'
causalproblemcheck <- function(digraph, query) {
    if (graphrescheck(graphres = digraph)) {
        if (querycheck(effecttext = query, graphres = digraph)) {
            return(TRUE)
        }
    }
    FALSE
}


#' Sample from a Dirichlet distribution
#' 
#' Generate a random vector from the k-dimensional symmetric Dirichlet distribution with concentration parameter alpha
#' 
#' @param k Length of the vector
#' @param alpha Concentration parameters
#' @returns a numeric vector
#' @export
#' @examples
#' qvals <- rdirichlet(16, 1)
#' sum(qvals)
rdirichlet <- function(k, alpha = 1) {
    tmp <- rgamma(k, alpha)
    tmp / sum(tmp)
}



#' Find all paths in a causal model
#' 
#' Given a set of response functions, find all directed paths from from to to 
#' 
#' @param respvars A set of response functions as created by \link{create_response_function}
#' @param from A character string indicating the start of the path
#' @param to A character string indicating the end of the path
#' @returns A list with all the paths or a list with NULL if there are none
#' @export
#' @examples
#'  b <- initialize_graph(igraph::graph_from_literal(X -+ Z, Z -+ Y, X -+ Y, Ur -+ Z, Ur -+ Y))
#'  medmod <- create_response_function(b)
#'  find_all_paths(medmod, "X", "Y")
#'  igraph::all_simple_paths(b, "X", "Y", mode = "out")
#' 
find_all_paths <- function(respvars, from, to) {
    
    parent_lookup <- lapply(respvars, \(var) {
        
        unlist(lapply(var$values, \(fun) {
            names(formals(fun))
        })) |> unique()
        
    })
    
    pathlist <- list(to)
    k <- 0
    repeat {
        newplist <- pathlist
        toadd <- NULL
        toremove <- NULL
        for(i in 1:length(pathlist)) {
            if(is.null(parent_lookup[[pathlist[[i]][1]]])) {
                newplist[[i]] <- c(path)
            } else {
                toadd[[i]] <- lapply(parent_lookup[[pathlist[[i]][1]]], \(x) c(x, pathlist[[i]]))
                toremove <- c(toremove, i)
            }
        }
        if(is.null(toremove)) break

                pathlist <- pathlist[-toremove]
        for(i in 1:length(toadd)) {
            pathlist <- c(pathlist, toadd[[i]])
        }
        
        k <- k + 1
        if(k > 1000) {
            stop("Greater than 1000 cycles, something wrong")
        }

    }
    
    unlist(lapply(pathlist, \(x) {
      if(x[1] == from & x[length(x)] == to) {
          paste(x , collapse = " -> ")
          } else NULL  
    }))
    
    
    
    
}
