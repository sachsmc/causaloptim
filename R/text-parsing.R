
#' Paste with asterisk sep
#' 
#' @param ... Things to paste together
#' @noRd
pastestar <- function(...) paste(..., sep = "*")


#' Expand potential outcome conditions
#' 
#' @param cond Text string of the condition
#' @param obsnames Vector of names of observed variables
#' @noRd
expand_cond <- function(cond, obsnames) {
    
    chk1 <- sapply(cond, function(x) substr(x, nchar(x), nchar(x))) %in% obsnames
    if(!any(chk1)) {
        return(paste(cond, collapse = "; "))
    }
    
    retcond <- cond[!chk1]
    
    for(j in (1:length(cond))[chk1]) {
        
        newcond <- paste0(substr(cond[j], 1, nchar(cond[j]) - 1), c("0", "1"))
        retcond <- paste(retcond, newcond, sep = "; ")
        
    }
    
    retcond
    
}

#' Translate lists of constraints to lists of vectors
#' 
#' @param constr List of constraint terms as character strings
#' @param objterms Vector of terms in the objective function
#' @noRd
const.to.sets <- function(constr, objterms) {
    
    sets <- lapply(strsplit(constr, " = "), function(x) {
        
        tmp1 <- grep("q", x, value = TRUE)  
        trimws(unlist(strsplit(tmp1, "( \\+ | - )")))
        
    })
    
    pnames <- lapply(strsplit(constr, " = "), function(x) {
        
        tmp1 <- grep("(q)", x, value = TRUE, invert = TRUE)
        trimws(tmp1)
        
    })
    
    K <- length(sets)
    sets[(K+1):(K + length(objterms))]<- objterms
    
    
    reduced.sets <- reduce.sets(sets)
    
    constr.new <- reduced.sets[1:K]
    obj.new <- reduced.sets[(K+1):(K+length(objterms))]
    var.new <- reduced.sets[[1]]
    
    list(constr = paste(unlist(lapply(constr.new, paste, collapse = " + ")), " = ", pnames), 
         objective.terms = obj.new, 
         variables = var.new, 
         raw.sets = constr.new, 
         pnames = pnames)
    
    
}



#' Algebraically reduce sets
#' 
#' Identifies and reduces redundant variables
#' 
#' @param sets List of constraints as sets of variables
#' @noRd
reduce.sets <- function(sets){
    #find commonalities
    K <- length(sets)
    fullset <- sets[[1]]
    common <- list()
    left <- fullset
    r <- 1
    while(length(left)>1){
        tmp <- left[1]
        for(i in 2:length(left)){
            together <- vector(length=K)
            for(k in 1:K){
                if((left[1]%in%sets[[k]] & left[i]%in%sets[[k]]) |
                   (!left[1]%in%sets[[k]] & !left[i]%in%sets[[k]]))
                    together[k] <- TRUE  
            }
            if(sum(together)==K)
                tmp <- c(tmp, left[i])     
        }
        left <- setdiff(left, tmp)
        if(length(tmp)>1){
            common[[r]] <- tmp
            r <- r+1
        }
    }
    
    if(length(common) == 0) return(sets)
    #make reduction
    R <- length(common)
    for(r in 1:R){
        tmp <- common[[r]][-1]
        for(k in 1:K){
            ind <- match(tmp, sets[[k]])
            if(sum(is.na(ind))==0)
                sets[[k]] <- sets[[k]][-ind]
        }  
    }
    #simplify names
    fullset <- sets[[1]]
    for(i in 1:length(sets)){
        tmp <- sets[[i]]
        for(j in 1:length(tmp)){
            tmp[j] <- paste0("q", match(tmp[j], fullset))
        }
        sets[[i]] <- tmp
    }
    return(sets)  
}

#' Symbolic subtraction
#' 
#' Like setdiff but doesn't remove duplicates x1 - x2
#' @param x1 First term (subtract from)
#' @param x2 Second term (subtract)
#' @noRd
symb.subtract <- function(x1, x2) {
    ## x1 - x2
    res1 <- x1
    res2 <- NULL
    for(j in x2) {
        if(!j %in% res1){
            res2 <- c(res2, j)
        } else {
            res1 <- res1[-which(res1 == j)[1]]  
            
        }
    }
    list(res1, res2)
    
}

#' Parse text that defines a causal effect
#' 
#' @param text Character string
#' @return A nested list that contains the following components:
#' \describe{
#'        \item{vars}{For each element of the causal query, this 
#'        indicates potential outcomes as names of the list elements, 
#'        the variables that they depend on, and the values that any variables are being fixed to.}
#'        \item{oper}{The vector of operators (addition or subtraction) that combine the terms of the causal query.}
#'        \item{values}{The values that the potential outcomes are set to in the query.}
#'        \item{pcheck}{List of logicals for each element of the query that are TRUE if the element 
#'        is a potential outcome and FALSE if it is an observational quantity.}
#'        }
#' @export
#' @examples
#' effectt <- "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}"
#' parse_effect(text = effectt)
parse_effect <- function(text) {
    
    text <- gsub("(\\n|\\t| )", "", text)
    
    terms0 <- strsplit(text, split = "-|\\+")[[1]]
    opers <- as.list(grep("-|\\+", strsplit(text, "")[[1]], value = TRUE))
    
    terms0 <- gsub("(p\\{)|(\\})", "", terms0)
    
    termssplit <- lapply(terms0, function(x) {
        
        strsplit(x, ";")[[1]]
        
    })
    
    res.effs <- res.vals <- res.pcheck <- vector(mode = "list", length= length(termssplit))
    
    for(j in 1:length(termssplit)) {
        terms0 <- termssplit[[j]]
        
        parse1 <- lapply(terms0, function(x) {
            rmain <- unlist(strsplit(x, "=\\d+$"))
            val0 <- substr(x = x, start = nchar(rmain) + 2, stop = nchar(x))
            list(as.numeric(val0), rmain)
        })
        
        terms <- lapply(parse1, "[[", 2)
        vals <- lapply(parse1, "[[", 1)
        
        pcheck <- grepl("(", terms, fixed = TRUE)
        pterms <- gsub("(", " = list(", terms, fixed = TRUE)
        
        parsedEffect <- vector(mode = "list", length = length(pterms))
        for(k in 1:length(parsedEffect)) {
            if(pcheck[k] == TRUE) {
            parsedEffect[[k]] <- 
                eval(parse(text = pterms[k], keep.source = FALSE))
            names(parsedEffect)[[k]] <- strsplit(pterms[k], " = ")[[1]][1]
        
            } else {
                
                parsedEffect[[k]] <- pterms[k]
                names(parsedEffect)[[k]] <- pterms[k]
                
            }
        }
    
        names(vals) <- names(parsedEffect)
        res.effs[[j]] <- parsedEffect
        res.vals[[j]] <- vals
        res.pcheck[[j]] <- pcheck
        
    }
    
    list(vars = res.effs, oper = opers, values = res.vals, pcheck = res.pcheck)
    
}

#' Parse text that defines a the constraints
#' 
#' @param constraints A list of character strings
#' @param obsnames Vector of names of the observed variables in the graph
#' @return A data frame with columns indicating the variables being constrained, what the values of their parents are for the constraints, and the operator defining the constraint (equality or inequalities).
#' @export
#' @examples
#' constrainttext <- "X(Z = 1) >= X(Z = 0)"
#' obsnames <- c("Z", "X", "Y")
#' parse_constraints(constraints = constrainttext, obsnames = obsnames)
parse_constraints <- function(constraints, obsnames) {
    
    parsed.constraints <- NULL
    for(j in 1:length(constraints)) {
        
        constin <- gsub("\\s", "", constraints[[j]])
        p0 <- strsplit(constin, "\\)")[[1]]
        pl1 <- strsplit(p0[1], "\\(")[[1]]
        
        leftout <- pl1[1]
        leftcond <- strsplit(pl1[-1], ",")[[1]]
        
        opdex <- ifelse(substr(p0[-1], 2, 2) == "=", 2, 1)
        operator <- substr(p0[-1], 1, opdex)
        if(operator == "=") operator <- "=="
        
        pr1 <- strsplit(substr(p0[-1], opdex + 1, nchar(p0[-1])), "\\(")[[1]]
        rightout <- pr1[1]
        
        if(!is.na(suppressWarnings(as.numeric(rightout)))) {
            rightcond2 <- rightout
        } else {
            
            rightcond <- strsplit(gsub("\\)", "", pr1[-1]), ",")[[1]]
            rightcond2 <- expand_cond(rightcond, obsnames)
            
        }
        leftcond2 <- expand_cond(leftcond, obsnames)
        
        
        conds <- expand.grid(leftcond = leftcond2, rightcond = rightcond2, stringsAsFactors = FALSE)
        parsed.constraints <- rbind(parsed.constraints, 
                                    data.frame(leftout = leftout, rightout = rightout, operator, conds, stringsAsFactors = FALSE))
        
    }
    
    parsed.constraints
    
}

#' Latex bounds equations
#' 
#' @param bounds Vector of bounds as returned by \link{optimize_effect_2}
#' @param parameters The parameters object as returned by \link{analyze_graph}
#' @param prob.sym Symbol to use for probability statements in latex, usually "P" or "pr"
#' @param brackets Length 2 vector with opening and closing bracket, usually \code{c("(", ")")}, or \code{c(" \{", "\}")}
#' @return A character string with latex code for the bounds
#' @export
#' @examples
#' b <- graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
#' V(b)$leftside <- c(0,0,0)
#' V(b)$latent <- c(0,0,1)
#' V(b)$nvals <- c(2,2,2)
#' E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0)
#' obj <- analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")
#' bounds <- optimize_effect_2(obj)
#' latex_bounds(bounds$bounds, obj$parameters)
#' latex_bounds(bounds$bounds, obj$parameters, "Pr")
latex_bounds <- function(bounds, parameters, prob.sym = "P", brackets = c("(", ")")) {
    
    
    lkeys <- strsplit(gsub("p", "", sapply(strsplit(parameters, "_"), "[", 1)), "") 
    lkeyup <- as.data.frame(do.call(rbind, lkeys))
    # lkeyup <- do.call(expand.grid, 
    #                   c(lapply(1:length(attr(parameters, "rightvars")), 
    #                            function(x) c("0", "1")), stringsAsFactors = FALSE))
    
    if(length(attr(parameters, "condvars")) == 0) {
        
        probstate <- lapply(1:nrow(lkeyup), function(i) {
            lkey <- lkeyup[i, ]
            paste0(prob.sym, brackets[1], 
                   paste(paste0(attr(parameters, "rightvars"), " = ", lkey), 
                         collapse = ", "), brackets[2])
        })
        namelook <- sapply(1:nrow(lkeyup), function(i) {
            
            paste0("p", paste(lkeyup[i, ], collapse = ""), "_")
            
        })
        
        names(probstate) <- namelook
        
        
    } else {
        
        nr <- length(attr(parameters, "rightvars"))
        nc <- length(attr(parameters, "condvars"))
        
        lrkeys <- lapply(strsplit(gsub("p", "", parameters), ""), function(x) {
            x[x!= "_"]
        }) 
        lrkeyup <- as.data.frame(do.call(rbind, lrkeys))
        
        # lrkeyup <- do.call(expand.grid, c(lapply(1:(nr + nc), 
        #                                          function(x) c("0", "1")), 
        #                                   stringsAsFactors = FALSE))
        
        probstate <- lapply(1:nrow(lrkeyup), function(i) {
            
            lkey <- lrkeyup[i, 1:nr]
            rkey <- lrkeyup[i, (nr + 1):(nr + nc)]
            paste0(prob.sym, brackets[1], paste(paste0(attr(parameters, "rightvars"), " = ", lkey), collapse = ", "), " | ", 
                        paste0(attr(parameters, "condvars"), " = ", rkey, collapse = ", "), brackets[2])
        })
        
        
        namelook <- sapply(1:nrow(lrkeyup), function(i) {
            
            paste0("p", paste(lrkeyup[i, 1:nr], collapse = ""), "_", paste(lrkeyup[i, (nr+1):(nr+nc)], collapse = ""))
            
            })
        
        names(probstate) <- namelook
        
    }
    
    
    ### apply the lookup table
    
    bnd2 <- gsub("\\n}\\n", "", substr(bounds, 8, nchar(bounds)))
    bnd3 <- strsplit(bnd2, "\\n")
    
    for(i in 1:length(probstate)) {
        
        bnd3$lower <- gsub(names(probstate)[i], probstate[[i]], bnd3$lower, fixed = TRUE)
        bnd3$upper <- gsub(names(probstate)[i], probstate[[i]], bnd3$upper, fixed = TRUE)
        
    }
    
    if(length(bnd3$lower) == 1) {
        
        lwr <- bnd3$lower
        
    } else {
        
        l0 <- paste(bnd3$lower, collapse = "\\\\ \n ")
        lwr <- sprintf("\\mbox{max} \\left. \\begin{cases} %s \\end{cases} \\right\\}", l0)
        
        
    }
    
    if(length(bnd3$upper) == 1) {
        
        upr <- bnd3$upper
        
    } else {
        
        r0 <- paste(bnd3$upper, collapse = "\\\\ \n ")
        upr <- sprintf("\\mbox{min} \\left. \\begin{cases} %s \\end{cases} \\right\\}", r0)
        
        
    }
    
    
    return(sprintf("\\[ \n \\mbox{Lower bound} = %s \n \\] \n \\[ \n \\mbox{Upper bound} = %s \n \\] \n", lwr, upr))
    
    
}
