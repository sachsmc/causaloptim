#' @import igraph shiny
#' @importFrom graphics legend plot
#' @importFrom stats runif
NULL


#' Analyze the causal graph to determine constraints and objective
#' 
#' The graph must contain edge attributes named "leftside" and "lrconnect"
#' that takes values 0 and 1. Only one edge may have a value 1 for lrconnect. 
#' The shiny app returns a graph in this format. 
#' 
#' @param graph An \link[igraph]{aaa-igraph-package} object that represents a directed acyclic graph
#' @param constraints A vector of character strings that represent the constraints
#' @param effectt A character string that represents the causal effect of interest
#' 
#' @return A list with the following components. This list can be passed to \link{optimize_effect} which interfaces with Balke's code: 
#'     \describe{
#'         \item{variables}{Character vector of variable names of potential outcomes, these start with 'q' to match Balke's notation} 
#'         \item{parameters}{Character vector of parameter names of observed probabilities, these start with 'p' to match Balke's notation}
#'         \item{constraints}{Character vector of parsed constraints}
#'         \item{objective}{Character string defining the objective to be optimized in terms of the variables}
#'         \item{p.vals}{Matrix of all possible values of the observed data vector, corresponding to the list of parameters.}
#'         \item{q.vals}{Matrix of all possible values of the response function form of the potential outcomes, corresponding to the list of variables.}
#'         \item{objective.nonreduced}{The objective in terms of the original variables, before algebraic variable reduction. The nonreduced variables can be obtained by concatenating the columns of q.vals.}
#'         \item{response.functions}{List of response functions.}
#'     }
#' 
#' @export
#' @examples 
#' ### confounded exposure and outcome

#' b <- igraph::graph_from_literal(X -+ Y, Ur -+ X, Ur -+ Y)
#' V(b)$leftside <- c(0,0,0)
#' V(b)$latent <- c(0,0,1)
#' E(b)$rlconnect <- E(b)$edge.monotone <- c(0, 0, 0)
#' analyze_graph(b, constraints = NULL, effectt = "p{Y(X = 1) = 1} - p{Y(X = 0) = 1}")

analyze_graph <- function(graph, constraints, effectt) {
    
    leftind <- vertex_attr(graph)$leftside
    
    if(sum(edge_attr(graph)$rlconnect) > 0) stop("No edges can go from right to left")
    
    cond.vars <- V(graph)[leftind == 1 & names(V(graph)) != "Ul"]
    right.vars <- V(graph)[leftind == 0 & names(V(graph)) != "Ur"] 
    ## allow outcome to be latent? but how
    ## all variables that have parents have a response function, even if unobserved
    
    observed.variables <- V(graph)[V(graph)$latent == 0]
    
    var.values <- lapply(names(observed.variables), function(i) c(0, 1))
    names(var.values) <- names(observed.variables)
    
    p.vals <- do.call(expand.grid, var.values)  # p vals need to be based on observed variables only
    
    jd <- do.call(paste0, p.vals[, names(right.vars[right.vars$latent == 0]), drop = FALSE])
    cond <- do.call(paste0, p.vals[, names(cond.vars[cond.vars$latent == 0]), drop = FALSE])
    
    parameters <- paste0("p", paste(jd, cond, sep = "_"))
    parameters.key <- paste(paste(names(right.vars[right.vars$latent == 0]), collapse = ""), 
                            paste(names(cond.vars[cond.vars$latent == 0]), collapse = ""), sep = "_")
    
    
    ## response variable for each variable observed or unobserved
    
    obsvars <- c(right.vars, cond.vars)
    respvars <- vector(mode = "list", length = length(obsvars))
    names(respvars) <- names(obsvars)
    for(ini in 1:length(obsvars)) {
        
        i <- obsvars[ini]
        intoi <- graph[from = obsvars, to = rep(i, length(obsvars))]
        nstart <- prod(c(2 , 2 ^ intoi))
        
        parents <- names(obsvars[as.logical(intoi)])
        arglist <- vector(mode = "list", length = length(parents))
        names(arglist) <- parents
        
        if(length(parents) == 0) {
            values <- list(function(){ 0 }, function() { 1 })
        } else {
            
            
            ## da matrix
            
            poss.ins <- expand.grid(lapply(parents, function(x) c(0, 1)))
            colnames(poss.ins) <- parents
            ini.outs <- expand.grid(lapply(1:nrow(poss.ins), function(x) c(0, 1)))
            
            args <- vector(mode = "list", length = ncol(poss.ins))
            names(args) <- parents
            
            values <- vector(mode = "list", length = nrow(ini.outs))
            for(j in 1:nrow(ini.outs)) {
                f.tmp <- function() {}
                formals(f.tmp) <- as.pairlist(args)
                
                ## build body
                a1 <- paste("paste0(", paste(parents, collapse = ", "), ")")
                switchnames <- paste0("`", do.call(paste0, poss.ins), "`")
                switchbod <- as.list(c(ini.outs[j, ]))
                fbod <- paste0("switch(", a1, ", ", paste(paste(switchnames, switchbod, sep = " = "), collapse = ", "), ")")
                body(f.tmp) <- parse(text = fbod)
                environment(f.tmp) <- parent.frame()
                
                values[[j]] <- f.tmp
                
            }
            
        }
        
        respvars[[ini]] <- list(index = 0:(length(values) - 1), values = values)
        
    }
    
    
    ## check for any monotonicity assumptions
    if(any(E(graph)$edge.monotone == 1)) {
      which.monotone <- which(E(graph)$edge.monotone == 1)
      for(j in which.monotone) {
        
        head.mono <- names(head_of(graph, j))
        tail.mono <- names(tail_of(graph, j))
        
        tmpenv.1 <- list(1)
        tmpenv.0 <- list(0)
        names(tmpenv.1) <- names(tmpenv.0) <- tail.mono
        
        resp.out.0 <- unlist(lapply(respvars[[head.mono]]$values, function(f) do.call(f, tmpenv.0)))
        resp.out.1 <- unlist(lapply(respvars[[head.mono]]$values, function(f) do.call(f, tmpenv.1)))
        
        settozeroindex <- respvars[[head.mono]]$index[resp.out.0 > resp.out.1]
        removedex <- respvars[[head.mono]]$index == settozeroindex
      
        respvars[[head.mono]]$index <- respvars[[head.mono]]$index[!removedex] 
        respvars[[head.mono]]$values <- respvars[[head.mono]]$values[!removedex] 
          
      }
    }
    
    ## additional constraints
    
    notsatlist <- NULL
    if(!is.null(constraints)) {
      
      
      parsed.constraints <- parse_constraints(constraints, names(obsvars)) 
      
      ### apply parsed constraints
      
      for(j in 1:nrow(parsed.constraints)) {
        
        iin <- parsed.constraints[j, ]
        tmpenv.left <- tmpenv.right <- list()
        tmpenv.left <- within(tmpenv.left, eval(parse(text = iin$leftcond)))
        tmpenv.right <- within(tmpenv.right, eval(parse(text = iin$rightcond)))
        
        resp.out.left <- unlist(lapply(respvars[[iin$leftout]]$values, function(f) do.call(f, tmpenv.left)))
        resp.out.right <- unlist(lapply(respvars[[iin$rightout]]$values, function(f) do.call(f, tmpenv.right)))
        
        if(iin$rightout %in% c("0", "1")) {
          resp.out.right <- rep(iin$rightout, length(resp.out.left))
        }
        
        if(iin$leftout == iin$rightout | iin$rightout %in% c("0", "1")) {  ## constraints are for the same counterfactual, these lead to removals of qs
            settozeroindex <- respvars[[iin$leftout]]$index[!do.call(iin$operator, list(resp.out.left, resp.out.right))]
            
            if(length(settozeroindex) > 0) {
              removedex <- respvars[[iin$leftout]]$index %in% settozeroindex
              
              respvars[[iin$leftout]]$index <- respvars[[iin$leftout]]$index[!removedex] 
              respvars[[iin$leftout]]$values <- respvars[[iin$leftout]]$values[!removedex] 
              
            }
            
        } else {  ## otherwise these lead to added constraints
          
          lnotsat <- length(notsatlist)
          finddex <- !do.call(iin$operator, expand.grid(resp.out.left, resp.out.right))
          notsat <- expand.grid(respvars[[iin$leftout]]$index, respvars[[iin$rightout]]$index)[finddex, ]
          colnames(notsat) <- c(iin$leftout, iin$rightout)  
          ## these sets of response variables do not satisfy the constraints and should be removed from the q.vals table
          notsatlist[[lnotsat + 1]] <- notsat
           
        }
        
      }
        
      } #endif
      
    
    q.vals.all <- do.call(expand.grid, lapply(respvars, "[[", 1))
    ## remove rows not in notsatlist
    if(length(notsatlist) > 0) {
      
      for(j in 1:length(notsatlist)) {
        
        q.vals.tmp <- merge(q.vals.all, cbind(notsatlist[[j]], remove=1), all.x = TRUE)
        q.vals.all <- q.vals.tmp[is.na(q.vals.tmp$remove), -ncol(q.vals.tmp)]
        
      }
      
    }
    
    q.vals <- do.call(expand.grid, lapply(respvars, "[[", 1)[which(obsvars %in% right.vars)])
    
    variables <- paste0("q", do.call(paste0, q.vals))
    
    q.vals.tmp <- cbind(q.vals, vars = variables, stringsAsFactors = FALSE)
    q.vals.all.lookup <- merge(q.vals.all, q.vals.tmp, by = names(right.vars), sort = TRUE)
    
    variables <- as.character(unique(q.vals.all.lookup$vars))
    ## constraints identify set of qs that correspond to observed p.vals
    
    
    gee_r <- function(r, i) {
      
      parents <- adjacent_vertices(graph, obsvars[i], "in")[[1]]
      parents <- parents[!names(parents) %in% c("Ul", "Ur")]
      
      
      if (length(parents) == 0){
        
        x <- respvars[[names(obsvars[[i]])]]$values[[which(respvars[[names(obsvars[[i]])]]$index == r[i])]]
        do.call(x, list())
        
      } else {
        
        lookin <- lapply(names(parents), function(gu) {
          
          as.numeric(gee_r(r, which(names(obsvars) == gu)))
          
        })
        names(lookin) <- names(parents)
        inres <- respvars[[names(obsvars[[i]])]]$values[[which(respvars[[names(obsvars[[i]])]]$index == r[i])]]
        do.call(inres, lookin)
        
      }
    }
    
    
    res.mat <- matrix(NA, ncol = ncol(q.vals.all), nrow = nrow(q.vals.all))
    for(k in 1:nrow(q.vals.all)) {
      for(j in 1:ncol(q.vals.all)) {
        res.mat[k, j] <- gee_r(r = unlist(q.vals.all.lookup[k, -ncol(q.vals.all.lookup)]), i = j)
        
      }
    }
    colnames(res.mat) <- names(obsvars)
    
    removeprows <- rep(0, nrow(p.vals))
    p.constraints <- rep(NA, nrow(p.vals) + 1)
    p.constraints[1] <- paste(paste(variables, collapse= " + "), " = 1")
    for(pj in 1:nrow(p.vals)) {
      
      p.chk <- do.call(rbind, lapply(1:nrow(res.mat), function(i) p.vals[pj, , drop = FALSE]))
      inp <- apply(res.mat[, colnames(p.chk), drop = FALSE] == p.chk, 1, all)
      
      if(!any(inp)) {
        removeprows[pj] <- 1
        next
        
      } else {
        q.match <- q.vals.all.lookup[inp, ncol(q.vals.all.lookup)]
        if(length(q.match) == 0) q.match <- "0"
        p.constraints[pj + 1] <- paste(parameters[pj], "=", paste(unique(q.match), collapse = " + "))
        
      }
    }
    
    p.vals <- p.vals[removeprows == 0, , drop = FALSE]
    parameters <- parameters[removeprows == 0]
    p.constraints <- p.constraints[!is.na(p.constraints)]
    
    baseind <- rep(FALSE, length(p.constraints))
    baseind[1:nrow(p.vals)] <- TRUE
    attr(p.constraints, "baseconstr") <- baseind
    ## determine objective based on exposure and outcome in terms of qs

    effect <- parse_effect(effectt)
    
    chk0 <- lapply(effect$vars, btm_var)
    
    interven.vars <- unique(unlist(chk0))
    
    ## check that children of intervention sets are on the right
    
    any.children.onleft <- sapply(interven.vars, function(v) {
      
      children <- neighbors(graph, V(graph)[v], mode = "out")
      any(children$leftside == 1)
      
    })
    
    if(any(any.children.onleft) == TRUE) {
      stop(sprintf("Cannot intervene on %s because it has children on the leftside!", 
                       paste(interven.vars[which(any.children.onleft)], collapse = ", ")))
    }
    
    if("oper" %in% names(chk0) & !chk0["oper"] %in% c("+", "-")) {
      stop(sprintf("Operator '%s' not allowed!", chk0["oper"]))
    }
    
    allnmes <- unique(unlist(lapply(effect$vars, names)))
    
    realnms <- names(V(graph))
    if(any(!allnmes %in% realnms)) {
      
      stop(sprintf("Names %s in effect not specified in graph!", 
                       paste(allnmes[which(!allnmes %in% realnms)], collapse = ", ")))
      
    }
    
    
    var.eff <- NULL
    for(v in 1:length(effect$vars)) {

      #nest 
      thisterm <- effect$vars[[v]]
      
      res.mat.list <- vector(mode = "list", length = length(thisterm))
      
      for(v2 in 1:length(thisterm)){
      
      thisvar <- thisterm[[v2]]
      outcome <- V(graph)[names(V(graph)) == names(thisterm)[v2]]
      intervene <- vector(mode = "list")
      
      if(effect$pcheck[[v]][v2] == FALSE) { ## observation
        
        
        gee_r <- function(r, i) {
          
          parents <- adjacent_vertices(graph, obsvars[i], "in")[[1]]
          parents <- parents[!names(parents) %in% c("Ul", "Ur")]
          
          
          if (length(parents) == 0){
            
            x <- respvars[[names(obsvars[[i]])]]$values[[which(respvars[[names(obsvars[[i]])]]$index == r[i])]]
            do.call(x, list())
            
          } else {
            
            lookin <- lapply(names(parents), function(gu) {
              
              as.numeric(gee_r(r, which(names(obsvars) == gu)))
              
            })
            names(lookin) <- names(parents)
            inres <- respvars[[names(obsvars[[i]])]]$values[[which(respvars[[names(obsvars[[i]])]]$index == r[i])]]
            do.call(inres, lookin)
            
          }
        }
        
        
        res.mat <- matrix(NA, ncol = ncol(q.vals.all), nrow = nrow(q.vals.all))
        for(k in 1:nrow(q.vals.all)) {
          for(j in 1:ncol(q.vals.all)) {
            res.mat[k, j] <- gee_r(r = unlist(q.vals.all.lookup[k, -ncol(q.vals.all.lookup)]), i = j)
            
          }
        }
        colnames(res.mat) <- names(obsvars)
        
        res.mat.list[[v2]] <- res.mat
        
        
      } else { ## intervention
      
        thisintervene <- unlist(list_to_path(thisvar, names(outcome)))
        basevars <- sapply(strsplit(names(thisintervene), " -> "), "[", 1)
        ## check for missing paths from intervention sets to outcome
        
        isets <- unique(btm_var(thisvar))
        missingpaths <- lapply(isets, function(cc) {
          allpaths <- all_simple_paths(graph, from = cc, to = names(outcome), mode = "out")
          paths2 <- unlist(lapply(allpaths, function(x) paste(names(x), collapse = " -> ")))
          setdiff(paths2, names(thisintervene))
        })
        for(pp in 1:length(missingpaths)) {
          
          if(length(missingpaths[[pp]]) == 0) {
            next
          }
          addval <- thisintervene[which(isets[pp] == basevars)[1]]
          addval2 <- rep(addval, length(missingpaths[[pp]]))
          names(addval2) <- missingpaths[[pp]]
          thisintervene <- c(thisintervene, addval2)
            
        }
        
      
      gee_rA <- function(r, i, path = NULL) {

        parents <- adjacent_vertices(graph, obsvars[i], "in")[[1]]
        parents <- parents[!names(parents) %in% c("Ul", "Ur")]
        
        if(!is.null(path)){
          #thisintervene <- intervene[[childcall]]
        }
        if(!is.null(path) && path %in% names(thisintervene)) {

          as.numeric(thisintervene[[path]])

        } else if (length(parents) == 0){

          x <- respvars[[names(obsvars[[i]])]]$values[[which(respvars[[names(obsvars[[i]])]]$index == r[i])]]
          do.call(x, list())

        } else {

          lookin <- lapply(names(parents), function(gu) {

            as.numeric(gee_rA(r, which(names(obsvars) == gu), path = paste(gu, "->", path)))

          })
          names(lookin) <- names(parents)
          inres <- respvars[[names(obsvars[[i]])]]$values[[which(respvars[[names(obsvars[[i]])]]$index == r[i])]]
          do.call(inres, lookin)

        }
      }


      res.mat <- matrix(NA, ncol = ncol(q.vals.all), nrow = nrow(q.vals.all))
      for(k in 1:nrow(q.vals.all)) {
        for(j in 1:ncol(q.vals.all)) {
          res.mat[k, j] <- gee_rA(r = unlist(q.vals.all.lookup[k, -ncol(q.vals.all.lookup)]), i = j, 
                                  path = names(obsvars)[j])

        }
      }
      colnames(res.mat) <- names(obsvars)
     
      res.mat.list[[v2]] <- res.mat
      
      }
      
      }
      
    
      
    
      var.dex <- rep(TRUE, nrow(q.vals.all))
      for(i in 1:length(effect$values[[v]])) {
        
        var.dex <- var.dex & res.mat.list[[i]][, names(effect$values[[v]])[i]] == effect$values[[v]][[i]]
        
      }
      var.eff[[v]] <- unique(as.character(q.vals.all.lookup[var.dex, "vars"]))
      
    }
    
    
    ## handle addition and subtraction based on operator
    ## accumulate final effect based on subtraction and addition
    
   
    
    objective <- list(var.eff[[1]])
    if(length(var.eff) > 1 & is.null(effect$oper) | (length(effect$oper) != length(var.eff) -1)){
      stop("Missing operator")
    }
    
    if(!is.null(effect$oper) & length(effect$oper) > 0) {
    curreff <- 2
    for(opp in 1:length(effect$oper)) {
      
      if(effect$oper[[opp]] == "-") {
        
        resss <- symb.subtract(objective[[curreff - 1]], var.eff[[curreff]])
        objective[[curreff - 1]] <- resss[[1]]
        objective[[curreff]] <- resss[[2]]
        curreff <- curreff + 1
        
      } else if(effect$oper[[opp]] == "+") {
        
        objective[[curreff]] <- var.eff[[curreff]]
        curreff <- curreff + 1
        
      }
      
    }
    }
    
    
    special.terms <- grepl("p(.*) = 0", p.constraints)
    
    red.sets <- const.to.sets(p.constraints[!special.terms], objective)
    
    
    objective.fin <- paste(red.sets$objective.terms[[1]], collapse = " + ")
    
   
    if(!is.null(effect$oper) & length(effect$oper) > 0 & length(red.sets$objective.terms) > 1) {
      
      for(opp in 1:length(effect$oper)) {
        
        thiscol <- ifelse(effect$oper[[opp]] == "-", " - ", " + ")
        objective.fin <- paste(objective.fin, effect$oper[[opp]], 
                               paste(red.sets$objective.terms[[opp + 1]], collapse = thiscol))
        
      }
      
    }
    
  
    
    attr(parameters, "key") <- parameters.key
    attr(parameters, "rightvars") <- names(right.vars[right.vars$latent == 0])
    attr(parameters, "condvars") <- names(cond.vars[cond.vars$latent == 0])
    
    list(variables = red.sets$variables, parameters = parameters, 
         constraints = c(red.sets$constr, p.constraints[special.terms]), 
         objective = objective.fin, p.vals = p.vals, q.vals = q.vals, 
         objective.nonreduced = objective, response.functions = respvars)
    
    
}
