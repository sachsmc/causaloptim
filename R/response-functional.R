#' Analyze the causal graph to determine constraints and objective
#' 
#' The graph must contain edge attributes named "leftside" and "lrconnect"
#' that takes values 0 and 1. Only one edge may have a value 1 for lrconnect. 
#' The shiny app returns a graph in this format. 
#' 
#' @param graph An \link[igraph]{igraph} object that represents a directed acyclic graph
#' @param constraints A vector of character strings that represent the constraints
#' @param effect A character string that represents the causal effect of interest
#' 
#' @return A list with the following components. This list can be passed to \link{optimize_effect} which interfaces with Balke's code: 
#'     \describe{
#'         \item{variables}{Character vector of variable names of potential outcomes, these start with 'q' to match Balke's notation} 
#'         \item{parameters}{Character vector of parameter names of observed probabilities, these start with 'p' to match Balke's notation}
#'         \item{constraints}{Character vector of parsed constraints}
#'         \item{objective}{Character string defining the objective to be optimized in terms of the variables}
#'         \item{p.vals}{Matrix of all possible values of the observed data vector, corresponding to the list of paramters.}
#'         \item{q.vals}{Matrix of all possible values of the response function form of the potential outcomes, corresponding to the list of variables.}
#'     }
#' 
#' @export

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
    parameters.key <- paste(paste(names(right.vars), collapse = ""), paste(names(cond.vars), collapse = ""), sep = "_")
    
    
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
        rightcond <- strsplit(gsub("\\)", "", pr1[-1]), ",")[[1]]
        
        # stopifnot(leftout == rightout)
        ## handle cases like X(Z = 0, Y = Y)
        rightcond2 <- expand_cond(rightcond, names(obsvars))
        leftcond2 <- expand_cond(leftcond, names(obsvars))
        
        conds <- expand.grid(leftcond = leftcond2, rightcond = rightcond2, stringsAsFactors = FALSE)
        parsed.constraints <- rbind(parsed.constraints, 
                                    data.frame(leftout = leftout, rightout = rightout, operator, conds, stringsAsFactors = FALSE))
        
      }
        
      ### apply parsed constraints
      
      for(j in 1:nrow(parsed.constraints)) {
        
        iin <- parsed.constraints[j, ]
        tmpenv.left <- tmpenv.right <- list()
        tmpenv.left <- within(tmpenv.left, eval(parse(text = iin$leftcond)))
        tmpenv.right <- within(tmpenv.right, eval(parse(text = iin$rightcond)))
        
        resp.out.left <- unlist(lapply(respvars[[iin$leftout]]$values, function(f) do.call(f, tmpenv.left)))
        resp.out.right <- unlist(lapply(respvars[[iin$rightout]]$values, function(f) do.call(f, tmpenv.right)))
        
        if(iin$leftout == iin$rightout) {  ## constraints are for the same counterfactual, these lead to removals of qs
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
    
    q.vals.tmp <- cbind(q.vals, vars = variables)
    q.vals.all.lookup <- merge(q.vals.all, q.vals.tmp, by = names(right.vars), sort = TRUE)
    
    variables <- as.character(unique(q.vals.all.lookup$vars))
    ## constraints identify set of qs that correspond to observed p.vals
    
    removeprows <- rep(0, nrow(p.vals))
    p.constraints <- rep(NA, nrow(p.vals) + 1)
    p.constraints[1] <- paste(paste(variables, collapse= " + "), " = 1")
    for(j in 1:nrow(p.vals)) {
        tmpenvir <- p.vals[j, ]
        
        tmp.match <- vector("list", length = length(obsvars))
        names(tmp.match) <- names(obsvars)
        for(i in 1:length(obsvars)) {
            
          neededargs <- names(formals(respvars[[names(obsvars)[i]]]$values[[1]]))
          
          if(all(neededargs %in% names(tmpenvir))) {
          
            cf.vals <- unlist(lapply(respvars[[i]]$values, function(x) {
                arg.names <- names(tmpenvir) %in% names(formals(x))
                
                do.call(x, tmpenvir[arg.names])
            }))
            
            if(names(obsvars)[i] %in% names(tmpenvir)) {
              matchobs <- cf.vals == tmpenvir[,names(obsvars)[i] ]
            } else matchobs <- rep(TRUE, length(cf.vals))
            
            tmp.match[[i]] <- respvars[[i]]$index[matchobs]
          } else {
            
            missargs <- neededargs[!neededargs %in% names(tmpenvir)]
            fillin <- lapply(missargs, function(x) c("0", "1"))
            names(fillin) <- missargs
            
            rownames(tmpenvir) <- NULL
            newtmpenv <- data.frame(tmpenvir, do.call(expand.grid, fillin))
            tmp.match.k <- NULL
            for(k in 1:nrow(newtmpenv)) {
              
              tmpenvir2 <- newtmpenv[k, ]
              cf.vals <- unlist(lapply(respvars[[i]]$values, function(x) {
                arg.names <- names(tmpenvir2) %in% names(formals(x))
                
                do.call(x, tmpenvir2[arg.names])
              }))
              
              if(names(obsvars)[i] %in% names(tmpenvir2)) {
                matchobs <- cf.vals == tmpenvir2[, names(obsvars)[i]]
              } else matchobs <- rep(TRUE, length(cf.vals))
              
              
              tmp.match.k <- c(tmp.match.k, respvars[[i]]$index[matchobs])
              
            }
            tmp.match[[i]] <- sort(unique(tmp.match.k))
            
            
          }
            
        }
        
        obsdex <- expand.grid(tmp.match)
        
        if(nrow(obsdex) == 0) {
          
          removeprows[j] <- 1
          next
          
        } else {
          
          q.match <- merge(obsdex, q.vals.all.lookup)$vars
          if(length(q.match) == 0) q.match <- "0"
          p.constraints[j + 1] <- paste(parameters[j], "=", paste(q.match, collapse = " + "))
          
        }
    }
    
    p.vals <- p.vals[removeprows == 0,]
    parameters <- parameters[removeprows == 0]
    p.constraints <- p.constraints[!is.na(p.constraints)]
    
    baseind <- rep(FALSE, length(p.constraints))
    baseind[1:nrow(p.vals)] <- TRUE
    attr(p.constraints, "baseconstr") <- baseind
    ## determine objective based on exposure and outcome in terms of qs

    effect <- parse_effect(effectt)
    
    var.eff <- NULL
    for(v in 1:length(effect$vars)) {

      thisvar <- effect$vars[[v]]
      outcome <- V(graph)[names(V(graph)) == names(effect$vars)[v]]
      intervene <- vector(mode = "list")
      varconditions <- vector(mode = "list")
      varconditions[[names(effect$vars)[v]]] <- 1
      
      for(ll in 1:length(thisvar)){

          if(is.list(thisvar[[ll]])) {
            
            intervene[[names(thisvar)[ll]]][[names(thisvar[[ll]])]] <- as.numeric(thisvar[[ll]])
            
          } else {
            
            intervene[[names(effect$vars)[v]]][[names(thisvar)[ll]]] <- as.numeric(thisvar[[ll]])
            
          }
        
        }

      gee_r <- function(r, i, childcall = NULL) {

        parents <- adjacent_vertices(graph, obsvars[i], "in")[[1]]
        parents <- parents[!names(parents) %in% c("Ul", "Ur")]
        
        if(!is.null(childcall)){
          thisintervene <- intervene[[childcall]]
        }
        if(!is.null(childcall) && names(obsvars)[i] %in% names(thisintervene)) {

          as.numeric(thisintervene[[names(obsvars[i])]])

        } else if (length(parents) == 0){

          x <- respvars[[names(obsvars[[i]])]]$values[[which(respvars[[names(obsvars[[i]])]]$index == r[i])]]
          do.call(x, list())

        } else {

          lookin <- lapply(names(parents), function(gu) {

            as.numeric(gee_r(r, which(names(obsvars) == gu), childcall = names(obsvars[i])))

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
      
      var.dex <- rep(TRUE, nrow(res.mat))
      for(i in 1:length(varconditions)) {
        
        var.dex <- var.dex & res.mat[, names(varconditions)[i]] == varconditions[[i]]
        
      }
      var.eff[[v]] <- unique(as.character(q.vals.all.lookup[var.dex, "vars"]))
    }
    
    
    ## handle addition and subtraction based on operator
    ## accumulate final effect based on subtraction and addition
    
   
    
    objective <- list(var.eff[[1]])
    if(length(var.eff) > 1 & is.null(effect$oper) | (length(effect$oper) != length(var.eff) -1)){
      stop("Missing operator")
    }
    
    if(!is.null(effect$oper)) {
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
    
   
    if(!is.null(effect$oper) & length(red.sets$objective.terms) > 1) {
      
      for(opp in 1:length(effect$oper)) {
        
        thiscol <- ifelse(effect$oper[[opp]] == "-", " - ", " + ")
        objective.fin <- paste(objective.fin, effect$oper[[opp]], 
                               paste(red.sets$objective.terms[[opp + 1]], collapse = thiscol))
        
      }
      
    }
    
  
    
    attr(parameters, "key") <- parameters.key
    attr(parameters, "rightvars") <- names(right.vars)
    attr(parameters, "condvars") <- names(cond.vars)
    
    list(variables = red.sets$variables, parameters = parameters, constraints = c(red.sets$constr, p.constraints[special.terms]), 
         objective = objective.fin, p.vals = p.vals, q.vals = q.vals)
    
    
}

pastestar <- function(...) paste(..., sep = "*")

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
#' Like setdiff but doesn't remove duplicates
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
#' 
parse_effect <- function(text) {
  
  text <- gsub("(\\n|\\t| )", "", text)
  
  terms <- strsplit(text, split = "-|\\+")[[1]]
  opers <- as.list(grep("-|\\+", strsplit(text, "")[[1]], value = TRUE))
  
  pterms <- gsub("(", " = list(", terms, fixed = TRUE)
  parsedEffect <- eval(str2expression(paste("list(", paste(pterms, collapse = ","), ")")))
  
  list(vars = parsedEffect, oper = opers)
  
}


#' Plot the analyzed graph object
#' 
#' Special plotting method for igraphs of this type
#' 
#' @param graphres an igraph object
#' 
#' @export

plot_graphres <- function(graphres) {
  
  mylayout <- cbind(V(graphres)$x, V(graphres)$y)
  plot(graphres, vertex.color = ifelse(V(graphres)$latent == 1, "grey70",
                                       ifelse(V(graphres)$exposure == 1, "green", "white")), 
       vertex.shape = ifelse(V(graphres)$outcome == 1, "rectangle", "circle"),
       edge.color = ifelse(E(graphres)$edge.monotone == 1, "blue", "black"), 
       layout = mylayout, main = "Graph to be analyzed, inspect carefully")
  legend("topleft", legend = c("latent", "outcome", "exposure", "monotone edge"), pt.cex = c(3, 3, 3, 1), 
         pch = c(20, 22, 20, NA), col = c("grey70", "black", "green", "blue"), lty = c(NA, NA, NA, 1))
  
}