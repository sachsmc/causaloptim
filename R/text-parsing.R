
#' Paste with asterix sep
#' 
pastestar <- function(...) paste(..., sep = "*")


#' Expand potential outcome conditions
#' 
#' @param cond Text string of the condition
#' @param obsnames Vector of names of observed variables
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

#' Translate lists of contraints to lists of vectors
#' 
#' @param constr List of constraint terms as character strings
#' @param objterms Vector of terms in the objective function
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
#' 
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
#' @return A list
#' @export
parse_effect <- function(text) {
    
    text <- gsub("(\\n|\\t| )", "", text)
    
    terms0 <- strsplit(text, split = "-|\\+")[[1]]
    opers <- as.list(grep("-|\\+", strsplit(text, "")[[1]], value = TRUE))
    
    terms0 <- gsub("(p\\{)|(\\})", "", terms0)
    
    termssplit <- lapply(terms0, function(x) {
        
        strsplit(x, ";")[[1]]
        
    })
    
    res.effs <- res.vals <- vector(mode = "list", length= length(termssplit))
    
    for(j in 1:length(termssplit)) {
        terms0 <- termssplit[[j]]
        
        parse1 <- lapply(terms0, function(x) {
            val0 <- substr(x, nchar(x) - 1, nchar(x))
            rmain <- substr(x, 1, nchar(x) - 2)
            
            list(as.numeric(substr(val0, 2, 2)), rmain)
            
        })
        
        terms <- lapply(parse1, "[[", 2)
        vals <- lapply(parse1, "[[", 1)
        
        pterms <- gsub("(", " = list(", terms, fixed = TRUE)
        parsedEffect <-
            eval(str2expression(paste(
                "list(", paste(pterms, collapse = ","), ")"
            )))
        
        names(vals) <- names(parsedEffect)
        
        res.effs[[j]] <- parsedEffect
        res.vals[[j]] <- vals
        
    }
    
    list(vars = res.effs, oper = opers, values = res.vals)
    
}

#' Parse text that defines a the constraints
#' 
#' @param constraints A list of character strings
#' @return A data frame
#' @export
parse_constraints <- function(constraints) {
    
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
        
        if(rightout %in% c("0", "1")) {
            rightcond2 <- rightout
        } else {
            
            rightcond <- strsplit(gsub("\\)", "", pr1[-1]), ",")[[1]]
            rightcond2 <- expand_cond(rightcond, names(obsvars))
            
        }
        leftcond2 <- expand_cond(leftcond, names(obsvars))
        
        
        conds <- expand.grid(leftcond = leftcond2, rightcond = rightcond2, stringsAsFactors = FALSE)
        parsed.constraints <- rbind(parsed.constraints, 
                                    data.frame(leftout = leftout, rightout = rightout, operator, conds, stringsAsFactors = FALSE))
        
    }
    
    parsed.constraints
    
}

