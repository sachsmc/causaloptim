
build_Pq <- function(red.sets, parameters) {
    
    
    see <- rep(0, length(red.sets$variables))
    see[match(red.sets$objective.terms[[1]], red.sets$variables)] <- 1
    see[match(red.sets$objective.terms[[2]], red.sets$variables)] <- -1
    
    see %c% red.sets$variables
    
    Pmat <- do.call(rbind, lapply(red.sets$raw.sets, function(x) {  ## equalities
        
        oot <- rep(0, length(red.sets$variables))
        oot[match(x, red.sets$variables)] <- 1
        oot
        
    }))
    
    pvect <- unlist(red.sets$pnames)
    
    xxx <- c(red.sets$variables, unlist(red.sets$pnames[-1]))
    
    Bmat <- do.call(rbind, lapply(pvect, function(x){
        
        oot <- rep(0, length(parameters))
        oot[match(x, parameters)] <- 1
        oot
        
    }))
    
    
    
    AAmat <- cbind(Pmat, -Bmat)
    AAmat <- rbind(AAmat, 
                   cbind(matrix(0, 2, ncol(Pmat)), rbind(c(rep(1, 4), rep(0, 4)), c(rep(0,4), rep(1, 4)))))
    AAmat %c% xxx
    
    redP <- gaussianElimination(AAmat, B = c(1, rep(0, nrow(AAmat) - 3), 1, 1))
    
    Pmat.red <- redP[, 1:length(red.sets$variables)]
    elim <- rowSums(abs(Pmat.red)) != 0
    Pmat.red <- Pmat.red[elim, ]
    
    BB <- redP[elim, -c(1:length(red.sets$variables), ncol(redP))]
    bee <- BB %c% pvect[-1]
    bee[1] <- paste(bee[1], " + 1")
    
    new.constraints <- paste0(Pmat.red %c% red.sets$variables, " = ", bee)
    newsets <- const.to.sets(new.constraints, red.sets$objective.terms[[1]], red.sets$objective.terms[[2]])
    
    
    Hrep <- makeH(a1 = t(-Pmat.red), b1 = see)
    Vrep <- scdd(Hrep)$output
    
    Vrep <- Vrep[, -c(1, 2)]
    Vrep <- Vrep[apply(Vrep, 1, function(x) all(x >= 0)), ]
    
    res1 <- Vrep %c% paste0("(", bee, ")")

    Hrep <- makeH(a1 = t(-Pmat.red), b1 = -see)
    Vrep <- scdd(Hrep)$output
    
    Vrep <- Vrep[, -c(1, 2)]
    Vrep <- Vrep[apply(Vrep, 1, function(x) all(x >= 0)), ]
    
    res2 <- Vrep %c% paste0("(", bee, ")")
    
    
    test <- c(paste0("\nMIN {\n", paste(res2, collapse = "\n"), "\n}\n\n"), 
              paste0("\nMAX {\n", paste(res1, collapse = "\n"), "\n}\n\n"))
    
}



`%c%` <- function(A, b) {
    
    if(is.null(dim(A))) {
        
        coeff <- abs(A[A != 0])
        collapse = ifelse(sign(A[A!=0]) == -1, " -", " +")
        if(collapse[1] == " + ") collapse[1] <- ""
        paste(collapse, ifelse(coeff == 1, "", paste0(coeff, "*")), b[A != 0], collapse = "")
        
    } else {
        
        if(dim(A)[2] != length(b)) stop("Incompatible matrices")
        res <- rep("", nrow(A))
        for(i in 1:nrow(A)) {
            
            A0 <- A[i, ]
            coeff <- abs(A0[A0 != 0])
            collapse <- ifelse(sign(A0[A0!=0]) == -1, " -", " +")
            if(collapse[1] == " +") collapse[1] <- ""
            res[i] <- paste(collapse, ifelse(coeff == 1, "", paste0(coeff, "*")), b[A0 != 0], collapse = "")
            
            
        }
        
        res
        
        
    }
    
}