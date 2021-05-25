# ----------------------------------------------------------
# Given a time, t, an alpha, alpha, and a type, type
# extract the appropriate data from mr -- main results,
# mse -- the main se results, sr -- the bootstrap results,
# and sse the bootstrap se results.
# ----------------------------------------------------------
selectOne <- function( t, mr, mse, sr, sse, type, alpha  ) {

    if ( type ==  1) Ctype1 <- "E"
    else Ctype1 <- "S"
    
    name  <- paste("T",t,sep="")
    
    MEst  <- mr [ mr [,"alpha"] == alpha & mr [,   "k"] ==      t & mr[,"type"] == type,  "Est"            ] 
    MSE   <- mse[ mse[,"alpha"] == alpha & mse[,"type"] == Ctype1,                        name             ]
    SEst  <- sr [ sr [,"alpha"] == alpha & sr [,"type"] ==   type & sr[,   "k"] == t,     c("Est","sub") ]
    SSE   <- sse[ sse[,"alpha"] == alpha & sse[,"type"] == Ctype1,                        c(name,"sub")    ]
    
    colnames(SEst)  <- c("Est","sub")
    SEst            <- merge( SEst, SSE, by = "sub" )
    row.names(SEst) <- NULL
    row.names(SSE)  <- NULL
    return(list( MEst=MEst, MSE = MSE, SEst = SEst ))
}
