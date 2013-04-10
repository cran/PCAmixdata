#' @export
#' @title Construction of the indicator matrix of a qualitative data matrix
#' @name tab.disjonctif.NA
#' @param tab a qualitative data matrix
#' @return res the indicator matrix with NA for the categories of the variable which 
#' is missing in tab
#' @examples
#' data(vnf) 
#' X <- vnf[1:10,9:12]
#' tab.disjonctif.NA(X)
#' recodqual(X)

tab.disjonctif.NA <-
function (tab) {
    tab <- as.data.frame(tab)
    modalite.disjonctif <- function(i) {
        moda <- tab[, i]
        nom <- names(tab)[i]
        n <- length(moda)
        moda <- as.factor(moda)
        x <- matrix(0, n, length(levels(moda)))
	  ind<-(1:n) + n * (unclass(moda) - 1)
	  indNA<-which(is.na(ind))
		
        x[(1:n) + n * (unclass(moda) - 1)] <- 1
        x[indNA,]<-NA 
            dimnames(x) <- list(row.names(tab), paste(nom, levels(moda), 
                sep = "="))
      
        return(x)
    }
    if (ncol(tab) == 1) 
        res <- modalite.disjonctif(1)
    else {
        res <- lapply(1:ncol(tab), modalite.disjonctif)
        res <- as.matrix(data.frame(res, check.names = FALSE))
    }
    return(res)
}

