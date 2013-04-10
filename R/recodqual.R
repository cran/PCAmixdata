#' @export
#' @title Recoding of the qualitative data matrix
#' @name recodqual
#' @param X the qualitative data matrix
#' @return \item{G}{The indicator matix of X with missing values replaced by 0}
#' @examples
#' data(vnf) 
#' X <- vnf[1:10,9:12]
#' tab.disjonctif.NA(X)
#' recodqual(X)


recodqual <-
function(X)
	{
		X <- as.matrix(X)
		GNA <- tab.disjonctif.NA(X)
		G <- replace(GNA,is.na(GNA),0)
		ns <- apply(G,2,sum)
		nmiss <- apply((apply(GNA,2,is.na)),2,sum)
		n <- nrow(X)
		if(sum((n-nmiss)==ns)!=0) stop("There are columns in X.quali where all the categories are identical")
		return(G)	
	}

