#' @export
#' @keywords internal
print.PCAmix <- function(x, ...){
  if (!inherits(x, "PCAmix")) 
    stop("use only with \"PCAmix\" objects")
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  p1 <- x$rec$p1
  p <- x$rec$p
  if (colnames(x$sqload)[1]=="dim1.rot")  cat("Method = rotation after ") else  cat("Method = ")
  if (p1==p) cat("Principal Component Analysis (PCA)") 
  if (p1==0) cat("Multiple Correspondence Analysis (MCA)") 
  if ((p1!=0) && (p1!=p)) cat("Principal Component of mixed data (PCAmix)") 
  cat("\n")
  if (colnames(x$sqload)[1]=="dim1.rot")  cat("number of iterations: ",x$iter,sep=" ") 
  cat("\n")
  cat("\n")
  
  if (colnames(x$sqload)[1]=="dim1.rot") {
    res <- matrix("",9,2)
    colnames(res) <-c("name","description")
    res[1,] <- c("$eig", "variances of the 'ndim' first dimensions after rotation")
    res[2,] <- c("$ind", "results for the individuals after rotation (coord)")
    res[3,] <- c("$quanti", "results for the quantitative variables (coord) after rotation")
    res[4,] <- c("$levels", "results for the levels of the qualitative variables (coord) after rotation")
    res[5,] <- c("$quali", "results for the qualitative variables (coord) after rotation ")  
    res[6,] <- c("$sqload", "squared loadings after rotation")    
    res[7,] <- c("$coef", "coef of the linear combinations defining the rotated PC")
    res[8,] <- c("$theta", "angle of rotation if 'dim'=2")
    res[9,] <- c("$T", "matrix of rotation")
  } else {
    res <- matrix("",7,2)
    colnames(res) <-c("name","description")
    res[1,] <- c("$eig", "eigenvalues of the principal components (PC) ")
    res[2,] <- c("$ind", "results for the individuals (coord,contrib,cos2)")
    res[3,] <- c("$quanti", "results for the quantitative variables (coord,contrib,cos2)")
    res[4,] <- c("$levels", "results for the levels of the qualitative variables (coord,contrib,cos2)")
    res[5,] <- c("$quali", "results for the qualitative variables (contrib,relative contrib)")
    res[6,] <- c("$sqload", "squared loadings")
    res[7,] <- c("$coef", "coef of the linear combinations defining the PC")
  }
  if (!(is.null(x$sqload.sup)))
  {
    sup <- matrix("",3,2)
    sup[1,] <- c("$quanti.sup", "results for the supplementary quantitative variables")
    sup[2,] <- c("$levels.sup", "results for the levels of the qualitative variables")
    sup[3,] <- c("$sqload.sup", "squared loadings of the supplementary variables")
    res <- rbind(res,sup)
  }
  utils::write.table(res,row.names = FALSE)
}