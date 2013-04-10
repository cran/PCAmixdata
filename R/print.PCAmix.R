print.PCAmix <- function(x, ...){
  if (!inherits(x, "PCAmix")) 
    stop("use only with \"PCAmix\" objects")
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  p1 <- x$rec$p1
  p <- x$rec$p
  if (colnames(x$sload)[1]=="dim1.rot")  cat("Method = rotation after ") else  cat("Method = ")
  if (p1==p) cat("Principal Component Analysis (PCA)") 
  if (p1==0) cat("Multiple Correspondence Analysis (MCA)") 
  if ((p1!=0) && (p1!=p)) cat("Principal Component of mixed data (PCAmix)") 
  cat("\n")
  if (colnames(x$sload)[1]=="dim1.rot")  cat("number of iterations: ",x$iter,sep=" ") 
  cat("\n")
  cat("\n")
  
  if (colnames(x$sload)[1]=="dim1.rot") {
    res <- matrix("",11,2)
    colnames(res) <-c("name","description")
    res[1,] <- c("$theta", "angle of rotation if 'dim'=2")
    res[2,] <- c("$T", "matrix of rotation if 'dim'=2")
    res[3,] <- c("$eig", "variances of the 'ndim' first dimensions after rotation")
    res[4,] <- c("$scores", "PC scores after rotation")
    res[5,] <- c("$scores.stand", "standardized PC scores after rotation")
    res[6,] <- c("$sload", "squared loadings after rotation")
    res[7,] <- c("$categ.coord", "categories coordinates after rotation")
    res[8,] <- c("$quanti.cor", "cor between quantitative variables and rotated PC scores")
    res[9,] <- c("$quali.eta2", "eta2 between qualitative variables and rotated PC scores")
    res[10,] <- c("$coef", "coef of the linear combinations defining the rotated PC")
    res[11,] <- c("$V", "The rotated standardized loadings")
  } else {
    res <- matrix("",12,2)
    colnames(res) <-c("name","description")
    res[1,] <- c("$eig", "eigenvalues of the principal components (PC) ")
    res[2,] <- c("$scores", "principal component(PC) scores")
    res[3,] <- c("$scores.stand", "standardized principal component(PC) scores")
    res[4,] <- c("$sload", "squared loadings")
    res[5,] <- c("$categ.coord", "categories coordinates")
    res[6,] <- c("$quanti.cor", "cor between quantitative variables and PC scores")
    res[7,] <- c("$quali.eta2", "eta2 between qualitative variables and PC scores")
    res[8,] <- c("$coef", "coef of the linear combinations defining the PC")
    res[9,] <- c("$V", "The standardized loadings")
    res[10,] <- c("$res.ind", "Results for the individuals (coord,contrib,cos2)")
    res[11,] <- c("$res.quanti", "Results for the quantitatives variables (coord,contrib,cos2)")
    res[12,] <- c("$res.categ", "Results for the categories of the categorials variables (coord,contrib,cos2)")
    
  }
  print(res)
}