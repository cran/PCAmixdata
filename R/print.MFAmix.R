print.MFAmix<-function (x, ...) 
{
  res.mfa <- x
  if (!inherits(res.mfa, "MFAmix")) 
    stop("non convenient data")
  cat("**Results of the Multiple Factor Analysis for mixed data (MFAmix)**\n")
  cat("The analysis was performed on", nrow(res.mfa$call$X), 
      "individuals, described by", ncol(res.mfa$call$X), "variables\n")
  cat("*Results are available in the following objects :\n\n")
  res <- array("", c(9, 2), list(1:9, c("name", "description")))
  res[1, ] <- c("$eig", "eigenvalues")
  res[2, ] <- c("$separate.analyses", "separate analyses for each group of variables")
  res[3, ] <- c("$group", "results for all the groups")
  res[4, ] <- c("$partial.axes", "results for the partial axes")
  res[5, ] <- c("$ind", "results for the individuals")
  res[6, ] <- c("$quanti.var", "results for the quantitatives variables")
  res[7, ] <- c("$quali.var", "results for the categorials variables")
  res[8, ] <- c("$global.pca", "results for the global PCA")
  res[9, ] <- c("$recap.eig.separate", "ndim first eigenvalues of the separate analyses")
  
  print(res)
  
}
