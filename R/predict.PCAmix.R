#' @S3method predict PCAmix
#' @export
#' @name predict.PCAmix
#' @title Scores of new objects on the principal components of PCAmix or PCArot
#' @description This function calculates the scores of a new set of data on the 
#' principal components of PCA. If the components have been rotated, the function gives
#' the scores of the new objects on the rotated PC. The new objects must have the same 
#' variables than the learning set.
#' @param object  an object of class PCAmix (output of the function PCAmix or PCArot)
#' @param X.quanti  numeric matrix of data for the new objects
#' @param X.quali  a categorical matrix of data for the new objects
#' @param ... futher arguments pased to or from other methods
#' @return Returns the matrix of the scores of the new objects of the ndim principal PC or rotated PC.
#' @author Marie Chavent \email{marie.chavent@@math.u-bordeaux1.fr}, Vanessa Kuentz, Benoit Liquet, Jerome Saracco
#' @examples  
#'data(decathlon)
#'n <- nrow(decathlon)
#'sub <- sample(1:n,20)
#'pca<-PCAmix(decathlon[sub,1:10], graph=FALSE)
#'predict(pca,decathlon[-sub,1:10])
#'rot <- PCArot(pca,dim=4)
#'predict(rot,decathlon[-sub,1:10])
#' 
predict.PCAmix <- function(object,X.quanti=NULL,X.quali=NULL,...)
{
  pca<-object
  if (!inherits(pca, "PCAmix")) 
    stop("use only with \"PCAmix\" objects")
  
  rec <- recod(X.quanti,X.quali)
  Y <- rec$Y
  n <- rec$n
  beta <- pca$coef
  if ((length(beta[[1]])-1)!=ncol(Y))
    stop("The number of categories in the learning set is different than in X.quali")
  if (!is.null(X.quanti)) 
    {
    label <- rownames(X.quanti)
    n1 <- nrow(X.quanti)
    p1 <- ncol(X.quanti)
    if (p1 != pca$rec$p1) stop("The number of variables in X.quanti must be the same than in the learning set")
  }
  if (!is.null(X.quali))
    {
    label <- rownames(X.quali)
    n2 <- nrow(X.quali)
    p2 <- ncol(X.quali)
    if (p2 != pca$rec$p2) stop("The number of variables in X.quali must be the same than in the learning set")
  }
  if (!is.null(X.quanti)&& !is.null(X.quali))
    {
    if (n1 != n2) stop("The number of objects in X.quanti and X.quali must be the same")
    if (sum(rownames(X.quali)!=rownames(X.quanti))!=0) stop("The names of the objects in X.quanti and X.quali must be the same")
  }
  
  scores <- matrix(,n,length(beta))
  for (g in 1: length(beta)) scores[,g] <-Y %*% beta[[g]][-1] +  beta[[g]][1]
  
  if (colnames(pca$sload)[1]=="dim1.rot")  
    colnames(scores) <- paste("dim", 1:length(beta), sep = "",".rot")
  else
    colnames(scores) <- paste("dim", 1:length(beta), sep = "")
  rownames(scores) <- label
  return(scores)			
}
