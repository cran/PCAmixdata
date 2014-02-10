#' @export
#' @name PCAmix
#' @title Principal Component Analysis for a mixture of qualitative and quantitative variables
#' @description PCAmix is a principal component method for a mixture of qualitative and quantitative variables.
#' PCAmix includes the ordinary principal component analysis (PCA) and multiple correspondence
#' analysis (MCA) as special cases. Squared loadings are correlation ratios for qualitative variables 
#' and squared correlation for quantitative variables. Missing values are replaced by means for quantitative
#' variables and by zeros in the indicator matrix for qualitative variables. Note that when all 
#' the p variables are qualitative, the scores of the n observations are equal to the usual scores of MCA 
#' times square root of p and the eigenvalues are then equal to the usual eigenvalues of MCA times p.
#' @param X.quanti a numeric matrix of data, or an object that can be coerced to such a matrix (such as a numeric vector or a data frame with all numeric columns).
#' @param X.quali a categorical matrix of data, or an object that can be coerced to such a matrix (such as a character vector, a factor or a data frame with all factor columns).
#' @param ndim number of dimensions kept in the results (by default 5).
#' @param graph boolean, if TRUE the following graphs are displayed for the first two dimensions of PCAmix: plot of the observations (the scores),
#' plot of the variables (squared loadings) plot of the correlation circle (if quantitative variables are available),
#' plot of the categories (if qualitative variables are available).
#' @param weighth.col a vector of weights for the quantitatives variables and the indicator of qualitatives variables
#' @param weighth.row a vector of weights for the individuals
#' @return \item{eig}{eigenvalues (i.e. variances) of the Principal Components (PC).}
#' @return \item{scores}{scores a n by ndim numerical matrix which contains the scores of the n observations on 
#' the ndim first Principal Components (PC).}
#' @return \item{scores.stand}{a n by ndim numerical matrix which contains the standardized scores of the n 
#' observations on the ndim first Principal Components (PC).}
#' @return \item{sload}{a p by ndim numerical matrix which contains the squared loadings of the p 
#' variables on the ndim first PC. For quantitative variables (resp. qualitative), 
#' squared loadings are the squared correlations (resp. the correlation ratios) with the PC scores.}
#' @return \item{categ.coord}{ 'NULL'  if X.quali is  'NULL' . Otherwise a m by ndim numerical matrix which 
#' contains the coordinates of the m categories of the qualitative variables on the 
#' ndim first PC. The coordinates of the categories are the averages of the standardized 
#' PC scores of the observations in those categories.}
#' @return \item{quanti.cor}{ 'NULL' if X.quanti is 'NULL'. Otherwise a p1 by ndim numerical matrix 
#' which contains the coordinates (the loadings) of the p1 quantitative variables 
#' on the ndim first PC. The coordinates of the quantitative variables are correlations with the PC scores.}
#' @return \item{quali.eta2}{ 'NULL' if X.quali is  'NULL' . Otherwise a p2 by ndim numerical matrix which 
#' contains the squared loadings of the p2 qualitative variables on the ndim first 
#' PC. The squared loadings of the qualitative variables are correlation ratios with 
#' the PC scores.}
#' @return \item{res.ind}{Results for the individuals (coord,contrib in percentage,cos2)} 
#' @return \item{res.quanti}{Results for the quantitatives variables (coord,contrib in percentage,cos2)} 
#' @return \item{res.ind}{Results for the categories of the categorials variables (coord,contrib in percentage,cos2)} 
#' @return \item{coef}{Coefficients of the linear combinations of the quantitative variables and the categories 
#' for constructing the principal components of PCAmix.}
#' @return \item{V}{The standardized loadings.}
#' @return \item{rec}{Results of the fonction recod(X.quanti,X.quali).}
#' @return \item{M}{Metric used in the svd for the weights of the variables.}
#' @author Marie Chavent \email{marie.chavent@@math.u-bordeaux1.fr}, Vanessa Kuentz, Amaury Labenne, Benoit Liquet, Jerome Saracco
#' @references {Chavent, M., Kuentz, V., Saracco, J. (2011), Orthogonal Rotation in PCAMIX. Advances in Classification and Data Analysis, Vol. 6, pp. 131-146.
#' 
#' Kiers, H.A.L., (1991), Simple structure in Component Analysis Techniques for mixtures of qualitative and quantitative variables, Psychometrika, 56, 197-212.}
#' @examples
#'#PCAMIX:
#'data(wine)
#'X.quanti <- wine[,c(3:29)] 
#'X.quali <- wine[,c(1,2)] 
#'pca<-PCAmix(X.quanti,X.quali,ndim=4)
#'pca<-PCAmix(X.quanti,X.quali,ndim=4,graph=FALSE)
#'pca$eig
#'
#'#Scores on dim 1-2
#'plot(pca,choice="ind",quali=wine[,1],
#'     posleg="bottomleft",main="Scores") 
#'#Scores on dim 2-3	
#'plot(pca,choice="ind",axes=c(2,3),quali=wine[,1],
#'     posleg="bottomleft",main="Scores")
#'#Other graphics 
#'plot(pca,choice="var",main="Squared loadings")
#'plot(pca,choice="categ",main="Categories")
#'plot(pca,choice="cor",xlim=c(-1.5,2.5),
#'     main="Correlation circle")


#'#plot with standardized scores:
#'plot(pca,choice="ind",quali=wine[,1],stand=TRUE,
#'     posleg="bottomleft",main="Standardized Scores")
#'plot(pca,choice="var",stand=TRUE,main="Squared loadings")
#'plot(pca,choice="categ",stand=TRUE,main="Categories")
#'plot(pca,choice="cor",stand=TRUE,main="Correlation circle")
#'
#'
#'#PCA:
#'data(decathlon)
#'quali<-decathlon[,13]
#'pca<-PCAmix(decathlon[,1:10])
#'pca<-PCAmix(decathlon[,1:10], graph=FALSE)
#'plot(pca,choice="ind",quali=quali,cex=0.8,
#'     posleg="topright",main="Scores")
#'plot(pca, choice="var",main="Squared correlations")
#'plot(pca, choice="cor",main="Correlation circle")
#'
#'
#'#MCA
#'data(flower)
#'mca <- PCAmix(X.quali=flower[,1:4])
#'mca <- PCAmix(X.quali=flower[,1:4],graph=FALSE)
#'plot(mca,choice="ind",main="Scores")
#'plot(mca,choice="var",main="Correlation ratios")
#'plot(mca,choice="categ",main="Categories")
#'
#'#Missing values
#'data(vnf) 
#'PCAmix(X.quali=vnf)
#'vnf2<-na.omit(vnf)
#'PCAmix(X.quali=vnf2)
#'
#'@keywords multivariate


PCAmix<- function (X.quanti=NULL,X.quali=NULL,ndim=5,weight.col=NULL,weight.row=NULL,graph=TRUE)
{
  cl <- match.call()
  rec <- recod(X.quanti, X.quali)
  n <- rec$n
  p <- rec$p
  p1 <- rec$p1
  p2 <- p - p1
  X <- rec$X
  G <- rec$G
  W <- rec$W
  m <- ncol(W) - p1
  q <- qr(W)$rank
  indexj <- rec$indexj
  if (!is.null(X.quali)) {
    ns <- apply(G, 2, sum)
    ps <- ns/nrow(G)
  }
  else {
    ns <- NULL
    ps <- NULL
  }
  M1 <- c(rep(1, p1))
  M2 <- ps
  M2.inv <- 1/M2
  N <- rep(1/n, n)
  if (!is.null(weight.col) == TRUE) {
    weight.col.quant <- weight.col[1:p1]
    weight.col.qual <- weight.col[(p1 + 1):(p1 + m)]
    M1 <- M1 * weight.col.quant
    M2.inv <- M2.inv * weight.col.qual
  }
  if (!is.null(weight.row) == TRUE) {
    N <- (N * weight.row)/sum(weight.row)
  }
  Met.global <- c(M1, M2.inv)
  names(Met.global) <- colnames(W)
  e <- svd.triplet(W, N, Met.global)
  eig <- matrix(0, q, 3)
  colnames(eig) <- c("Eigenvalue", "Proportion", "Cumulative")
  rownames(eig) <- paste("dim", 1:q, sep = " ")
  eig[, 1] <- e$vs[1:q]^2
  eig[, 2] <- 100 * eig[, 1]/sum(eig[, 1], na.rm = T)
  eig[1, 3] <- eig[1, 2]
  if (q > 1) {
    for (j in 2:q) eig[j, 3] <- eig[j, 2] + eig[j - 1, 3]
  }
  if (ndim <= 1) 
    stop("'ndim' must be an interger greater or equal to 2")
  ndim <- min(ndim, q)
  U <- data.frame(e$U[, 1:ndim])
  rownames(U) <- rownames(W)
  colnames(U) <- rownames(eig)[1:dim(U)[2]]
  d <- e$vs[1:ndim]
  V.total.dim <- data.frame(e$V)
  U.total.dim <- data.frame(e$U)
  d.total.dim <- e$vs
  if (q != 1) {
    F <- as.matrix(U) %*% diag(d)
    F.total.dim <- as.matrix(U.total.dim) %*% diag(d.total.dim)
    colnames(F) <- rownames(eig)[1:dim(F)[2]]
  }
  else {
    F <- data.frame(U * d)
    F.total.dim <- data.frame(U.total.dim * d.total.dim)
    colnames(F) <- rownames(eig)[1:dim(F)[2]]
  }
  V <- data.frame(e$V[, 1:ndim])
  A <- NULL
  A1 <- NULL
  A2 <- NULL
  C <- NULL
  if (p1 > 0 & p2 == 0) {
    V1 <- as.matrix(V[1:p1, ])
    V1.total.dim <- as.matrix(V.total.dim[1:p1, ])
    if (p1 > 1) {
      A1 <- V1 %*% diag(d)
      A1.total.dim <- V1.total.dim %*% diag(d.total.dim)
    }
    else {
      A1 <- data.frame(V1 * d)
      A1.total.dim <- data.frame(V1.total.dim * d.total.dim)
    }
    colnames(A1) <- paste("dim", 1:ndim, sep = "")
    rownames(A1) <- colnames(W)[1:p1]
    contrib.quanti <- sweep(A1^2, 1, STATS = M1, FUN = "*")
    contrib.quanti <- sweep(contrib.quanti, 2, STATS = d^2, 
                            FUN = "/")
    colnames(contrib.quanti) <- paste("dim", 1:ndim, sep = "")
    contrib.moda <- NULL
    A <- A1
    rownames(A) <- colnames(W)
    cos2.quanti <- sweep(A1^2, 1, STATS = apply(A1.total.dim, 
                                                1, function(v) {
                                                  return(sum(v^2))
                                                }), FUN = "/")
    contrib.moda <- NULL
    cos2.moda <- NULL
    rownames(A) <- colnames(W)
    rownames(contrib.quanti) <- rownames(cos2.quanti) <- colnames(W)
    colnames(contrib.quanti) <- colnames(cos2.quanti) <- colnames(A)
  }
  if (p1 == 0 & p2 > 0) {
    V2 <- as.matrix(V[(p1 + 1):(p1 + m), ])
    V2.total.dim <- as.matrix(V.total.dim[(p1 + 1):(p1 + 
                                                      m), ])
    if (p2 > 1) {
      A2 <- diag(M2.inv) %*% V2 %*% diag(d)
      A2.total.dim <- diag(M2.inv) %*% V2.total.dim %*% 
        diag(d.total.dim)
    }
    else {
      A2 <- data.frame(diag(M2.inv) %*% V2 * d)
      A2.total.dim <- data.frame(diag(M2.inv) %*% V2.total.dim * 
                                   d.total.dim)
    }
    if (!is.null(weight.col)) {
      A2 <- sweep(A2, 1, STATS = weight.col.qual, FUN = "/")
      A2.total.dim <- sweep(A2.total.dim, 1, STATS = weight.col.qual, 
                            FUN = "/")
    }
    colnames(A2) <- paste("dim", 1:ndim, sep = "")
    rownames(A2) <- colnames(W)[(p1 + 1):(p1 + m)]
    contrib.moda <- sweep(A2^2, 1, STATS = ps, FUN = "*")
    contrib.moda <- sweep(contrib.moda, 2, STATS = d^2, FUN = "/")
    if (!is.null(weight.col)) {
      contrib.moda <- sweep(contrib.moda, 1, STATS = weight.col.qual, 
                            FUN = "*")
    }
    colnames(contrib.moda) <- paste("dim", 1:ndim, sep = "")
    C <- matrix(NA, p2, length(d))
    rownames(C) <- colnames(X.quali)
    colnames(C) <- paste("dim", 1:ndim, sep = "")
    for (j in 1:(p - p1)) {
      C[j, ] <- apply(as.matrix(contrib.moda[which(indexj == 
                                                     j), ]), 2, FUN = sum)
    }
    C <- sweep(C, 2, STATS = d^2, FUN = "*")
    contrib.quanti <- NULL
    A <- diag(sqrt(ps)) %*% as.matrix(A2)
    cos2.moda <- sweep(A2^2, 1, STATS = apply(A2.total.dim, 
                                              1, function(v) {
                                                return(sum(v^2))
                                              }), FUN = "/")
    contrib.quanti <- NULL
    cos2.quanti <- NULL
    rownames(A) <- colnames(W)
    rownames(contrib.moda) <- colnames(W)
    colnames(contrib.moda) <- colnames(A)
  }
  if (p1 > 0 & p2 > 0) {
    V1 <- as.matrix(V[1:p1, ])
    V1.total.dim <- as.matrix(V.total.dim[1:p1, ])
    V2 <- as.matrix(V[(p1 + 1):(p1 + m), ])
    V2.total.dim <- as.matrix(V.total.dim[(p1 + 1):(p1 + 
                                                      m), ])
    A1 <- (V1) %*% diag(d)
    A1.total.dim <- (V1.total.dim) %*% diag(d.total.dim)
    colnames(A1) <- paste("dim", 1:ndim, sep = "")
    rownames(A1) <- colnames(W)[1:p1]
    A2 <- diag(M2.inv) %*% V2 %*% diag(d)
    A2.total.dim <- diag(M2.inv) %*% V2.total.dim %*% diag(d.total.dim)
    if (!is.null(weight.col)) {
      A2 <- sweep(A2, 1, STATS = weight.col.qual, FUN = "/")
      A2.total.dim <- sweep(A2.total.dim, 1, STATS = weight.col.qual, 
                            FUN = "/")
    }
    colnames(A2) <- paste("dim", 1:ndim, sep = "")
    rownames(A2) <- colnames(W)[(p1 + 1):(p1 + m)]
    contrib.quanti <- sweep(A1^2, 1, STATS = M1, FUN = "*")
    contrib.quanti <- sweep(contrib.quanti, 2, STATS = d^2, 
                            FUN = "/")
    colnames(contrib.quanti) <- paste("dim", 1:ndim, sep = "")
    contrib.moda <- sweep(A2^2, 1, STATS = ps, FUN = "*")
    contrib.moda <- sweep(contrib.moda, 2, STATS = d^2, FUN = "/")
    if (!is.null(weight.col)) {
      contrib.moda <- sweep(contrib.moda, 1, STATS = weight.col.qual, 
                            FUN = "*")
    }
    colnames(contrib.moda) <- paste("dim", 1:ndim, sep = "")
    C <- matrix(NA, p2, length(d))
    rownames(C) <- colnames(X.quali)
    colnames(C) <- paste("dim", 1:ndim, sep = "")
    for (j in 1:(p - p1)) {
      C[j, ] <- apply(contrib.moda[which(indexj == (j + 
                                                      p1)) - p1, ], 2, FUN = sum)
    }
    C <- sweep(C, 2, STATS = d^2, FUN = "*")
    cos2.moda <- sweep(A2^2, 1, STATS = apply(A2.total.dim, 
                                              1, function(v) {
                                                return(sum(v^2))
                                              }), FUN = "/")
    A <- rbind(A1, diag(sqrt(ps)) %*% A2)
    cos2.quanti <- sweep(A1^2, 1, STATS = apply(A1.total.dim, 
                                                1, function(v) {
                                                  return(sum(v^2))
                                                }), FUN = "/")
    rownames(A) <- colnames(W)
    rownames(contrib.quanti) <- rownames(cos2.quanti) <- colnames(W)[1:p1]
    rownames(contrib.moda) <- colnames(W)[(p1 + 1):(p1 + 
                                                      m)]
    colnames(contrib.moda) <- colnames(contrib.quanti) <- colnames(cos2.quanti) <- colnames(A)
  }
  if (p2 > 0) {
    contrib.quali <- matrix(NA, p2, length(d))
    for (j in 1:(p - p1)) {
      contrib.quali[j, ] <- apply(as.matrix(contrib.moda[which(indexj == 
                                                                 (j + p1)) - p1, ]), 2, FUN = sum)
    }
    colnames(contrib.quali) <- colnames(C)
    rownames(contrib.quali) <- rownames(C)
  }
  else {
    contrib.quali <- NULL
  }
  contrib.ind <- sweep((1/n) * F^2, 2, STATS = d^2, FUN = "/")
  cos2.ind <- sweep(F^2, 1, STATS = apply(F.total.dim, 1, function(v) {
    return(sum(v^2))
  }), FUN = "/")
  rownames(contrib.ind) <- rownames(cos2.ind) <- rownames(W)
  colnames(contrib.ind) <- colnames(cos2.ind) <- colnames(A)
  result.ind <- list(coord = F, contrib = 100 * contrib.ind, 
                     cos2 = cos2.ind)
  result.quanti <- list(coord = A1, contrib = 100 * contrib.quanti, 
                        cos2 = cos2.quanti)
  result.categ <- list(coord = A2, contrib = 100 * contrib.moda, 
                       cos2 = cos2.moda, contrib.quali = 100 * contrib.quali)
  rownames(V) <- rownames(A)
  colnames(V) <- colnames(A)
  sload <- rbind(A1^2, C)
  names(d) <- colnames(U) <- colnames(F) <- colnames(sload) <- colnames(cos2.ind) <- colnames(contrib.ind) <- paste("dim", 
                                                                                                                    1:ndim, sep = "")
  if (!is.null(weight.col)) {
  V.star <- weight.col*sqrt(c(rep(1,p1),1/M2)) * V
  }
  else{
  V.star <-sqrt(c(rep(1,p1),1/M2)) * V  
  }
  
  coef <- structure(vector(mode = "list", length = ndim), names = paste("dim", 
                                                                        1:ndim, sep = ""))
  gc <- rec$g
  sdev <- rec$s
  for (g in 1:ndim) {
    beta <- matrix(NA, p1 + m + 1, 1)
    beta[1, 1] <- -sum(V.star[, g] * gc/sdev)
    beta[2:(p1 + m + 1), 1] <- V.star[, g]/sdev
    rownames(beta) <- c("const", colnames(W))
    coef[[g]] <- beta
  }
  Z <- rec$Z
  res <- list(call = cl, eig = eig, scores.stand = as.matrix(U), 
              scores = F, V = as.matrix(V), sload = sload, A = as.matrix(A), 
              categ.coord = A2, quanti.cor = A1, quali.eta2 = C, rec = rec, 
              ndim = ndim, W = W, res.ind = result.ind, res.quanti = result.quanti, 
              res.categ = result.categ, coef = coef, Z = Z, M = Met.global)
  class(res) <- "PCAmix"
  if (graph) {
    plot.PCAmix(res, main = "Scores")
    if (p1 != p) {
      dev.new()
      plot.PCAmix(res, choice = "categ", main = "Categories")
    }
    if (p1 != 0) {
      dev.new()
      plot.PCAmix(res, choice = "cor", main = "Correlation circle")
    }
    dev.new()
    plot.PCAmix(res, choice = "var", main = "Squared loadings")
  }
  return(res)
}