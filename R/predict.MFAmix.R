predict.MFAmix<-function (object, data, group,name.group,...) 
{
  mfa <- object
  if (!inherits(mfa, "MFAmix")) 
    stop("use only with \"MFAmix\" objects")
  
  n <- nrow(data)
  nbr.group <- length(unique(group))
  Lst.group <- Cut.Group(base = data, group = group, name.group = name.group)
  long.group <- sapply(Lst.group, ncol)
  typ.group <- unlist(sapply(Lst.group, Tri.Data)[3, ])
  DATA.ord <- data.frame(matrix(NA, ncol = ncol(data), nrow = nrow(data)))
  init <- 0
  for (g in 1:nbr.group) {
    DATA.ord[, c((1 + init):(init + ncol(Lst.group[[g]])))] <- Lst.group[[g]]
    init <- init + ncol(Lst.group[[g]])
  }
  colnames(DATA.ord) <- unlist(sapply(Lst.group, colnames))
  rownames(DATA.ord) <- rownames(data)
  group.ord <- NULL
  for (g in 1:nbr.group) {
    group.ord <- c(group.ord, rep(g, ncol(Lst.group[[g]])))
  }
  group <- group.ord
  data <- DATA.ord
  
  X.quanti<-Tri.Data(data)$X.quanti
  X.quali<-Tri.Data(data)$X.quali
  
  
  rec <- recod(X.quanti, X.quali)
  Y <- rec$Y
  n <- rec$n
  beta <- mfa$global.pca$coef
  if ((length(beta[[1]]) - 1) != ncol(Y)) 
    stop("The number of categories in the learning set is different than in X.quali")
  if (!is.null(X.quanti)) {
    label <- rownames(X.quanti)
    n1 <- nrow(X.quanti)
    p1 <- ncol(X.quanti)
    if (p1 != mfa$global.pca$rec$p1) 
      stop("The number of variables in X.quanti must be the same than in the learning set")
  }
  if (!is.null(X.quali)) {
    label <- rownames(X.quali)
    n2 <- nrow(X.quali)
    p2 <- ncol(X.quali)
    if (p2 != mfa$global.pca$rec$p2) 
      stop("The number of variables in X.quali must be the same than in the learning set")
  }
  if (!is.null(X.quanti) && !is.null(X.quali)) {
    if (n1 != n2) 
      stop("The number of objects in X.quanti and X.quali must be the same")
    if (sum(rownames(X.quali) != rownames(X.quanti)) != 0) 
      stop("The names of the objects in X.quanti and X.quali must be the same")
  }
  scores <- matrix(, n, length(beta))
  for (g in 1:length(beta)) scores[, g] <- Y %*% beta[[g]][-1] + 
    beta[[g]][1]
  colnames(scores) <- paste("dim", 1:length(beta), sep = "")
  rownames(scores) <- label
  return(scores)
}


