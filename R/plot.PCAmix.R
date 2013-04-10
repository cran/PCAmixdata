plot.PCAmix <- function(x,axes = c(1, 2), choice = "ind", stand=FALSE,label=TRUE, quali=NULL,posleg="topleft",xlim=NULL,ylim=NULL,cex=1,col.var=NULL,...)
{
  if (!inherits(x, "PCAmix")) 
    stop("use only with \"PCAmix\" objects")
  if (max(axes) > x$ndim) 
    stop(paste("axes must be between 1 and ", x$ndim, sep = ""))
  if (!(choice %in% c("ind", "var", "categ", "cor"))) 
    stop("the argument 'choice' must be 'ind','var','cor' or 'categ'")
  dim1 <- axes[1]
  dim2 <- axes[2]
  if (stand) {
    lab.x <- paste("Dimension ", dim1, sep = "")
    lab.y <- paste("Dimension ", dim2, sep = "")
    scores <- x$scores.stand
  }
  else {
    px <- round(x$eig[dim1, 2], digits = 2)
    py <- round(x$eig[dim2, 2], digits = 2)
    lab.x <- paste("Dim ", dim1, " (", px, "%)", sep = "")
    lab.y <- paste("Dim ", dim2, " (", py, "%)", sep = "")
    scores <- x$scores
  }
  p1 <- x$rec$p1
  p <- x$rec$p
  quanti.coord <- x$quanti.cor
  if (choice == "ind") {
    if (is.null(xlim)) {
      xmin <- min(scores[, dim1])
      xmax <- max(scores[, dim1])
      xlim <- c(xmin, xmax) * 1.2
    }
    if (is.null(ylim)) {
      ymin <- min(scores[, dim2])
      ymax <- max(scores[, dim2])
      ylim <- c(ymin, ymax) * 1.2
    }
    if (is.null(quali)) {
      plot(scores[, axes], xlim = xlim, ylim = ylim, xlab = lab.x, 
           ylab = lab.y, pch = 20, cex = cex, ...)
      abline(h = 0, lty = 2, cex = cex)
      abline(v = 0, lty = 2, cex = cex)
      if (label) 
        text(scores[, axes], labels = rownames(scores), 
             pos = 3, cex = cex, ...)
    }
    else {
      if (is.numeric(quali)) 
        stop("quali must be categorical")
      if (length(quali) != nrow(x$scores)) 
        stop("the length of quali is inapproriate")
      quali <- as.factor(quali)
      plot(scores[, axes], xlim = xlim, ylim = ylim, xlab = lab.x, 
           ylab = lab.y, pch = 20, col = as.numeric(quali), 
           cex = cex, ...)
      abline(h = 0, lty = 2, cex = cex)
      abline(v = 0, lty = 2, cex = cex)
      legend(posleg, legend = levels(quali), text.col = c(1:length(levels(quali))), 
             cex = 0.8 * cex)
      if (label) 
        text(scores[, axes], labels = rownames(x$scores), 
             pos = 3, col = as.numeric(quali), cex = cex, 
             ...)
    }
  }
  if (choice == "var") {
    if (is.null(xlim)) {
      xmax <- max(x$sload[, dim1])
      xlim <- c(-0.1, xmax * 1.2)
    }
    if (is.null(ylim)) {
      ymax <- max(x$sload[, dim2])
      ylim <- c(-0.1, ymax * 1.2)
    }
    plot(0, 0, type = "n", xlab = lab.x, ylab = lab.y, xlim = xlim, 
         ylim = ylim, cex = cex, ...)
    abline(v = 0, lty = 2, cex = cex)
    abline(h = 0, lty = 2, cex = cex)
    for (j in 1:nrow(x$sload)) {
      arrows(0, 0, x$sload[j, dim1], x$sload[j, dim2], 
             length = 0.1, angle = 15, code = 2, cex = cex, 
             ...)
      if (label) {
        if (x$sload[j, dim1] > x$sload[j, dim2]) {
          pos <- 4
        }
        else pos <- 3
        text(x$sload[j, dim1], x$sload[j, dim2], labels = rownames(x$sload)[j], 
             pos = pos, cex = cex, ...)
      }
    }
  }
  if (choice == "categ") {
    if (p1 == p) 
      stop("Argument 'categ' is inappropriate for only quantitative data")
    if (is.null(xlim)) {
      xmin <- min(x$categ.coord[, dim1])
      xmax <- max(x$categ.coord[, dim1])
      xlim <- c(xmin, xmax) * 1.2
    }
    if (is.null(ylim)) {
      ymin <- min(x$categ.coord[, dim2])
      ymax <- max(x$categ.coord[, dim2])
      ylim <- c(ymin, ymax) * 1.2
    }
    plot(x$categ.coord[, axes], xlim = xlim, ylim = ylim, 
         xlab = lab.x, ylab = lab.y, pch = 17, cex = cex, 
         ...)
    abline(h = 0, lty = 2, cex = cex)
    abline(v = 0, lty = 2, cex = cex)
    if (label) 
      text(x$categ.coord[, axes], labels = rownames(x$categ.coord), 
           pos = 3, cex = cex, ...)
  }
  if (choice == "cor") {
    par(pty = "s")
    if (p1 == 0) 
      stop("Argument 'cor' is inappropriate for only qualitative data")
    if (is.null(xlim)) 
      xlim <- c(-1, 1) * 1.3
    if (is.null(ylim)) 
      ylim <- c(-1, 1) * 1.3
    plot(0, 0, type = "n", xlab = lab.x, ylab = lab.y, xlim = xlim, 
         ylim = ylim, cex = cex, ...)
    x.cercle <- seq(-1, 1, by = 0.01)
    y.cercle <- sqrt(1 - x.cercle^2)
    lines(x.cercle, y = y.cercle)
    lines(x.cercle, y = -y.cercle)
    abline(v = 0, lty = 2, cex = cex)
    abline(h = 0, lty = 2, cex = cex)
    
    if(is.null(col.var)){
      for (j in 1:nrow(quanti.coord)) {
        arrows(0, 0, quanti.coord[j, dim1], quanti.coord[j, 
                                                         dim2], length = 0.1, angle = 15, code = 2, cex = cex, 
               ...)
        if (label) {
          if (abs(quanti.coord[j, dim1]) > abs(quanti.coord[j, 
                                                            dim2])) {
            if (quanti.coord[j, dim1] >= 0) 
              pos <- 4
            else pos <- 2
          }
          else {
            if (quanti.coord[j, dim2] >= 0) 
              pos <- 3
            else pos <- 1
          }
          text(quanti.coord[j, dim1], quanti.coord[j, dim2], 
               labels = rownames(quanti.coord)[j], pos = pos, 
               cex = cex, ...)
        }
      }
    } else {
      for (j in 1:nrow(quanti.coord)) {
        arrows(0, 0, quanti.coord[j, dim1], quanti.coord[j, 
                                                         dim2], length = 0.1, angle = 15, code = 2, cex = cex,col=col.var[j],
               ...)
        if (label) {
          if (abs(quanti.coord[j, dim1]) > abs(quanti.coord[j, 
                                                            dim2])) {
            if (quanti.coord[j, dim1] >= 0) 
              pos <- 4
            else pos <- 2
          }
          else {
            if (quanti.coord[j, dim2] >= 0) 
              pos <- 3
            else pos <- 1
          }
          text(quanti.coord[j, dim1], quanti.coord[j, dim2], 
               labels = rownames(quanti.coord)[j], pos = pos, 
               cex = cex,col=col.var[j], ...)
        }
      }
    } 
  }
}

