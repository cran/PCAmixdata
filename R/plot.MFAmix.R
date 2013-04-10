#' @S3method plot MFAmix
#' @export
#' @name plot.MFAmix
#' @title Graphs of a MFAmix analysis 
#' @description Graphs of MFAmix analysis : plot of the individuals, the partial individuals, 
#' the groups, the numeric variables on the correlation circle, the categories of 
#' the categorials variables, the partial axes of each separates analyses
#' @param x  an object of class MFAmix 
#' @param axes alength 2 vector specifying the components to plot
#' @param choice  a string corresponding to the graph that you want to do 
#' ("ind" for the individual or categorical variables graph, "var" for the
#' correlation circle of the quantitative variables, "axes" for the graph of the partial axes,
#' "group" for the groups representation)
#' @param lab.grpe boolean, if TRUE, the labels of the groups are drawn
#' @param lab.var boolean, if TRUE, the labels of the variables are drawn
#' @param lab.ind boolean, if TRUE, the labels of the individuals and of the categories are drawn
#' @param lab.par boolean, if TRUE, the labels of the partial points are drawn
#' @param habillage string corresponding to the color which are used.
#' If "ind", one color is used for each individual;
#' if "group" the individuals are colored according to the group; 
#' else if it is the name of a categorical variable, it colors according to the different
#' categories of this variable
#' @param col.hab  the colors to use. By default, colors are chosen
#' @param invisible a string; for choice ="ind", the individuals can be omit (invisible = "ind"),
#' or the categories of the the categorical variables (invisible= "quali")
#' @param partial  list of the individuals or of the categories for which 
#' the partial points should be drawn (by default, partial = NULL and no partial points are drawn) 
#' @param lim.cos2.var value of the square cosinus below which the points are not drawn
#' @param chrono boolean, if TRUE, the partial points of a same point
#' are linked (useful when groups correspond to different dates)
#' @param xlim numeric vectors of length 2, giving the x coordinates range
#' @param ylim numeric vectors of length 2, giving the y coordinates range
#' @param cex cf. function \code{par} in the \bold{graphics} package
#' @param title string corresponding to the title of the graph you want to draw 
#' (by default NULL and a title is chosen)
#' @param new.plot boolean, if TRUE, a new graphical device is created
#' @param ... further arguments passed to or from othe methods
#' @return Returns the graph you want to plot
#' @examples
#' blabla
#' 
#' 
plot.MFAmix<-function (x, axes = c(1, 2), choice = "axes", lab.grpe = TRUE, lab.var = TRUE, 
                       lab.ind = TRUE, lab.par = FALSE, habillage = "ind",
                       col.hab = NULL, invisible = NULL,  partial = NULL, lim.cos2.var = 0,
                       chrono = FALSE, xlim = NULL,  ylim = NULL, cex = 1, title = NULL,
                       palette = NULL, new.plot = FALSE,leg=TRUE, ...) 
{
  
  
  res.mfa <- x
  if (!inherits(res.mfa, "MFAmix")) 
    stop("non convenient data")
  if (is.null(palette)) 
    palette(c("black", "red", "green3", "blue", "cyan", "magenta", 
              "darkgray", "darkgoldenrod", "darkgreen", "violet", 
              "turquoise", "orange", "lightpink", "lavender", "yellow", 
              "lightgreen", "lightgrey", "lightblue", "darkkhaki", 
              "darkmagenta", "darkolivegreen", "lightcyan", "darkorange", 
              "darkorchid", "darkred", "darksalmon", "darkseagreen", 
              "darkslateblue", "darkslategray", "darkslategrey", 
              "darkturquoise", "darkviolet", "lightgray", "lightsalmon", 
              "lightyellow", "maroon"))
  lab.x <- paste("Dim ", axes[1], " (", signif(res.mfa$eig[axes[1], 
                                                           2], 4), " %)", sep = "")
  lab.y <- paste("Dim ", axes[2], " (", signif(res.mfa$eig[axes[2], 
                                                           2], 4), " %)", sep = "")
  group <- res.mfa$lst.group
  nbre.grpe <- length(unique(group))
  
  
  
  if (choice == "axes") {
    if (new.plot) 
      dev.new()
    if (is.null(title)) 
      title <- "Partial axes"
    coord.axes <- res.mfa$partial.axes$coord[, axes, drop = FALSE]
    plot(0, 0, xlab = lab.x, ylab = lab.y, xlim = c(-1.1, 
                                                    1.1), ylim = c(-1.1, 1.1), col = "white", asp = 1, 
         cex = cex, main = title)
    x.cercle <- seq(-1, 1, by = 0.01)
    y.cercle <- sqrt(1 - x.cercle^2)
    lines(x.cercle, y = y.cercle)
    lines(x.cercle, y = -y.cercle)
    abline(v = 0, lty = 2, cex = cex)
    abline(h = 0, lty = 2, cex = cex)
    
    if (habillage == "group") {
      if (is.null(col.hab) | length(col.hab) < nbre.grpe) {
        col.hab <- 2:(nbre.grpe + 1)
      }
      i = 1
      couleur.axes <- col.hab[i]
      auxil = strsplit(rownames(res.mfa$partial.axes$coord)[1], 
                       ".", fixed = TRUE)[[1]]
      auxil2 = auxil[length(auxil)]
      for (j in 2:nrow(res.mfa$partial.axes$coord)) {
        auxil = strsplit(rownames(res.mfa$partial.axes$coord)[j], 
                         ".", fixed = TRUE)[[1]]
        if (auxil2 != auxil[length(auxil)]) {
          i = i + 1
          auxil2 = auxil[length(auxil)]
        }
        couleur.axes <- c(couleur.axes, col.hab[i])
      }
    } 
    
    else {
      couleur.axes <- NULL
      for (i in 1:nbre.grpe) couleur.axes <- c(couleur.axes, 
                                               rep("black", ncol(res.mfa$partial.axes$coord)))
    }
    
    for (v in 1:nrow(coord.axes)) {
      arrows(0, 0, coord.axes[v, 1], coord.axes[v, 2], 
             length = 0.1, angle = 15, code = 2, col = couleur.axes[v], 
             cex = cex)
      if (abs(coord.axes[v, 1]) > abs(coord.axes[v, 2])) {
        if (coord.axes[v, 1] >= 0) 
          pos <- 4
        else pos <- 2
      }
      else {
        if (coord.axes[v, 2] >= 0) 
          pos <- 3
        else pos <- 1
      }
      text(coord.axes[v, 1], y = coord.axes[v, 2], labels = rownames(coord.axes)[v], 
           pos = pos, col = couleur.axes[v], cex = cex)
    }
    
    if ((habillage == "group") & (leg==T)) {
      legend("topleft", legend = rownames(res.mfa$group$Lg), 
             text.col = unique(couleur.axes), cex = 0.8 * 
               cex)
    }
  }
  if (choice == "group") {
    if (new.plot) 
      dev.new()
    if (is.null(title)) 
      title <- "Groups representation"
    coord.actif <- res.mfa$group$coord[, axes, drop = FALSE]
    
    if (length(col.hab) == 1) 
      col.hab = rep(col.hab, nbre.grpe)
    if (is.null(col.hab)) {
      col.hab = rep("darkred", nrow(coord.actif))
    }
    
    if (habillage == "group") 
      col.hab <- (2:(nbre.grpe + 1))
    plot(coord.actif, xlab = lab.x, ylab = lab.y, xlim = c(0, 
                                                           1), ylim = c(0, 1), pch = 17, col = col.hab[1:nrow(coord.actif)], 
         cex = cex, main = title, cex.main = cex * 1.2, asp = 1)
    if (lab.grpe) 
      text(coord.actif[, 1], y = coord.actif[, 2], labels = rownames(coord.actif), 
           pos = 3, col = col.hab[1:nrow(coord.actif)], 
           cex = cex)
    
  }
  
  
  
  if (choice=="loadings") {
    if (is.null(title)) title<-"Squared Loadings"
    color<-NULL
    if (habillage == "group") {
      if (is.null(col.hab) | length(col.hab) < nbre.grpe) 
        col.hab <- 2:(nrow(res.mfa$global.pca$sload) + 1)
      for (i in 1:nbre.grpe){  
        if (!is.null(nrow(res.mfa$separate.analyses[[i]]$sload))){
          color <- c(color,rep(col.hab[i], nrow(res.mfa$separate.analyses[[i]]$sload)))}
      }
    
    }    else {
      if (is.null(col.hab) | length(col.hab) < nrow(res.mfa$quanti.var$coord)) 
        color <- rep(1,nrow(res.mfa$quanti.var$coord))
      else color <- col.hab
    }
    
    if (is.null(xlim)){
      xmax <- max(res.mfa$global.pca$sload[,axes[1]])
      xlim <- c(-0.1, xmax*1.2)}
    if (is.null(ylim)){
      ymax <- max(res.mfa$global.pca$sload[,axes[2]])
      ylim <- c(-0.1, ymax*1.2)}
    plot(0, 0, type="n",xlab = lab.x, ylab = lab.y, xlim = xlim, ylim = ylim,cex=cex,main=title)
    abline(v = 0, lty = 2,cex=cex)
    abline(h = 0, lty = 2,cex=cex)
    for (j in 1:nrow(res.mfa$global.pca$sload)) {
      arrows(0, 0, res.mfa$global.pca$sload[j,axes[1]], res.mfa$global.pca$sload[j,axes[2]], length = 0.1, angle = 15, code = 2,cex=cex,col=color[j])
    
        if (res.mfa$global.pca$sload[j,axes[1]] > res.mfa$global.pca$sload[j,axes[2]]) { pos <- 4
        } else  pos <- 3
        text(res.mfa$global.pca$sload[j,axes[1]],res.mfa$global.pca$sload[j,axes[2]],labels = rownames(res.mfa$global.pca$sload)[j], pos = pos,cex=cex,col=color[j])
      
    }
    if ((habillage == "group")& (leg==T)) {
    legend("topright", legend = rownames(res.mfa$group$Lg), 
           text.col = unique(color), cex = 0.8 * 
             cex)
    }
  }
  
  
  
  
  
  
  
  
  
  if (choice == "var") {
    if (new.plot) 
      dev.new()
    test.invisible <- vector(length = 2)
    if (!is.null(invisible)) {
      test.invisible[1] <- match("actif", invisible)
      test.invisible[2] <- match("sup", invisible)
    }    else test.invisible <- rep(NA, 2)
    col <- NULL
    
    
    if (habillage == "group") {
      if (is.null(col.hab) | length(col.hab) < nbre.grpe) 
        col.hab <- 2:(nrow(res.mfa$quanti.var$coord) + 1)
      for (i in 1:nbre.grpe){  
        if (!is.null(nrow(res.mfa$separate.analyses[[i]]$res.quanti$coord))){
          col <- c(col,rep(col.hab[i], nrow(res.mfa$separate.analyses[[i]]$res.quanti$coord)))}
      }
    }    else {
      if (is.null(col.hab) | length(col.hab) < nrow(res.mfa$quanti.var$coord)) 
        col <- rep(1,nrow(res.mfa$quanti.var$coord))
      else col <- col.hab
    }
    
    
    
    if (is.null(title)) 
      title <- "Correlation circle"
    plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, 
         xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1), col = "white", 
         asp = 1, cex = cex)
    x.cercle <- seq(-1, 1, by = 0.01)
    y.cercle <- sqrt(1 - x.cercle^2)
    lines(x.cercle, y = y.cercle)
    lines(x.cercle, y = -y.cercle)
    abline(v = 0, lty = 2, cex = cex)
    abline(h = 0, lty = 2, cex = cex)
    if (habillage == "group" & is.na(test.invisible[1]) & is.na(test.invisible[2]) & (leg==T)) 
      legend("topleft", legend = rownames(res.mfa$group$Lg), text.col = col.hab, cex = 0.8)
    if (habillage == "group" & is.na(test.invisible[1]) & !is.na(test.invisible[2])& (leg==T)) 
      legend("topleft", legend = rownames(res.mfa$group$Lg), text.col = col.hab, cex = 0.8)
    nrow.coord.var <- 0
    if (!is.null(res.mfa["quanti.var"]$quanti.var$coord) &  is.na(test.invisible[1])) {
      coord.var <- res.mfa$quanti.var$coord[, axes, drop = FALSE]
      nrow.coord.var <- nrow(coord.var)
      for (v in 1:nrow(coord.var)) {
        if (sum(res.mfa$quanti.var$cos2[v, axes], na.rm = TRUE) >= 
              lim.cos2.var && !is.na(sum(res.mfa$quanti.var$cos2[v, 
                                                                 axes], na.rm = TRUE))) {
          arrows(0, 0, coord.var[v, 1], coord.var[v, 
                                                  2], length = 0.1, angle = 15, code = 2, col = col[v], 
                 cex = cex)
          if (lab.var) {
            if (abs(coord.var[v, 1]) > abs(coord.var[v, 
                                                     2])) {
              if (coord.var[v, 1] >= 0) 
                pos <- 4
              else pos <- 2
            }
            else {
              if (coord.var[v, 2] >= 0) 
                pos <- 3
              else pos <- 1
            }
            text(coord.var[v, 1], y = coord.var[v, 2], 
                 labels = rownames(coord.var)[v], pos = pos, 
                 col = col[v], cex = cex)
          }
        }
      }
    }
    
    par(mar = c(5, 4, 4, 2) + 0.1)
  }
  if (choice == "ind") {
    test.invisible <- vector(length = 3)
    if (!is.null(invisible)) {
      test.invisible[1] <- match("ind", invisible)
      test.invisible[2] <- match("ind.sup", invisible)
      test.invisible[3] <- match("quali", invisible)
    }    else test.invisible <- rep(NA, 3)
    nb.ind.actif <- nrow(res.mfa$ind$coord)
    nb.ind.illu <- 0
    nb.ind <- nb.ind.actif+nb.ind.illu
    coord.ind <- res.mfa$ind$coord[, axes, drop = FALSE]
    coord.ind.partiel <- res.mfa$ind$coord.partiel[, axes,drop = FALSE]
    coord.ind.sup <- NULL
    
    coord.quali <- coord.quali.sup <- coord.quali.partiel <- coord.quali.sup.partiel <- NULL
    nrow.coord.quali <- 0
    if (!is.null(res.mfa["quali.var"]$quali.var)) {
      coord.quali <- res.mfa$quali.var$coord[, axes, drop = FALSE]
      coord.quali.partiel <- res.mfa$quali.var$coord.partiel[,axes, drop = FALSE]
      nrow.coord.quali <- nrow(coord.quali)
    }
    
    group.ind.actif <- group.ind.sup <- group.quali <- group.quali.sup <- NULL
    
    if (!is.null(partial)) {
      if (length(partial) == 1) {
        if (partial == "all") {
          group.ind.actif <- 1:nrow(coord.ind)
          if (!is.null(res.mfa["quali.var"]$quali.var)) 
            group.quali <- 1:nrow(coord.quali)
        }        else {
          for (i in 1:length(partial)) {
            if (partial[i] %in% rownames(coord.ind)) 
              group.ind.actif <- c(group.ind.actif, match(partial[i], 
                                                          rownames(coord.ind)))
            
            if (partial[i] %in% rownames(coord.quali)) 
              group.quali <- c(group.quali, match(partial[i], 
                                                  rownames(coord.quali)))
            
          }
        }
      }      else {
        for (i in 1:length(partial)) {
          if (partial[i] %in% rownames(coord.ind)) 
            group.ind.actif <- c(group.ind.actif, match(partial[i], 
                                                        rownames(coord.ind)))
          
          if (partial[i] %in% rownames(coord.quali)) 
            group.quali <- c(group.quali, match(partial[i], 
                                                rownames(coord.quali)))
          
        }
      }
    }
    
    if (is.null(xlim)) {
      xmin <- xmax <- 0
      if (is.na(test.invisible[1])) 
        xmin <- min(xmin, coord.ind[, 1])
      if (is.na(test.invisible[1])) 
        xmax <- max(xmax, coord.ind[, 1])
      if (is.na(test.invisible[1])) 
        xmin <- min(xmin, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                                                          function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))),1])
      if (is.na(test.invisible[1])) 
        xmax <- max(xmax, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                                                          function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                                            1])
      
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        xmin <- min(xmin, coord.quali[, 1])
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        xmax <- max(xmax, coord.quali[, 1])
      
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        xmin <- min(xmin, coord.quali.partiel[unlist(lapply(group.quali, 
                                                            function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                                              1])
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        xmax <- max(xmax, coord.quali.partiel[unlist(lapply(group.quali, 
                                                            function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                                              1])
      xlim <- c(xmin, xmax) * 1.1
    }   
    else {
      xmin = xlim[1]
      xmax = xlim[2]
    }
    
    if (is.null(ylim)) {
      ymin <- ymax <- 0
      if (is.na(test.invisible[1])) 
        ymin <- min(ymin, coord.ind[, 2])
      if (is.na(test.invisible[1])) 
        ymax <- max(ymax, coord.ind[, 2])
      
      if (is.na(test.invisible[1])) 
        ymin <- min(ymin, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                                                          function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                                            2])
      if (is.na(test.invisible[1])) 
        ymax <- max(ymax, coord.ind.partiel[unlist(lapply(group.ind.actif, 
                                                          function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                                            2])
      
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        ymin <- min(ymin, coord.quali[, 2])
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        ymax <- max(ymax, coord.quali[, 2])
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        ymin <- min(ymin, coord.quali.partiel[unlist(lapply(group.quali, 
                                                            function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                                              2])
      if (!is.null(res.mfa["quali.var"]$quali.var) & is.na(test.invisible[3])) 
        ymax <- max(ymax, coord.quali.partiel[unlist(lapply(group.quali, 
                                                            function(k) seq(nbre.grpe * (k - 1) + 1, length = nbre.grpe))), 
                                              2])
      ylim <- c(ymin, ymax) * 1.1
      
    }   
    else {
      ymin = ylim[1]
      ymax = ylim[2]
    }
    
    #Habillage = group
    if (habillage == "group") {
      if (is.null(col.hab) | length(col.hab) != (nbre.grpe)) 
        col.hab <- 2:(nbre.grpe + 1)
      col.ind <- c(rep(1, nb.ind.actif), rep(col.hab, nb.ind.actif))
      
      if(!is.null(res.mfa$quali.var$coord)){
        nbre.moda<-nrow(res.mfa$quali.var$coord)
        col.quali <- c(rep(1, nbre.moda), rep(col.hab, nbre.moda))
      }
    }
    
    
    
    #Habillage = ind  
    if (habillage == "ind") {
      if (is.null(col.hab) | length(col.hab) != nb.ind) {
        col.hab <- 1:nb.ind
      }
      col.ind <- c(col.hab[1:nb.ind.actif], rep(col.hab[1:nb.ind.actif], 
                                                each = nbre.grpe))
      
      if(!is.null(res.mfa$quali.var$coord)){
        nbre.moda<-nrow(res.mfa$quali.var$coord)        
        col.quali <- col.quali.sup <- rep("black", (1 +  nbre.grpe) * nbre.moda)
      }
      
    }
    
    
    #Habillage != group != ind !=none  
    if ((habillage != "none") & (habillage != "ind") & (habillage !="group")) {
      group.act <- (1:nbre.grpe)
      nbre.modalite <- NULL
      liste.quali <- NULL
      for (i in group.act) { 
        if (!is.null(res.mfa$separate.analyses[[i]]$res.categ$coord)){
          for (k in 1:ncol(res.mfa$separate.analyses[[i]]$rec$X.quali)) 
            nbre.modalite <- c(nbre.modalite, nlevels(res.mfa$separate.analyses[[i]]$rec$X.quali[,k]))
          if (i == 1) 
            liste.quali <- c(liste.quali, colnames(res.mfa$separate.analyses[[i]]$rec$X.quali))
          else liste.quali <- c(liste.quali, colnames(res.mfa$separate.analyses[[i]]$rec$X.quali))
        }
      }
      
      
      if (is.double(habillage))
        stop("Enter the name of the variable")
      nom.quali = habillage
      if (!(nom.quali %in% liste.quali)) {
        va.out<-1
        #stop("The variable ", habillage, " is not qualitative")
        var.quali<-eval(parse(text=nom.quali))
        modalite <- levels(var.quali)
        col.ind <- as.numeric(var.quali)
        liste.quali<-c(nom.quali,liste.quali)
      }      else {
        va.out<-0
        modalite <- levels(as.factor(res.mfa$global.pca$rec$X.quali[, nom.quali]))
        col.ind <- as.numeric(as.factor(res.mfa$global.pca$rec$X.quali[, nom.quali]))
      }
      
      if (is.null(col.hab) | length(col.hab) != length(modalite)) 
        col.hab <- 2:(1 + length(modalite))
      col.ind <- col.hab[col.ind]
      
      col.ind <- c(col.ind, rep(col.ind, each = nbre.grpe))
      indice.inf <- sum(nbre.modalite[0:(match(nom.quali,liste.quali) - 1)]) + 1
      indice.sup <- indice.inf + length(modalite) - 1
      if (va.out==0)
        col.quali <- rep("black",nrow(res.mfa$quali.var$coord))
      if (va.out==1)
        col.quali <- rep("black",length(modalite))
      
      if (!is.null(res.mfa$quali.var$coord)) {
        for (i in 1:length(liste.quali)) {
          if (liste.quali[i] == nom.quali) 
            col.quali[indice.inf:indice.sup] <- col.hab
        }
      }
      col.quali <- c(col.quali, rep(col.quali, each = nbre.grpe))
      col.quali.sup <- col.quali
    }
    
    
    
    if (habillage == "none") 
      col.ind <- col.quali <- rep("black",  nb.ind * (nbre.grpe + 1))
    
    
    
    if (new.plot) 
      dev.new(width = min(14, max(8, 8 * (xmax - xmin)/(ymax - 
                                                          ymin))), height = 8)
    
    if (is.null(title) & is.null(invisible)) title <- "Individual factor map"
    if (is.null(title) & (!is.null(invisible))) {
        if (invisible=="quali") title <- "Individual factor map"
        if (invisible=="ind") title <- "Categories factor map"
    }
    
    
    plot(0, 0, main = title, xlab = lab.x, ylab = lab.y, 
         xlim = xlim, ylim = ylim, col = "white", asp = 1, 
         cex = cex)
    abline(v = 0, lty = 2, cex = cex)
    abline(h = 0, lty = 2, cex = cex)
    if (is.na(test.invisible[1])) {
      points(coord.ind, pch = 20, col = col.ind[1:nb.ind.actif], 
             cex = cex)
      if (lab.ind) 
        text(coord.ind[, 1], y = coord.ind[, 2], labels = rownames(coord.ind), 
             pos = 3, col = col.ind[1:nb.ind.actif], cex = cex)
      for (i in group.ind.actif) {
        for (j in 1:nbre.grpe) {
          points(coord.ind.partiel[(i - 1) * nbre.grpe + 
                                     j, ], cex = 0.8 * cex, col = col.ind[nb.ind.actif + 
                                                                            (i - 1) * nbre.grpe + j], pch = 20)
          if (lab.par) 
            text(coord.ind.partiel[(i - 1) * nbre.grpe + 
                                     j, 1], y = coord.ind.partiel[(i - 1) * 
                                                                    nbre.grpe + j, 2], labels = rownames(coord.ind.partiel)[(i - 
                                                                                                                               1) * nbre.grpe + j], pos = 3, col = col.ind[nb.ind.actif + 
                                                                                                                                                                             (i - 1) * nbre.grpe + j], cex = cex)
          if (chrono) {
            if (j > 1) 
              lines(c(coord.ind.partiel[(i - 1) * nbre.grpe + 
                                          (j - 1), 1], coord.ind.partiel[(i - 1) * 
                                                                           nbre.grpe + j, 1]), c(coord.ind.partiel[(i - 
                                                                                                                      1) * nbre.grpe + (j - 1), 2], coord.ind.partiel[(i - 
                                                                                                                                                                         1) * nbre.grpe + j, 2]), col = col.ind[i])
          }
          else lines(c(coord.ind[i, 1], coord.ind.partiel[(i - 
                                                             1) * nbre.grpe + j, 1]), c(coord.ind[i, 2], 
                                                                                        coord.ind.partiel[(i - 1) * nbre.grpe + j, 
                                                                                                          2]), col = col.ind[nb.ind.actif + (i - 
                                                                                                                                               1) * nbre.grpe + j], lty = j)
        }
      }
    }
    
    if (!is.null(coord.quali) & is.na(test.invisible[3])) {
      points(coord.quali, pch = 15, col = col.quali[1:nrow.coord.quali], 
             cex = cex)
      if (lab.var) 
        text(coord.quali[, 1], y = coord.quali[, 2], 
             labels = rownames(coord.quali), pos = 3, col = col.quali[1:nrow.coord.quali], 
             cex = cex)
      for (i in group.quali) {
        for (j in 1:nbre.grpe) {
          points(coord.quali.partiel[(i - 1) * nbre.grpe + 
                                       j, ], pch = 15, col = col.quali[nrow.coord.quali + 
                                                                         (i - 1) * nbre.grpe + j], cex = cex * 0.8)
          if (lab.var & lab.par) 
            text(coord.quali.partiel[(i - 1) * nbre.grpe + 
                                       j, 1], y = coord.quali.partiel[(i - 1) * 
                                                                        nbre.grpe + j, 2], labels = rownames(coord.quali.partiel)[(i - 
                                                                                                                                     1) * nbre.grpe + j], pos = 3, col = col.quali[nrow.coord.quali + 
                                                                                                                                                                                     (i - 1) * nbre.grpe + j], cex = cex)
          if (chrono) {
            if (j > 1) 
              lines(c(coord.quali.partiel[(i - 1) * nbre.grpe + 
                                            (j - 1), 1], coord.quali.partiel[(i - 
                                                                                1) * nbre.grpe + j, 1]), c(coord.quali.partiel[(i - 
                                                                                                                                  1) * nbre.grpe + (j - 1), 2], coord.quali.partiel[(i - 
                                                                                                                                                                                       1) * nbre.grpe + j, 2]), col = col.quali[i])
          }
          else lines(c(coord.quali[i, 1], coord.quali.partiel[(i - 
                                                                 1) * nbre.grpe + j, 1]), c(coord.quali[i, 
                                                                                                        2], coord.quali.partiel[(i - 1) * nbre.grpe + 
                                                                                                                                  j, 2]), col = col.quali[nrow.coord.quali + 
                                                                                                                                                            (i - 1) * nbre.grpe + j], lty = j)
        }
      }
    }
    
    if ((!is.null(partial)) & (habillage == "group") & (leg==T)) 
      legend("topleft", legend = rownames(res.mfa$group$Lg), lty = 1:length(rownames(res.mfa$group$Lg)), text.col = col.hab, 
             col = col.hab, cex = 0.8)
    if ((!is.null(partial)) & (habillage != "group") & (leg==T)) 
      legend("topleft", legend = rownames(res.mfa$group$Lg), lty = 1:length(rownames(res.mfa$group$Lg)), cex = 0.8)
    if ((habillage != "none") & (habillage != "ind") & (habillage != "group") & (leg==T)) 
      legend("topleft", legend = modalite, text.col = col.hab, cex = 0.8)
    
    
  }
}










