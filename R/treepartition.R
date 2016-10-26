#' Convert tree partitions into matrix containing leaf rectangles
#' 
#' Convert the recursively defined partitions of a tree into a matrix containing just the leaf rectangles
#' 
#' The first row pertains to the global bounding box, .i.e. the rectangle containing all data
#' The columns in detail are:
#' columns 1-4 (xleft,ybottom,xright,ytop) are the bounding box for the respective rectangle
#' columns 5-6 (yy1,yy2) contain the (x,y) coords of the center of the rectangle
#' area gives the rectangular area computed by $ScaleAREA * delta X * delta Y$
#' yval for factors: maximum class probability, for regression: conditional average of response inside rectangle
#' lab label used by \link{plot_partition}
#' maxClass for factors only: majority class label inside rectangle
#'
#' @param TreeFit An object of class "tree"
#' @param label A character string giving the column of the frame component of tree to be used to label the regions.
#' @param add If true, add to existing plot, otherwise start a new plot.
#' @param ordvars The ordering of the variables to be used in a 2D plot. Specify the names in a character string of length 2; the first will be used on the x axis.
#' @param ndigits number of digits to display/keep
#' @param ScaleAREA multiplier applied to simple area computations 
#' @param verbose level of verbosity
#' @param ... Graphical parameters passed to \code{line}
#'
#' @return data.frame where each row describes a leaf rectangle of the tree.
#' @export
#'
#' @examples
#' # examples to come
tree_partition <- function (TreeFit, 
                            label = "yval", 
                            add = FALSE, 
                            ordvars, 
                            ndigits = 3,
                            ScaleAREA = 1, 
                            verbose = 0, ...) {
  if (class(TreeFit)[1] == "tree") {
    if (inherits(TreeFit, "singlenode")) 
      stop("cannot plot singlenode tree")
    if (!inherits(TreeFit, "tree")) 
      stop("not legitimate tree")
    frame <- TreeFit$frame
    leaves <- frame$var == "<leaf>"
    var <- unique(as.character(frame$var[!leaves]))
    if (length(var) > 2L || length(var) < 1L) 
      stop("tree can only have one or two predictors")
    nlevels <- sapply(attr(TreeFit, "xlevels"), length)
    if (any(nlevels[var] > 0L)) 
      stop("tree can only have continuous predictors")
    x <- rep(NA, length(leaves))
    x[!leaves] <- as.double(substring(frame$splits[!leaves, 
                                                   "cutleft"], 2L, 100L))
    m <- model.frame(TreeFit)
    if (length(var) == 1L) {
      x <- sort(c(range(m[[var]]), x[!leaves]))
      if (is.null(attr(TreeFit, "ylevels"))) 
        y <- frame$yval[leaves]
      else y <- frame$yprob[, 1L]
      y <- c(y, y[length(y)])
      if (add) 
        lines(x, y, type = "s", ...)
      else {
        a <- attributes(attr(m, "terms"))
        yvar <- as.character(a$variables[1 + a$response])
        xo <- m[[yvar]]
        if (is.factor(xo)) 
          ylim <- c(0, 1)
        else ylim <- range(xo)
        plot(x, y, ylab = yvar, xlab = var, type = "s", 
             ylim = ylim, xaxs = "i", ...)
      }
      invisible(list(x = x, y = y))
    }
    else {
      if (!missing(ordvars)) {
        ind <- match(var, ordvars)
        if (any(is.na(ind))) 
          stop("unmatched names in vars")
        var <- ordvars[sort(ind)]
      }
      rx <- range(m[[var[1L]]])
      rx <- rx + c(-0.025, 0.025) * diff(rx)
      rz <- range(m[[var[2L]]])
      rz <- rz + c(-0.025, 0.025) * diff(rz)
      xrange <- c(rx, rz)[c(1, 3, 2, 4)]
      xcoord <- NULL
      ycoord <- NULL
      if (0) 
        if (!add) 
          plot(rx, rz, xlab = var[1L], ylab = var[2L], 
               type = "n", xaxs = "i", yaxs = "i", ...)
      xy <- leaf_rectangles(x, 
                            frame$var, 
                            xrange, 
                            xcoord, 
                            ycoord, 
                            var, 
                            verbose = verbose)
      yy <- matrix(xy$ycoord, nrow = 2L)
      if (verbose > 1) 
        browser()
      TreeRects = matrix(xy$xrangeAll, ncol = 4, byrow = TRUE)
      colnames(TreeRects) = c("xleft", "ybottom", "xright", 
                              "ytop")
      TreeRects = as.data.frame(rbind(c(rx, rz)[c(1, 3, 
                                                  2, 4)], TreeRects))
      rownames(TreeRects) = paste0("rect", 1:nrow(TreeRects) - 
                                     1)
      TreeRects[, c("yy1", "yy2")] = rbind(c(NA, NA), 
                                           t(yy))
      TreeRects[, "area"] = ScaleAREA * (TreeRects[, "xright"] - 
                                           TreeRects[, "xleft"]) * (TreeRects[, "ytop"] - 
                                                                      TreeRects[, "ybottom"])
      attr(TreeRects, "vars") = var
      lab <- frame$yval[leaves]
      if (is.factor(TreeFit$y)) {
        classProb = round(apply(frame$yprob[leaves, 
                                            ], 1, max), 3)
        lab = paste(lab, classProb, sep = "\n ")
        classTable = table(TreeFit$y)
        baseProb = as.numeric(classTable)/sum(classTable)
        names(baseProb) = names(classTable)
        attr(TreeRects, "baseProb") = baseProb
        classProb1 = max(classTable)/sum(classTable)
        TreeRects[, "yval"] = signif(c(classProb1, classProb), 
                                     3L)
        TreeRects[-1, "maxClass"] = colnames(frame$yprob)[apply(frame$yprob[leaves, 
                                                                            ], 1, which.max)]
        TreeRects[1, "maxClass"] = names(classTable)[which.max(classTable)]
      }
      TreeRects[, "lab"] = c("", as.character(lab))
      if (is.null(frame$yprob)) {
        lab <- format(signif(lab, 3L))
        TreeRects[, "yval"] = as.numeric(c(signif(frame[1, 
                                                        "yval"], 3L), lab))
        TreeRects[, "lab"] = as.character(TreeRects[, 
                                                    "yval"])
      }
      else if (match(label, attr(TreeFit, "ylevels"), 
                     nomatch = 0L)) {
        lab <- format(signif(frame$yprob[leaves, label], 
                             3L))
        if (!is.null(ndigits)) 
          lab = as.character(round(as.numeric(lab), 
                                   ndigits))
        TreeRects[, "yval"] = as.numeric(c(signif(frame$yprob[1, 
                                                              "TRUE"], 3L), lab))
        TreeRects[, "lab"] = as.character(TreeRects[, 
                                                    "yval"])
      }
    }
  }
  else if (class(TreeFit)[1] == "rpart" | "ctFit" %in% names(TreeFit)) {
    if (class(TreeFit)[1] == "rpart") {
      party_rp <- partykit::as.party(TreeFit)
      tmp = attr(formula(TreeFit), "dataClasses")
    }
    else {
      party_rp = TreeFit$ctFit
      tmp = attr(formula(TreeFit$ctFit), "dataClasses")
    }
    rotX = cbind.data.frame(TreeFit$y, TreeFit$x)
    colnames(rotX)[1] = names(tmp)[1]
    rx <- range(rotX[[ordvars[1L]]])
    rz <- range(rotX[[ordvars[2L]]])
    BBOX = list(rx, rz)
    names(BBOX) = ordvars
    rx <- rx + c(-0.025, 0.025) * diff(rx)
    rz <- rz + c(-0.025, 0.025) * diff(rz)
    Leafs = PartitionParty(party_rp, ordvars, PLOT = F, 
                           BBOX = BBOX)
    nl = length(Leafs)
    LeafProbs = tapply(as.numeric(rotX[, 1]), predict(party_rp, 
                                                      type = "node"), function(y) mean(y))
    TreeRects = as.data.frame(matrix(NA, ncol = 4, nrow = nl + 
                                       1))
    colnames(TreeRects) = c("xleft", "ybottom", "xright", 
                            "ytop")
    rownames(TreeRects) = paste0("rect", 0:nl)
    TreeRects[1, ] = c(rx, rz)[c(1, 3, 2, 4)]
    k = 2
    for (i in names(Leafs)) {
      Leaf = Leafs[[as.character(i)]]
      gt = grep(">", colnames(Leaf))
      lt = grep("<", colnames(Leaf))
      TreeRects[k, ] = c(xleft = Leaf[ordvars[1], gt], 
                         ybottom = Leaf[ordvars[2], gt], xright = Leaf[ordvars[1], 
                                                                       lt], ytop = Leaf[ordvars[2], lt])
      k = k + 1
    }
    TreeRects[, c("yy1")] = rowMeans(TreeRects[, c(1, 3)])
    TreeRects[, c("yy2")] = rowMeans(TreeRects[, c(2, 4)])
    TreeRects[, "area"] = ScaleAREA * (TreeRects[, "xright"] - 
                                         TreeRects[, "xleft"]) * (TreeRects[, "ytop"] - TreeRects[, 
                                                                                                  "ybottom"])
    TreeRects[, "yval"] = c(mean(as.numeric(rotX[, 1]), 
                                 na.rm = T), LeafProbs[names(Leafs)])
    TreeRects[, "lab"] = as.character(format(TreeRects[, 
                                                       "yval"], digits = 3))
    if (is.factor(rotX[, 1])) {
      myPropTable = function(y) {
        ty = table(y)
        return(max(ty/sum(ty)))
      }
      classProb = tapply(rotX[, 1], predict(party_rp, 
                                            type = "node"), myPropTable)
      maxClass = tapply(rotX[, 1], predict(party_rp, type = "node"), 
                        function(y) which.max(table(y)))
      classTable = table(rotX[, 1])
      baseProb = as.numeric(classTable)/sum(classTable)
      names(baseProb) = names(classTable)
      attr(TreeRects, "baseProb") = baseProb
      classProb1 = max(classTable)/sum(classTable)
      TreeRects[, "yval"] = signif(c(classProb1, classProb), 
                                   3L)
      TreeRects[, "maxClass"] = c("", maxClass)
      TreeRects[1, "maxClass"] = names(classTable)[which.max(classTable)]
    }
  }
  ii = order(TreeRects$yval, decreasing = TRUE)
  TreeRects = TreeRects[ii, ]
  return(TreeRects)
}


# Calculate the leaf rectangles if the chosen tree algorithm comes 
# from the tree package.
leaf_rectangles <- function(x, 
                            v, 
                            xrange, 
                            xcoord = NULL, 
                            ycoord = NULL, 
                            tvar, 
                            i = 1L, 
                            xrangeAll = NULL, 
                            verbose = 0) {
  if (v[i] == "<leaf>") {
    y1 <- (xrange[1L] + xrange[3L])/2
    y2 <- (xrange[2L] + xrange[4L])/2
    if (verbose) {
      cat("leaf:", i, xrange, "\n")
      rn = runif(3)
      COL = rgb(rn[1], rn[2], rn[3])
      rect(xleft = xrange[1L], ybottom = xrange[2L], 
           xright = xrange[3L], ytop = xrange[4L], col = COL)
    }
    xrangeAll = c(xrangeAll, xrange)
    return(list(xcoord = xcoord, ycoord = c(ycoord, 
                                            y1, y2), i = i, xrangeAll = xrangeAll))
  }
  if (v[i] == tvar[1L]) {
    xcoord <- c(xcoord, x[i], xrange[2L], x[i], xrange[4L])
    xr <- xrange
    xr[3L] <- x[i]
    ll2 <- Recall(x, v, xr, xcoord, ycoord, tvar, i + 
                    1L, xrangeAll)
    xr <- xrange
    xr[1L] <- x[i]
    return(Recall(x, v, xr, ll2$xcoord, ll2$ycoord, 
                  tvar, ll2$i + 1L, ll2$xrangeAll))
  }
  else if (v[i] == tvar[2L]) {
    xcoord <- c(xcoord, xrange[1L], x[i], xrange[3L], 
                x[i])
    xr <- xrange
    xr[4L] <- x[i]
    ll2 <- Recall(x, v, xr, xcoord, ycoord, tvar, i + 
                    1L, xrangeAll)
    xr <- xrange
    xr[2L] <- x[i]
    return(Recall(x, v, xr, ll2$xcoord, ll2$ycoord, 
                  tvar, ll2$i + 1L, ll2$xrangeAll))
  }
  else stop("wrong variable numbers in tree.")
}
