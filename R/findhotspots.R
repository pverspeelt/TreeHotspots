#' Find spatial clusters using supervised learning methods
#' 
#' Here we consider one fixed rotation of the data and seek hot spots using a classification tree
#'
#' @param formula A formula expression. The left-hand-side (response) should be either a numerical vector when a regression tree will be fitted or a factor, when a classification tree is produced. The right-hand-side should be a series of numeric or factor variables separated by +; there should be no interaction terms. Both . and - are allowed: regression trees can have offset terms.
#' @param rotX data, possibly already rotated
#' @param NullClass if y is a factor, this is the category used for the background
#' @param minsize minimum number of points inside a cluster
#' @param minArea minimum area of a cluster, units: lat*lon
#' @param maxArea maximum area of a cluster, units: lat*lon
#' @param ORfilter filter on minimum and maximum odds ratios (OR)
#' @param TreeAlgorithm which tree algorithm to choose
#' @param verbose level of verbosity
#' @param ... further arguments to tree algorithm
#'
#' @return identified clusters (if any)
#' 
#' @export
#'
#' @examples 
#' # example to come
find_hotspots <- function(formula = violent ~ X + Y,
                          rotX,
                          NullClass = "0",
                          minsize = 200,
                          minArea = 20,
                          maxArea = 250,
                          ORfilter = list(OR <- TRUE, OR1 <- 1.8, OR2 <- 0.1),
                          TreeAlgorithm = c("rpart", "ctree", "tree")[1],
                          verbose = 1,
                          ...  ) {
    if (TreeAlgorithm == "tree") {
      rotX <- model.frame(formula, rotX)
      ycol <- colnames(rotX)[1]
      stopifnot(is.factor(rotX[, 1]))
      xcols <- colnames(rotX)[-1]

      fit <- tree::tree(formula, 
                        data = rotX, 
                        minsize = minsize, 
                        ...
                        )
      if (inherits(fit, "singlenode") |
          !inherits(fit, "tree"))
        return(NULL)
    } else if (TreeAlgorithm == "rpart") {
      fit <- rpart::rpart(formula, 
                          data = rotX, 
                          x = TRUE, 
                          y = TRUE,
                          model = TRUE,
                          control = rpart::rpart.control(minsplit = minsize),
                          ...)
      
      tmp <- attr(formula(fit), "dataClasses")
      ycol <- names(tmp)[1]
      xcols <- names(tmp)[-1]
      fit$y <- rotX[, ycol]
      ##############VERY DANGEROUS, VERY IDIOTIC FIX !!!#############
      #rotX <<- rotX;#attach(rotX)
      #party_rp <- as.party(fit)
    } else if (TreeAlgorithm == "ctree") {
      fit <- partykit::ctree(formula,
                             data = rotX,
                             control = partykit::ctree_control(minsplit = minsize),
                             ...)
      
      tmp <- attr(formula(fit), "dataClasses")
      ycol <- names(tmp)[1]
      xcols <- names(tmp)[-1]
      
      ctFit <- fit
      fit <- list()
      fit$ctFit <- ctFit
      fit$y <- rotX[, ycol]
      fit$x <- rotX[, xcols]
      ##############VERY DANGEROUS, VERY IDIOTIC FIX !!!#############
      #rotX <<- rotX;#attach(rotX)
      #party_rp <- as.party(fit)
    }
    #browser()
    xy <- tree_partition(fit, ordvars = xcols)
    
    if (verbose)
      cat("overall avg:", xy[1, "yval"], ", numRect <- ", nrow(xy), "\n")
    if (verbose > 1) {
      #browser()
      if (TreeAlgorithm == "tree")
        partition.tree(fit)
      #point to the cluster outside
      #getGeoCode("850 Bryant Street, San Francisco")
      #points(-122.4038,37.7753, col <- "red", pch=20)
    }
    if (is.factor(fit$y)) {
      #the odds ratios need a different baseline probability for each factor level!
      baseProb <- attr(xy, "baseProb")
      #lab <- matrix(unlist(strsplit(xy[,"lab"], "\n")[-1]),ncol=2,byrow=T)
    }
    
    if (ORfilter$OR == TRUE) {
      #correct the extreme values 0 and 1:
      xy[xy[, "yval"] > 0.999, "yval"] <- 0.999
      xy[xy[, "yval"] < 0.01, "yval"] <- 0.01
      #change class percentage to OR
      xy[, "yval"] <- xy[, "yval"] / (1 - xy[, "yval"])
      # if (is.factor(fit$y)) {#the odds ratios need a different baseline probability for each factor level!
      #   xy[,"yval"]=xy[,"yval"]/c(xy[1,"yval"],baseProb[lab[,1]])
      # } else   xy[,"yval"]=xy[,"yval"]/xy[1,"yval"]
      OR <- xy[, "yval"] / xy[1, "yval"]
    } else {
      # if (is.factor(fit$y)) {#the odds ratios need a different baseline probability for each factor level!
      #   OR=xy[,"yval"]/c(xy[1,"yval"],baseProb[lab[,1]])
      # } else
      OR <- xy[, "yval"] / xy[1, "yval"]
    }
    #browser()
    hotspots <- rep(TRUE, nrow(xy))
    
    if (!is.null(NullClass)) {
      #eliminated so far:
      ElSoFar <- sum(as.character(xy$maxClass) == NullClass)
      if (verbose)
        cat(ElSoFar, "instances of NULL class eliminated \n")
      ToKeep <- as.character(xy$maxClass) != NullClass
      xy <- subset(xy, ToKeep)
      hotspots <- subset(hotspots, ToKeep)
      OR <- subset(OR, ToKeep)
    }
    
    if (verbose < 0)
      browser()
    
    if (!is.null(ORfilter$OR1) &
        !is.null(ORfilter$OR2)) {
      hotspots <- hotspots & (OR > ORfilter$OR1 | OR < ORfilter$OR2)
    } else if (!is.null(ORfilter$OR1)) {
      hotspots <- hotspots & (OR > ORfilter$OR1)
    } else if (!is.null(ORfilter$OR2)) {
      hotspots <- hotspots & (OR < ORfilter$OR2)
    }
    ElSoFar <- sum(!hotspots)
    if (verbose)
      cat("OR filter eliminates", ElSoFar, "rectangles\n")
    
    # if (0) {
    #   #Compute the areas in km^2:
    #   spots <- Rect2PBSpolys(xy)
    #   Areakm2 <- suppressWarnings(PBSmapping::calcArea(PBSmapping::as.PolySet(spots[, 1:5], projection =
    #                                                                            "LL")))
    #   xy[, "area"] <- Areakm2$area
    # }
    
    if (!is.null(minArea))
      hotspots <- hotspots & xy[, "area"] > minArea
    if (verbose)
      cat("minArea filter eliminates another",
          sum(!hotspots) - ElSoFar,
          "rectangles\n")
    ElSoFar <- sum(!hotspots)
    if (!is.null(maxArea))
      hotspots <- hotspots & xy[, "area"] < maxArea
    if (verbose)
      cat("maxArea filter eliminates another",
          sum(!hotspots) - ElSoFar,
          "rectangles\n")
    
    if (all(!hotspots))
      return(NULL)
    #hotspots <- which( (OR>ORfilter$OR1 | OR < ORfilter$OR2) & (xy[,"area"]>minArea & xy[,"area"]<maxArea))
    rotPolys <- Rect2PBSpolys(xy[hotspots, , drop == F])
    rotPolys <- subset(rotPolys, rowSums(is.na(rotPolys)) == 0)
    return(rotPolys)
    ### identified clusters (if any)
  }
