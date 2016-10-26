#' Plot the partitions of a tree model
#' 
#' Plot the partitions of a tree involving one or two variables.
#'
#' @param xy object returned by \code{tree_partition}
#' @param add If true, add to existing plot, otherwise start a new plot.
#' @param col color
#' @param lab axis label
#' @param ... Graphical parameters
#'
#' @return None
#' @export
#'
#' @examples
#' 
#' plot_partition()
plot_partition <- function(xy, add = FALSE, col = NULL, lab = NULL, ...) {
  if (!add) {
    rx = as.numeric(xy[1, c("xleft", "xright")])
    ry = as.numeric(xy[1, c("ybottom", "ytop")])
    plot(
      rx,
      ry,
      xlab = attr(xy, "vars")[1L],
      ylab = attr(xy, "vars")[2L],
      type = "n",
      xaxs = "i",
      yaxs = "i",
      ...
    )
  }
  if (is.null(col)) {
    rn = matrix(runif(3 * nrow(xy)), ncol = 3)
    COL = rgb(rn[, 1], rn[, 2], rn[, 3])
    
  } else if (is.character(col)) {
    tmpCol = (xy[, col] + min(xy[, col]))
    tmpCol = tmpCol / max(tmpCol)
    COL = rgb(tmpCol, tmpCol, tmpCol, 0.4)
    #browser()
  } else {
    stopifnot(length(col) == nrow(xy))
    COL = col
  }
  
  for (i in 1:nrow(xy)) {
    rect(
      xleft = xy[i, "xleft"],
      ybottom = xy[i, "ybottom"],
      xright = xy[i, "xright"],
      ytop = xy[i, "ytop"],
      col = COL[i]
    )
  }
  if (!is.null(lab)) {
    yy = t(xy[, c("yy1", "yy2")])
    lab = as.character(xy[, "lab"])
    text(yy[1L, ], yy[2L, ], lab)
  }
}

