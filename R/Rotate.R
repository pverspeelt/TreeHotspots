#' rotate data by multiplying with a rotation matrix.
#'
#' @param x data frame to be rotated
#' @param angle angle to rotate over
#' @param center center of rotation
#' @param inverse invert previously applied rotation
#'
#' @export
#'
#' @examples
#' example to come
rotate <- function (x, angle = 45, center = colMeans(x), inverse = FALSE) {
  # another possible choice would be colMeans(x[,c("X","Y")]) 
  # or colMeans(x[, c("latitude", "longitude")])
  
  # insert test missing x data frame. 
  # insert test on numeric columns
  # does it need to be purely numeric?
  

  theta <- pi * (angle / 180)
  rotX <- x
  #rotation matrix:
  rotation <- rbind(c(cos(theta), -sin(theta)), c(sin(theta), cos(theta)))
  if (inverse) {
    if (theta != 0)
      rotX <- as.data.frame(t(t(rotation) %*% t(as.matrix(x))))
    #rotX = rotX + center
    rotX <- sweep(rotX, 2, center, FUN = "+")
  }
  else {
    rotX <- sweep(rotX, 2, center, FUN = "-")
    if (theta != 0)
      #rotX = rotX - center
      rotX <- as.data.frame(t(rotation %*% t(as.matrix(rotX))))
  }
  colnames(rotX) <- colnames(x)
  return(rotX)
}





test_rotation <- function(x, angles = c(0, 30, 45, 60), center = c(0, 0), OVERLAY = FALSE){
  
  # insert test missing x
  
  for (a in angles) {
    rotX <- rotate(x[, c("X","Y")], a, center)
    plot(Y ~ X, data = rotX, pch = 20, col = rgb(0,0,1,0.5), cex=0.75, main = paste("angle ", a))
    lines(rotX[c(1:4, 1), "X"], rotX[c(1:4, 1), "Y"])
    lines(rotX[c(5:8, 5), "X"], rotX[c(5:8, 5), "Y"], col=2)

    #rotate them back:
    rotX2 <- rotate(rotX, a, center, inverseRot = TRUE)
    cat("angle ", a, ", mse:", round(mean((rotX2[, c("X","Y")] - x[, c("X", "Y")])^2), 3), "\n")
    plot(Y ~ X, data = rotX2, pch = 20, col = rgb(0, 0, 1, 0.5), cex = 0.75, main = paste("reverting angle ", a))
  }
}  

