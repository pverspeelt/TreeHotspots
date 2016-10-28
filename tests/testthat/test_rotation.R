context("Equal")

test_that("Rotate and inverse rotate returns original data", {
  x <- cbind.data.frame(X = c(0,4,4,0,1,3,3,1), Y = c(0,0,0,0,1.5,1.5,2.5,2.5))
  cm <- colMeans(x[,c("X","Y")]) 
  y1 <- rotate(x, a = 45, center = cm)
  y2 <- rotate(y1, a = 45, center = cm, inverse = TRUE)
  
  expect_equal(y2, x)
  
})