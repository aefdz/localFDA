#' Localization processes
#'
#' Compute the localization processes of order k of the curve \code{y0}.
#'
#' @param y matrix p by n, being n the number of functions and p the number of grid points.
#' @param y0 focal curve index or name
#' @return a list with one element, \code{lc}, a matrix of size p x (n-1), being the (n-1) columns the localization processes of its corresponding order.
#'
#' @examples
#' localizationProcesses_1 <- localizationProcesses(exampleData, y0 = "1")
#'
#' @references Elías, Antonio, Jiménez, Raúl and Yukich, Joe (2020). Localization processes for functional data analysis (submitted).
#'
#' @export
localizationProcesses <- function(y, y0){
  N <- dim(y)[2]
  P <- dim(y)[1]

  x <- as.numeric(rownames(y))

  # the nearest in L1
  D <- y[, -which(colnames(y) == y0)] - y[,y0]

  #piecewise estimator
  y_NS_involved <- t(apply(abs(D), 1, function(x) names(sort(x, decreasing = FALSE, index.return = TRUE)$x)))
  y_NS <- t(sapply(1:nrow(y_NS_involved), function(x) y[x,-which(colnames(y) == y0)][y_NS_involved[x,]]))

  rownames(y_NS) <- rownames(y)
  colnames(y_NS) <- paste0("k=", 1:c(dim(y)[2]-1))

  return(list(lc = y_NS))
}


