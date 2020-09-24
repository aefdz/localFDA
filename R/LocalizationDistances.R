#' Localization distances
#'
#' Compute the localization distances of order k of the curve \code{y0}.
#'
#' @param y matrix p by n, being n the number of functions and p the number of grid points.
#' @param y0 focal curve (index or character name).
#' @return a vector of length (n-1), being the localization distance of its corresponding order.
#'
#' @examples
#' \dontrun{
#' localizationDistances_1 <- localizationDistances(exampleData, y0 = "1")
#' }
#'
#' @references Elías, Antonio, Jiménez, Raúl and Yukich, Joe (2020). Localization processes for functional data analysis (submitted).
#'
#' @export
localizationDistances <- function(y, y0){

  localizarionProcesses_focal <- localizationProcesses(y, y0)$lc

  l_estimator <- apply(localizarionProcesses_focal, 2, function(x) mean(abs(y[,y0] - x)))

  return(l_estimator)
}


