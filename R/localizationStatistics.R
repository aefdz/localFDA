#' Localization Distances Statistics
#'
#' Estimate the mean and standard deviation of the localization distances mean.
#'
#' @param y matrix p by n, being n the number of functions and p the number of grid points.
#' @param robustify if TRUE the mean and standard deviation are estimated with a the trimmed sample. Default is TRUE.
#' @param whiskerrule Range parameter for the univariate boxplot detection rule. Default = 3.
#' @return a list with the localization distances of each function (localizationDistances),
#' the estimated mean (mean) and standard deviation (sd).
#'
#' @examples
#' localizationStatistics_full <- localizationStatistics(exampleData[,1:101], robustify = TRUE)
#' localizationStatistics_full$trim_mean[c(1, 25, 50 ,75, 100)]
#' localizationStatistics_full$trim_sd[c(1, 25, 50 ,75, 100)]
#'
#' @references Elías, Antonio, Jiménez, Raúl and Yukich, Joe (2020). Localization processes for functional data analysis (submitted).
#'
#' @importFrom stats sd
#' @importFrom graphics boxplot
#' @export
localizationStatistics <- function(y, robustify = TRUE, whiskerrule){

  l_estimator <- matrix(NA, ncol = c(ncol(y)-1), nrow = ncol(y))
  rownames(l_estimator) <- colnames(y)
  colnames(l_estimator) <- 1:c(ncol(y)-1)

  localizationDistances_all <- t(sapply(colnames(y), function(y0) localizationDistances(y, y0)))

  rownames(localizationDistances_all) <- colnames(y)
  colnames(localizationDistances_all) <- paste0("k=", 1:c(dim(y)[2]-1))

  if(robustify == TRUE){
    if(missing(whiskerrule)){whiskerrule <- 3}

    outliers <- unique(unlist(apply(localizationDistances_all, 2, function(x) which(x %in% graphics::boxplot(x, plot = FALSE, range = whiskerrule)$out))))

    output <- list(localizationDistances = localizationDistances_all,
                   trim_mean = colMeans(localizationDistances_all[!rownames(localizationDistances_all) %in% outliers,]),
                   trim_sd = apply(localizationDistances_all[!rownames(localizationDistances_all) %in% outliers,], 2, stats::sd))
  }else{
    output <- list(localizationDistances = localizationDistances_all,
                   mean = colMeans(localizationDistances_all),
                   sd = apply(localizationDistances_all, 2, sd))
  }

  return(output)
}


