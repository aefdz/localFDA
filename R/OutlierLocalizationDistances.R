#' Outlier localization distances
#'
#' Compute the localization distances of order k of the curve \code{y0}.
#'
#' @param X matrix p by n, being n the number of functions and p the number of grid points.
#' @param localrule Local distance rule: the method marks a curve as outlier if
#' its k order localization distances are outliers in more than local_rulex100 percent of the k-order univariate boxplots.
#' Default is 0.90 so a function must be at least an outlier in 90 percent of the k-order localization distances.
#' @param whiskerrule Parameter for the whiskers of the univariate boxplot of the localization distances of order kth. Default value is 3.
#'
#' @return A list
#'
#' @examples
#' outliers <- outlierLocalizationDistance(outlierData, localrule = 0.9, whiskerrule = 3)
#' outliers$outliers_ld_rule
#'
#' @references Elías, Antonio, Jiménez, Raúl and Yukich, Joe (2020). Localization processes for functional data analysis (submitted).
#'
#' @importFrom graphics boxplot
#' @export
outlierLocalizationDistance <- function(X, localrule = 0.9, whiskerrule = 3){
  ld <- localizationStatistics(X, robustify = TRUE)

  outliers_table <- table(colnames(X)[unlist(apply(ld$localizationDistances, 2, function(x) which(x %in% graphics::boxplot(x, plot = FALSE, range = whiskerrule)$out)))])
  outliers_final <- names(which(outliers_table  >= floor(ncol(X)*localrule)))

  return(list(outliers_ld_rule = outliers_final,
         outliers_all = colnames(X)[outliers_table],
         outliers_all_table = outliers_table)
  )
}


