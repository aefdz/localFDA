#' Outlying localization distances
#'
#' Compute the localization distances of order k of the curve \code{y0}.
#'
#' @param X matrix p by n, being n the number of functions and p the number of grid points.
#' @param local_rule Local distance rule: the method marks a curve as outlier if
#' its k order localization distances are outliers in more than local_rulex100% of the k-order univariate boxplots.
#' Default is 0.90 so a function must be at least an outlier in 90% of the k-order localization distances.
#' @param whisker_rule Parameter for the whiskers of the univariate boxplot of the
#' localization distances of order kth. Default value is 3.
#' @return
#'
#' @examples
#' outliers <- outlierLocalizationDistance(outliersData, local_rule = 0.9, whisker_rule = 3)
#' outliers$outliers_ld_rule
#'
#' @references Elías, Antonio, Jiménez, Raúl and Yukich, Joe (2020). Localization processes for functional data analysis (submitted).
#'
#' @export
outlierLocalizationDistance <- function(X, local_rule = 0.9, whisker_rule = 3){
  ld <- localizationStatistics(X, robustify = TRUE)

  outliers_table <- table(colnames(X)[unlist(apply(ld$localizationDistances, 2, function(x) which(x %in% boxplot(x, plot = FALSE, range = whisker_rule)$out)))])
  outliers_final <- names(which(outliers_table  >= floor(ncol(X)*local_rule)))

  return(list(outliers_ld_rule = outliers_final,
         outliers_all = colnames(X)[outliers_table],
         outliers_all_table = outliers_table)
  )
}


