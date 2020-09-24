#' Localization classifier
#'
#' Given a training sample with g groups, it predicts the group of the test sample.
#'
#' @param trainingSample matrix p by n, being n the number of functions and p the number of grid points.
#' The colnames of the trainingSample matrix are i_groupName where i goes from 1 to the sample size of the group.
#' @param testSample matrix p by n, being n the number of functions to classify and p the number of grid points.
#' @param classNames  character vector with the group names.
#' @param k_opt Maximum order of the localization processes used in the classification rule.
#' @param g_pi Vector of size g with a priori probabilities for the bayes classifier. If it is missing the probability is defined by
#' the proportion of curves of each group.
#' @return Two named training and test. Training contains the estimations made with the training sample
#' (localization statistics and localization distances). Test contains the classification results
#' (for each incoming data, localization distances in each group, prior probabilities used,
#' likelihood in each group and the predicted_class).
#'
#' @examples
#' \dontrun{
#' X <- classificationData
#' ids_training <- sample(colnames(X), 90)
#' ids_testing <- setdiff(colnames(X), ids_training)
#' trainingSample <- X[,ids_training]
#' testSample <- X[,ids_testing]; colnames(testSample) <- NULL #blind
#' classNames <- c("G1", "G2")
#' classification_results <- localizationClassifier(trainingSample, testSample, classNames, k_opt = 3)
#' }
#'
#' @references Elías, Antonio, Jiménez, Raúl and Yukich, Joe (2020). Localization processes for functional data analysis (submitted).
#'
#' @export
localizationClassifier <- function(trainingSample, testSample, classNames, k_opt, g_pi){
  ids_training <- colnames(trainingSample)
  ids_test <- paste0(1:ncol(testSample), "_toClassify") -> colnames(testSample)

  if(missing(g_pi)){
    g_pi <- sapply(classNames, function(x) sum(grepl(x, ids_training))/length(ids_training))
  }

  #estimation
  g_localizationStatistics <- sapply(classNames,
                                     function(x) localizationStatistics(trainingSample[,grepl(x, ids_training)], robustify = TRUE))

  #classification
  localizationDistances_incoming <- list()
  classifier_incoming <- list()
  predicted_class <- vector(mode = "numeric", length = ncol(testSample))*NA

  for(i in 1:ncol(testSample)){
    aux_localizationDistances_incoming <- list()
    aux_classifier_incoming <- list()

    for(g in 1:length(classNames)){
      g_size <- sum(grepl(classNames[g], ids_training))
      data_plus_new <- cbind(trainingSample[,ids_training[grepl(classNames[g], ids_training)]], testSample[,ids_test[i]])
      colnames(data_plus_new)[dim(data_plus_new)[2]] <- "new_data"

      aux_localizationDistances_incoming[[g]] <- localizationDistances(data_plus_new, "new_data")
      names(aux_localizationDistances_incoming)[g] <- classNames[g]

      aux_classifier_incoming[[g]] <- g_pi[g]*sapply(1:c(g_size-1), function(x) stats::dnorm(aux_localizationDistances_incoming[[g]][x],
                                                                                      mean = unlist(g_localizationStatistics[2,g])[x],
                                                                                      sd = unlist(g_localizationStatistics[3,g])[x]))
      names(aux_classifier_incoming)[g] <- classNames[g]
    }

    localizationDistances_incoming[[i]] <- aux_localizationDistances_incoming
    names(localizationDistances_incoming)[i] <- ids_test[i]

    classifier_incoming[[i]] <- aux_classifier_incoming
    names(classifier_incoming)[i] <- ids_test[i]

    #class
    if(k_opt == 1){
      predicted_class[i] <- names(which.max(sapply(classifier_incoming[[i]], function(x) x[1:k_opt])))
    }else{
      predicted_class[i] <- names(which.max(table(apply(sapply(classifier_incoming[[i]], function(x) x[1:k_opt]), 1, function(x) names(which.max(x))))))
    }
  }

  return(list(training = list(g_localizationStatistics = g_localizationStatistics),
              test = list(localizationDistances_incoming = localizationDistances_incoming,
                          g_pi = g_pi,
                          classifier_incoming = classifier_incoming,
                          predicted_class = predicted_class)))
}

