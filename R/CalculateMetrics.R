CalculateMetrics <- function(predictedProb , actualBin, model_binomial) {#
  library(caret)
  library(pROC)  # For roc calculation
  library(glmtoolbox)
  # predictedProb=predictions_test
  # actualBin= test_data$DERIVAT_DIABETES_DS_HISTORY
  # model_binomial=lasso_model_filtered
  predictedBin <- ifelse(predictedProb > 0.5, 1, 0)

  predicted <- factor(unlist(predictedBin), levels = c(0, 1))
  actual <- factor(unlist(actualBin), levels = c(0, 1))

  auc <- roc(actual, predictedProb)$auc

  pseudo_r_squared <- 1- (model_binomial$deviance/model_binomial$null.deviance)
  adj_pseudo_r_squared <- adjR2(model_binomial, digits = max(3, getOption("digits") - 2), verbose = TRUE)

  return(list("adj_pseudo_r_squared" = adj_pseudo_r_squared, "AUC" = auc))
}