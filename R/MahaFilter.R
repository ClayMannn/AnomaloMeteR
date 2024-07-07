
MahaFilter <- function(data, columns, treshold, filter = TRUE)
{
  imputator_vals <- colMeans(data[,columns], na.rm = TRUE)

  # Impute missing values with column means
  for (column in columns) {
    data[[column]][is.na(data[[column]])] <- imputator_vals[column]
  }


  library(caret)

  df_no_linear_combos <- data[, which(colnames(data) %in% columns)]

  linear_combos <- findLinearCombos(df_no_linear_combos)
  df_linear_combos <- df_no_linear_combos[, linear_combos$remove] # columns with linear combinations

  df_no_linear_combos <- df_no_linear_combos[, -linear_combos$remove] # remove columns with linear combinations

  #  BS_3D_WAISTBAND_F_HT BS_3D_WAISTBAND_B_HT
  #  BS_DIST_CROTCH_WAISTBAN BS_CROTCH_LTH_AT_WB_A


  # Calculate the covariance matrix
  covariance <- cov(df_no_linear_combos)

  dim(covariance)
  # Calculate squared Mahalanobis distance between data points and mean
  mahalanobis_dist <- mahalanobis(df_no_linear_combos, colMeans(df_no_linear_combos), covariance)

  #median(mahalanobis_dist)+ 101 * IQR(mahalanobis_dist)


  data$mahalanobis_dist <- mahalanobis_dist

  if(filter == TRUE){
    data <- data[-(which(data$mahalanobis_dist > treshold)),]
  }


  return(data)
}
