prepare_data <- function(data, datasort, seed = 123, split_ratio = 0.7) {
  # HT scaling (assuming you might want to uncomment and use)
  # data[, unlist(c(datasort$evaluation5))] <- data[, unlist(c(datasort$evaluation5))] / data$BS_HT

  # Scaling selected columns
  # cols_to_scale <- c(datasort$evaluation5, "NEW_BMI", "NEW_WHT", "NEW_WTH", "ABSI")
  # data[, cols_to_scale] <- as.data.frame(scale(data[, cols_to_scale]))

  # Set the seed for reproducibility
  set.seed(seed)

  # Shuffle data and create train-test split
  num_rows <- nrow(data)
  index <- sample(num_rows)
  split_point <- round(split_ratio * num_rows)

  train_data <- data[index[1:split_point], ]
  test_data <- data[index[(split_point + 1):num_rows], ]

  # Define columns to keep
  columns_to_keep <- unlist(c("age", "SEX", datasort$evaluation5, "NEW_BMI", "NEW_WHT", "NEW_WTH", "ABSI", "DERIVAT_DIABETES_DS_HISTORY"))

  # Clean and subset training and test data
  train_data <- na.omit(train_data[, columns_to_keep])
  test_data <- na.omit(test_data[, columns_to_keep])

  # Prepare model matrices for predictors and response
  x_train <- model.matrix(~ . - 1, data = train_data[, -which(names(train_data) == "DERIVAT_DIABETES_DS_HISTORY")])
  y_train <- train_data$DERIVAT_DIABETES_DS_HISTORY

  x_test <- model.matrix(~ . - 1, data = test_data[, -which(names(test_data) == "DERIVAT_DIABETES_DS_HISTORY")])
  y_test <- test_data$DERIVAT_DIABETES_DS_HISTORY

  # Combine train and test data for full dataset analysis
  full_data <- na.omit(rbind(train_data, test_data))

  x_all <- model.matrix(~ . - 1, data = full_data[, -which(names(full_data) == "DERIVAT_DIABETES_DS_HISTORY")])
  y_all <- full_data$DERIVAT_DIABETES_DS_HISTORY

  # Return a list containing all relevant data structures
  return(list(
    train = list(x = x_train, y = y_train),
    test = list(x = x_test, y = y_test),
    all = list(x = x_all, y = y_all)
  ))
}

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

calculate_vip_scores <- function(model) {
  # Sicherstellen, dass das Modell die notwendigen Komponenten enthält
  if (!("loadings" %in% names(model)) || !("scores" %in% names(model))) {
    stop("Das Modell muss 'loadings' und 'scores' enthalten.")
  }

  # Ladungen extrahieren
  loadings <- model$loadings

  # Scores für die Berechnung der erklärten Varianz in jeder Komponente verwenden
  explained_variances <- colSums(model$scores^2) / sum(model$scores^2)

  # Berechnung der quadrierten Ladungen
  squared_loadings <- loadings^2

  # Gewichtung jeder Variable mit der erklärten Varianz jeder Komponente
  weighted_squared_loadings <- squared_loadings %*% explained_variances

  # Summierung der gewichteten quadrierten Ladungen für jede Variable
  sum_weighted_squared_loadings <- rowSums(weighted_squared_loadings)

  # Berechnung des VIP-Scores
  vip_scores <- sqrt(sum_weighted_squared_loadings * ncol(loadings) / sum(explained_variances))

  return(sort(vip_scores, decreasing=TRUE))
}


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
MahaPcaFilter <- function(data, datasort)
{
  library(caret)
  df_no_linear_combos <- data[, which(colnames(data) %in% c(datasort$pcaDim1NamesOver01 , datasort$pcaDim2NamesOver01))]

  df_no_linear_combos <- na.omit(df_no_linear_combos)

  # linear_combos <- findLinearCombos(data)
  #df_linear_combos <- data[, linear_combos$remove] # columns with linear combinations
  #  BS_3D_WAISTBAND_F_HT BS_3D_WAISTBAND_B_HT BS_DIST_CROTCH_WAISTBAN BS_CROTCH_LTH_AT_WB_A


  # Calculate the covariance matrix
  covariance <- cov(df_no_linear_combos)

  dim(covariance)
  # Calculate squared Mahalanobis distance between data points and mean
  mahalanobis_dist <- mahalanobis(df_no_linear_combos, colMeans(df_no_linear_combos), covariance)


  max_6iqr_grenze_maha <- median(mahalanobis_dist) + 6 * IQR(data$mahalanobis_dist)

  data <- data[-(which(mahalanobis_dist > max_6iqr_grenze_maha)),]

  return(data)
}

QuantileIqrFilter <- function(df, colnames, number_of_iqr) {
  # df=data
  # colnames = unlist(datasort$evaluation5)
  # number_of_iqr = 6

  life_Xiqr <- df[,colnames]

  iqr_vals <- sapply(life_Xiqr, IQR, na.rm = TRUE)
  q_vals <- sapply(life_Xiqr, quantile, na.rm = TRUE)

  low_q_vals <- q_vals[2,] # 25%
  up_q_vals <- q_vals[4,] # 75%

  minsXIQR <- low_q_vals - number_of_iqr * iqr_vals
  maxsXIQR <- up_q_vals + number_of_iqr * iqr_vals
  maxsXIQR["BS_HT"] <- 210

  # Initialize the outlier columns
  life_Xiqr$min_outlier <- 0
  life_Xiqr$max_outlier <- 0
  life_Xiqr$min_outlier_detail <- NA
  life_Xiqr$max_outlier_detail <- NA

  # Initialize the outlier columns
  df$min_outlier <- 0
  df$max_outlier <- 0
  df$min_outlier_detail <- NA
  df$max_outlier_detail <- NA

  pb = txtProgressBar(min = 0, max = nrow(life_Xiqr), initial = 0, style = 3)
  for(i in c(1:dim(life_Xiqr)[1])) {
    # i=1
    row <- life_Xiqr[i,]
    outlier_check <- check_outliers(row, minsXIQR, maxsXIQR)

    min_outlier <- as.numeric(outlier_check[1])
    max_outlier <- as.numeric(outlier_check[2])
    min_outlier_detail <- as.list(outlier_check[3])
    max_outlier_detail <- as.list(outlier_check[4])

    life_Xiqr[i,]$min_outlier <- min_outlier
    life_Xiqr[i,]$max_outlier <- max_outlier
    life_Xiqr[i,]$min_outlier_detail <- min_outlier_detail
    life_Xiqr[i,]$max_outlier_detail <- max_outlier_detail

    #set all outliers to NA
    life_Xiqr[i,unlist(min_outlier_detail)] <- NA
    life_Xiqr[i,unlist(max_outlier_detail)] <- NA

    setTxtProgressBar(pb, i, label = sprintf("%.1f%%", i/nrow(life_Xiqr)*100))
  }
  close(pb)

  df[,colnames] <- life_Xiqr[,colnames]

  df$min_outlier <- life_Xiqr$min_outlier
  df$max_outlier <- life_Xiqr$max_outlier
  df$min_outlier_detail <- life_Xiqr$min_outlier_detail
  df$max_outlier_detail <- life_Xiqr$max_outlier_detail

  return(df)
}

MedianIqrFilter <- function(df, colnames, number_of_iqr) {
  # df=PV0273_T00411_NODUP_Kopie
  # colnames = unlist(datasort$evaluation5)
  # number_of_iqr = 2

  life_Xiqr <- df[,colnames]

  median_vals <- sapply(life_Xiqr, median, na.rm = TRUE)
  iqr_vals <- sapply(life_Xiqr, IQR, na.rm = TRUE)

  minsXIQR <- median_vals - number_of_iqr * iqr_vals
  maxsXIQR <- median_vals + number_of_iqr * iqr_vals
  maxsXIQR["BS_HT"] <- 210

  # Initialize the outlier columns
  life_Xiqr$min_outlier <- 0
  life_Xiqr$max_outlier <- 0
  life_Xiqr$min_outlier_detail <- NA
  life_Xiqr$max_outlier_detail <- NA

  # Initialize the outlier columns
  df$min_outlier <- 0
  df$max_outlier <- 0
  df$min_outlier_detail <- NA
  df$max_outlier_detail <- NA

  pb = txtProgressBar(min = 0, max = nrow(life_Xiqr), initial = 0, style = 3)
  for(i in c(1:dim(life_Xiqr)[1])) {
    # i=1
    row <- life_Xiqr[i,]
    outlier_check <- check_outliers(row, minsXIQR, maxsXIQR)

    min_outlier <- as.numeric(outlier_check[1])
    max_outlier <- as.numeric(outlier_check[2])
    min_outlier_detail <- as.list(outlier_check[3])
    max_outlier_detail <- as.list(outlier_check[4])

    life_Xiqr[i,]$min_outlier <- min_outlier
    life_Xiqr[i,]$max_outlier <- max_outlier
    life_Xiqr[i,]$min_outlier_detail <- min_outlier_detail
    life_Xiqr[i,]$max_outlier_detail <- max_outlier_detail

    #set all outliers to NA
    life_Xiqr[i,unlist(min_outlier_detail)] <- NA
    life_Xiqr[i,unlist(max_outlier_detail)] <- NA

    setTxtProgressBar(pb, i, label = sprintf("%.1f%%", i/nrow(life_Xiqr)*100))
  }
  close(pb)

  df[,colnames] <- life_Xiqr[,colnames]

  df$min_outlier <- life_Xiqr$min_outlier
  df$max_outlier <- life_Xiqr$max_outlier
  df$min_outlier_detail <- life_Xiqr$min_outlier_detail
  df$max_outlier_detail <- life_Xiqr$max_outlier_detail

  return(df)
}

# Define a function to check for outliers
CheckOutliers <- function(row, mins, maxs)
{
  # mins <- min_vals
  # maxs <- max_val
  # row <- life[which(life$BS_WT == max(life$BS_WT, na.rm = TRUE)),unlist(datasortListeMitUnterlisten)]

  values <- row[names(row) %in% names(mins)]
  is_numeric <- sapply(values, is.double)
  is_outlier_mins <- ifelse(is_numeric & !is.na(values), values < mins[names(values)], FALSE)
  is_outlier_maxs <- ifelse(is_numeric & !is.na(values), values > maxs[names(values)], FALSE)
  return(
    c(
      min_outlier = sum(is_outlier_mins, na.rm = TRUE),
      max_outlier = sum(is_outlier_maxs, na.rm = TRUE),
      min_outlier_detail = list(names(row)[names(row) %in% names(mins)][is_outlier_mins]),
      max_outlier_detail = list(names(row)[names(row) %in% names(maxs)][is_outlier_maxs])
    )
  )
}

MeanSdFilter <- function(df, colnames, number_of_sd, detail_plot = FALSE) {

  # df= filtered_df
  # colnames= unlist(datasort$evaluation5)
  # number_of_sd=3
  library(gridExtra)
  plot_df <- df
  life_Xsd <- df[,colnames]

  mean_vals <- sapply(life_Xsd, mean, na.rm = TRUE)
  sd_vals <- sapply(life_Xsd, sd, na.rm = TRUE)

  minsXIQR <- mean_vals - number_of_sd * sd_vals
  maxsXIQR <- mean_vals + number_of_sd * sd_vals
  maxsXIQR["BS_HT"] <- 210

  # Initialize the outlier columns
  life_Xsd$min_outlier <- 0
  life_Xsd$max_outlier <- 0
  life_Xsd$min_outlier_detail <- NA
  life_Xsd$max_outlier_detail <- NA

  # Initialize the outlier columns
  df$min_outlier <- 0
  df$max_outlier <- 0
  df$min_outlier_detail <- NA
  df$max_outlier_detail <- NA

  pb = txtProgressBar(min = 0, max = nrow(life_Xsd), initial = 0, style = 3)
  for(i in c(1:dim(life_Xsd)[1])) {
    # i=1
    row <- life_Xsd[i,]
    outlier_check <- CheckOutliers(row, minsXIQR, maxsXIQR)

    min_outlier <- as.numeric(outlier_check[1])
    max_outlier <- as.numeric(outlier_check[2])
    min_outlier_detail <- as.list(outlier_check[3])
    max_outlier_detail <- as.list(outlier_check[4])

    life_Xsd[i,]$min_outlier <- min_outlier
    life_Xsd[i,]$max_outlier <- max_outlier
    life_Xsd[i,]$min_outlier_detail <- min_outlier_detail
    life_Xsd[i,]$max_outlier_detail <- max_outlier_detail

    # Debugging output
    print(paste("Row:", i))
    print(paste("min_outlier:", life_Xsd[i,]$min_outlier))
    print(paste("min_outlier_detail:", life_Xsd[i,]$min_outlier_detail))
    print(paste("max_outlier:", life_Xsd[i,]$max_outlier))
    print(paste("max_outlier_detail:", life_Xsd[i,]$max_outlier_detail))

    #set all outliers to NA
    life_Xsd[i,unlist(min_outlier_detail)] <- NA
    life_Xsd[i,unlist(max_outlier_detail)] <- NA

    setTxtProgressBar(pb, i, label = sprintf("%.1f%%", i/nrow(life_Xsd)*100))
  }
  close(pb)

  df[,colnames] <- life_Xsd[,colnames]

  df$min_outlier <- life_Xsd$min_outlier
  df$max_outlier <- life_Xsd$max_outlier
  df$min_outlier_detail <- life_Xsd$min_outlier_detail
  df$max_outlier_detail <- life_Xsd$max_outlier_detail

  if(detail_plot == TRUE){
    plots <- list()
    for (i in c(1:length(unlist(c(datasort$evaluation5))))) {
      plots[[i]] <- PlotBoxViolinScatterMean(plot_df, unlist(c(datasort$evaluation5))[[i]],number_of_sd,FALSE)
    }


    # Combine plots into a grid
    grid_plot <- do.call(grid.arrange, c(plots, ncol = 6))

    filename <- paste0("outliers_",number_of_sd, "meansd.png")

    ggsave(filename, grid_plot,  width = 18, height = 62, dpi = 300, limitsize = FALSE)
  }

  return(df)
}


NaFilter <- function(df, max_na, colnames, filter = TRUE) {
  df$BS_MISSINGS <- apply(df[unlist(colnames)], 1, function(x) sum(is.na(x)))
  if(filter){    df <- df[which(df$BS_MISSINGS <= max_na),]    }
  return(df)
}


SymetricViolinBoxPlot <- function(df_left, df_right, xAxisTitle, yAxisTitle, tTitle) {
  library(ggplot2)
  library(reshape2)
  # Melt the data frame to long format
  melted_data_left <- melt(df_left)

  # Get min and max values for each column
  min_vals <- apply(df_left, 2, min, na.rm = TRUE)
  max_vals <- apply(df_left, 2, max, na.rm = TRUE)
  means <- apply(df_left, 2, mean, na.rm = TRUE)
  sds <- apply(df_left, 2, sd, na.rm = TRUE)

  # Create a new data frame with the statistics for each column
  stats_data_left <- data.frame(
    variable = rep(colnames(df_left), each = 3),
    value = c(min_vals, means, max_vals),
    stat = rep(c("Min", "Mean", "Max"), times = ncol(df_left)),
    sd = rep(sds, each = 3)
  )

  # Melt the data frame to long format
  melted_data <- melt(df_right)

  # Get min and max values for each column
  min_vals <- apply(df_right, 2, min, na.rm = TRUE)
  max_vals <- apply(df_right, 2, max, na.rm = TRUE)
  means <- apply(df_right, 2, mean, na.rm = TRUE)
  sds <- apply(df_right, 2, sd, na.rm = TRUE)

  # Create a new data frame with the statistics for each column
  stats_data <- data.frame(
    variable = rep(colnames(df_right), each = 3),
    value = c(min_vals, means, max_vals),
    stat = rep(c("Min", "Mean", "Max"), times = ncol(df_right)),
    sd = rep(sds, each = 3)
  )

  # Create the plot
  violin_plot <-
    ggplot(melted_data, aes(x = variable, y = value, fill = variable)) +
    see::geom_violinhalf(data = melted_data_left, flip = TRUE) +
    see::geom_violinhalf() +
    scale_fill_manual(values = rep(c("skyblue", "orange", "green", "red", "purple", "yellow"), length(unique(melted_data$variable)))) +
    stat_summary(fun.y = mean, geom = "point", shape = 23, size = 1, color = "black") +
    stat_summary(fun.y = median, geom = "point", size = 2, color = "black") +
    geom_segment(data = stats_data_left[stats_data_left$stat %in% c("Min", "Max"), ], aes(x = variable, xend = variable, y = value, yend = value), size = 1, colour = "black") +
    geom_segment(data = stats_data[stats_data$stat %in% c("Min", "Max"), ], aes(x = variable, xend = variable, y = value, yend = value), size = 1, colour = "black") +
    labs(x = xAxisTitle, y = yAxisTitle, title = tTitle) +
    theme_bw() +
    theme(axis.text.x = element_blank()) +
    guides(fill = FALSE)

  return(violin_plot)
}

# Call the function with your data
#create_violin_plot(df_left, df_right, "sym diff circumfances", "[cm]", "title")


PlotSketchColor <-  function (df,            col = "gray80",            border = NULL,            main = "",  bodyxlab = "", bodyylab  = ""    ,     x.offset = 0,            leg = FALSE,            hip= FALSE,            torso= FALSE,            waist= FALSE,headneck= FALSE,            arm= FALSE,            shoulder= FALSE,            body= FALSE,            breastbust= FALSE,            bback= FALSE  )
{
  library(plotrix)
  library(ellipse)
  # library(ggplot2)
  # library(ggforce)
  if (is.null(leg)) {
    leg = FALSE
  }

  if (is.null(hip)) {
    hip = FALSE
  }

  if (is.null(torso)) {
    torso = FALSE
  }

  if (is.null(waist)) {
    waist = FALSE
  }

  if (is.null(headneck)) {
    headneck = FALSE
  }

  if (is.null(arm)) {
    arm = FALSE
  }

  if (is.null(shoulder)) {
    shoulder = FALSE
  }

  if (is.null(body)) {
    body = FALSE
  }

  if (is.null(breastbust)) {
    breastbust = FALSE
  }

  if (is.null(bback)) {
    bback = FALSE
  }




  if(leg | hip | torso | waist | headneck | arm | shoulder | body | breastbust | bback){givenoutliers <-  c( "outliers found in:")}

  height = df$BS_HT

  calf.length = df["BS_KNEE_HT"]
  calf.width.R = df$BS_CALF_GTH_R / pi
  calf.width.L = df$BS_CALF_GTH_L / pi
  calf.width = (calf.width.R  + calf.width.L )/2
  thigh.length = df["BS_HIP_HT"] - df["BS_KNEE_HT"]
  thigh.width.R = df$BS_THIGH_GTH_L_HZ / pi
  thigh.width.L = df$BS_THIGH_GTH_R_HZ / pi
  hip.width = df["BS_HIP_GTH"] / pi
  thigh.width = (thigh.width.R + thigh.width.L) /2

  chest.width = df["BS_BUST_CHEST_GTH"] / pi
  belly.width = df["BS_MAX_BELLY_CIRC"] / pi

  head.height = df["BS_HEAD_HT"]
  head.width = df["BS_HEAD_CIRC"] / pi

  up.arm.length.R = df$BS_UP_ARM_LTH_L
  up.arm.length.L = df$BS_UP_ARM_LTH_R
  up.arm.length = (up.arm.length.R +up.arm.length.L ) /2
  up.arm.width.R = df$BS_UP_ARM_DIAM_L
  up.arm.width.L = df$BS_UP_ARM_DIAM_R
  up.arm.width = (up.arm.width.R + up.arm.width.L) /2
  forearm.length.R =  df$BS_FOREARM_LTH_R
  forearm.length.L =  df$BS_FOREARM_LTH_L
  forearm.length = (forearm.length.R + forearm.length.L) / 2
  forearm.width.R = df$BS_FOREARM_GTH_R / pi
  forearm.width.L = df$BS_FOREARM_GTH_L / pi
  forearm.width = (forearm.width.R + forearm.width.L) / 2


  shoulder.height = df["BS_NECK_HT"]
  shoulder.angle.R = df$BS_SHOULDER_ANGLE_R
  shoulder.angle.L = df$BS_SHOULDER_ANGLE_L
  shoulder.angle = (shoulder.angle.R + shoulder.angle.L) /2
  #if it is multi dimuension it makes mean
  height <- mean(height, na.rm=TRUE)
  calf.length <- mean(calf.length$BS_KNEE_HT, na.rm=TRUE)
  calf.width.R <- mean(calf.width.R, na.rm=TRUE)
  calf.width.L <- mean(calf.width.L, na.rm=TRUE)
  thigh.length <- mean(thigh.length$BS_HIP_HT, na.rm=TRUE)
  thigh.width.R <- mean(thigh.width.R, na.rm=TRUE)
  thigh.width.L <- mean(thigh.width.L, na.rm=TRUE)
  hip.width <- mean(hip.width$BS_HIP_GTH, na.rm=TRUE)
  chest.width <- mean(chest.width$BS_BUST_CHEST_GTH, na.rm=TRUE)
  belly.width <- mean(belly.width$BS_MAX_BELLY_CIRC, na.rm=TRUE)
  head.height <- mean(head.height$BS_HEAD_HT, na.rm=TRUE)
  head.width <- mean(head.width$BS_HEAD_CIRC, na.rm=TRUE)
  up.arm.length.R <- mean(up.arm.length.R, na.rm=TRUE)
  up.arm.length.L <- mean(up.arm.length.L, na.rm=TRUE)
  up.arm.width.R <- mean(up.arm.width.R, na.rm=TRUE)
  up.arm.width.L <- mean(up.arm.width.L, na.rm=TRUE)
  forearm.length.R <- mean(forearm.length.R, na.rm=TRUE)
  forearm.length.L <- mean(forearm.length.L, na.rm=TRUE)
  forearm.width.R <- mean(forearm.width.R, na.rm=TRUE)
  forearm.width.L <- mean(forearm.width.L, na.rm=TRUE)
  shoulder.height <- mean(shoulder.height$BS_NECK_HT, na.rm=TRUE)
  shoulder.angle.R <- mean(shoulder.angle.R, na.rm=TRUE)
  shoulder.angle.L <- mean(shoulder.angle.L, na.rm=TRUE)

  ##################################################################
  #col = "gray80"
  col_leg = "gray80"
  if(leg){col_leg = "red"
  givenoutliers <- append(givenoutliers, "leg")}
  col_hip  = "gray80"
  if(hip){col_hip = "red"
  givenoutliers <- append(givenoutliers, "hip")}
  col_torso  = "gray80"
  if(torso){col_torso = "red"
  givenoutliers <- append(givenoutliers, "torso")}
  col_waist  = "gray80"
  if(waist){col_waist = "red"
  givenoutliers <- append(givenoutliers, "waist")}
  col_headneck = "gray80"
  if(headneck){col_headneck = "red"
  givenoutliers <- append(givenoutliers, "headneck")}
  col_arm = "gray80"
  if(arm){col_arm = "red"
  givenoutliers <- append(givenoutliers, "arm")}
  col_shoulder = "gray80"
  if(shoulder){col_shoulder = "red"
  givenoutliers <- append(givenoutliers, "shoulder")}
  if(breastbust){ col_torso = "red"
  givenoutliers <- append(givenoutliers, "breastbust")}
  if(body){
    col_leg = "red"
    col_hip = "red"
    col_torso = "red"
    col_waist = "red"
    col_headneck = "red"
    col_shoulder = "red"
    col_torso = "red"
    col_arm = "red"
    givenoutliers <- append(givenoutliers, "body")}

  if(bback){
    col_leg = "red"
    col_hip = "red"
    col_torso = "red"
    col_waist = "red"
    col_headneck = "red"
    col_shoulder = "red"
    col_torso = "red"
    col_arm = "red"
    givenoutliers <- append(givenoutliers, "bback")}


  border = NULL
  x.offset = 0
  height = mean(height)
  calf.length = mean(calf.length)
  #calf.width = mean(calf.width)
  thigh.length = mean(thigh.length)
  #thigh.width = mean(thigh.width)
  hip.width = mean(hip.width)
  chest.width = mean(chest.width)
  belly.width = mean(belly.width)
  head.height = mean(head.height)
  head.width = mean(head.width)
  #up.arm.length = mean(up.arm.length)
  #up.arm.width = mean(up.arm.width)
  #forearm.length =  mean(forearm.length)
  #forearm.width = mean(forearm.width)
  shoulder.height = mean(shoulder.height)
  #shoulder.angle = mean(shoulder.angle)
  #

  # }


  calf.width = (calf.width.R  + calf.width.L )/2

  thigh.width = (thigh.width.R + thigh.width.L) /2

  up.arm.length = (up.arm.length.R +up.arm.length.L ) /2

  up.arm.width = (up.arm.width.R + up.arm.width.L) /2

  forearm.length = (forearm.length.R + forearm.length.L) / 2

  forearm.width = (forearm.width.R + forearm.width.L) / 2

  shoulder.angle = (shoulder.angle.R + shoulder.angle.L) /2



  plot(
    c(0, 200),
    c(0, 220),
    type = "n",
    xlab = bodyxlab,
    ylab = bodyylab,
    axes = F,
    main = main,
    yaxs = "i"
  )



  if(leg | hip | torso | waist | headneck | arm | shoulder | body | breastbust | bback){ legend(0,220,givenoutliers, col=list(222,333,333,333,333,333,333,333))}
  box()
  # box coordinates

  max.y.calf = calf.length

  angle.thigh = 5
  dy.thigh = abs(sin(2 * pi / 360 * (90 - angle.thigh)) * thigh.length)
  dx.thigh = abs(cos(2 * pi / 360 * (90 - angle.thigh)) * thigh.length)
  max.y.thigh = calf.length + dy.thigh - 3

  angle.up.arm = 40
  dy.up.arm = abs(sin(2 * pi / 360 * (90 - angle.up.arm)) * up.arm.length.R)
  dx.up.arm = abs(cos(2 * pi / 360 * (90 - angle.up.arm)) * up.arm.length.L)
  min.y.up.arm = shoulder.height - dy.up.arm - 3

  angle.forearm = 5
  dy.forearm = abs(sin(2 * pi / 360 * (90 - angle.forearm)) * forearm.length.R)
  dx.forearm = abs(cos(2 * pi / 360 * (90 - angle.forearm)) * forearm.length.L)


  # calfs Waden

  draw.ellipse(
    90 - dx.thigh / 1.8 + x.offset,
    calf.length / 1.8,
    calf.width.R / 1.8,
    calf.length / 1.8,
    angle = 0,
    col = col_leg,
    border = border
  )
  draw.ellipse(
    110 + dx.thigh / 1.8 + x.offset,
    calf.length / 1.8,
    calf.width.L / 1.8,
    calf.length / 1.8,
    angle = 0,
    col = col_leg,
    border = border
  )


  # hip

  draw.ellipse(
    100 + x.offset,
    max.y.thigh + (shoulder.height - max.y.thigh) / 12,
    hip.width / 2,
    (shoulder.height - max.y.thigh) / 6,
    col = col_hip,
    border = border
  )



  # thighs Oberschenkel

  draw.ellipse(
    90 + x.offset,
    max.y.calf + dy.thigh / 2,
    thigh.width.R / 2,
    thigh.length / 2,
    angle = -angle.thigh,
    col = col_waist,
    border = border
  )
  draw.ellipse(
    110 + x.offset,
    max.y.calf + dy.thigh / 2,
    thigh.width.L / 2,
    thigh.length / 2,
    angle = angle.thigh,
    col = col_waist,
    border = border
  )



  # torso

  draw.ellipse(
    100 + x.offset,
    shoulder.height - (shoulder.height - max.y.thigh) / 3,
    chest.width / 2,
    (shoulder.height - max.y.thigh) / 3,
    col = col_torso,
    border = border
  )
  draw.ellipse(
    100 + x.offset,
    max.y.thigh + (shoulder.height - max.y.thigh) / 3,
    belly.width / 2,
    (shoulder.height - max.y.thigh) / 3,
    col = col_torso,
    border = border
  )





  # head

  draw.ellipse(
    100 + x.offset,
    height - head.height / 2 + 2,
    head.width / 2,
    head.height / 2,
    col = col_headneck,
    border = border
  )



  # upper arms

  draw.ellipse(
    100 - chest.width * 0.45 - dx.up.arm / 2 + x.offset,
    shoulder.height - dy.up.arm / 2 - 3,
    up.arm.width.R / 2,
    up.arm.length.R / 2,
    angle = -angle.up.arm,
    col = col_arm,
    border = border
  )
  draw.ellipse(
    100 + chest.width * 0.45 + dx.up.arm / 2 + x.offset,
    shoulder.height - dy.up.arm / 2 - 3,
    up.arm.width.L / 2,
    up.arm.length.L / 2,
    angle = angle.up.arm,
    col = col_arm,
    border = border
  )



  # forearms

  draw.ellipse(
    100 - chest.width * 0.45 - dx.up.arm + x.offset,
    min.y.up.arm - dy.forearm / 2,
    forearm.width.R / 2,
    forearm.length.R / 2,
    angle = -angle.forearm,
    col = col_arm,
    border = border
  )
  draw.ellipse(
    100 + chest.width * 0.45 + dx.up.arm + x.offset,
    min.y.up.arm - dy.forearm / 2,
    forearm.width.L / 2,
    forearm.length.L / 2,
    angle = angle.forearm,
    col = col_arm,
    border = border
  )



  # hands

  draw.ellipse(
    100 - chest.width * 0.45 - dx.up.arm - dx.forearm / 2 + x.offset,
    min.y.up.arm - dy.forearm - 1,
    forearm.width.R / 2,
    5,
    angle = 0,
    col = col_arm,
    border = border
  )
  draw.ellipse(
    100 + chest.width * 0.45 + dx.up.arm + dx.forearm / 2 + x.offset,
    min.y.up.arm - dy.forearm - 1,
    forearm.width.L / 2,
    5,
    angle = 0,
    col = col_arm,
    border = border
  )



  # shoulders

  draw.ellipse(
    100 - chest.width * 0.4 + x.offset,
    shoulder.height - 3,
    1,
    10,
    col = col_shoulder,
    border = border,
    angle = -90 + shoulder.angle
  )
  draw.ellipse(
    100 + chest.width * 0.4 + x.offset,
    shoulder.height - 3,
    1,
    10,
    col = col_shoulder,
    border = border,
    angle = 90 - shoulder.angle
  )


  #  p <- ggplot() +
  #          xlim(0, 200) + ylim(0, 220) +  # set the plot limits
  #          labs(x = bodyxlab, y = bodyylab, title = main) +  # Set labels and title
  #          theme(axis.title.x = element_blank(),
  #                axis.text.x = element_blank(),
  #                axis.ticks.x = element_blank(),
  #                axis.title.y = element_blank(),
  #                axis.text.y = element_blank(),
  #                axis.ticks.y = element_blank())  +   # Remove axes
  #           # calfs Waden
  #   geom_ellipse(aes(x0 = 90 - dx.thigh / 1.8 + x.offset,
  #                          y0 = calf.length / 1.8,
  #                          a = calf.width.R / 3.6,
  #                          b = calf.length / 3.6,
  #                          angle = 0),
  #                      color = col_leg) +
  #    geom_ellipse(aes(x0 = 110 + dx.thigh / 1.8 + x.offset,
  #                          y0 = calf.length / 1.8,
  #                          a = calf.width.L / 3.6,
  #                          b = calf.length / 3.6,
  #                          angle = 0),
  #                      color = col_leg) +
  #                      # hip
  #   geom_ellipse(aes(x0 = 100 + x.offset,
  #                      y0 = max.y.thigh + (shoulder.height - max.y.thigh) / 12,
  #                      a = hip.width / 3.6,
  #                      b = (shoulder.height - max.y.thigh) / 6,
  #                      angle = 0),
  #                  color = col_hip) +
  #      # thighs Oberschenkel
  #  geom_ellipse(aes(x0 = 90 + x.offset,
  #                      y0 = max.y.calf + dy.thigh / 2,
  #                      a = thigh.width.R / 3.6,
  #                      b = thigh.length / 3.6,
  #                      angle = -angle.thigh),
  #                  color = col_waist) +
  #    geom_ellipse(aes(x0 = 110 + x.offset,
  #                      y0 = max.y.calf + dy.thigh / 2,
  #                      a = thigh.width.L / 3.6,
  #                      b = thigh.length / 3.6,
  #                      angle = angle.thigh),
  #                  color = col_waist)+
  #     # torso
  #     geom_ellipse(aes(x0 = 100 + x.offset,
  #                      y0 = max.y.thigh + (shoulder.height - max.y.thigh) / 3,
  #                      a = chest.width / 3.6,
  #                      b = (shoulder.height - max.y.thigh) / 3,
  #                      angle = 0),
  #                  color = col_torso) +
  #    geom_ellipse(aes(x0 = 100 + x.offset,
  #                      y0 = max.y.thigh + (shoulder.height - max.y.thigh) / 3,
  #                      a = belly.width / 3.6,
  #                      b = (shoulder.height - max.y.thigh) / 3,
  #                      angle = 0),
  #                  color = col_torso)+
  #     # head
  #     geom_ellipse(aes(x0 = 100 + x.offset,
  #                      y0 = height - head.height / 2 + 2,
  #                      a = head.width / 3.6,
  #                      b = head.height / 3.6,
  #                      angle = 0),
  #                  color = col_headneck)+
  #     # upper arms
  #     geom_ellipse(aes(x0 = 100 - chest.width * 0.45 - dx.up.arm / 2 + x.offset,
  #                      y0 = shoulder.height - dy.up.arm / 2 - 3,
  #                      a = up.arm.width.R / 3.6,
  #                      b = up.arm.length.R / 3.6,
  #                      angle = -angle.up.arm),
  #                  color = col_arm) +
  #    geom_ellipse(aes(x0 = 100 + chest.width * 0.45 + dx.up.arm / 2 + x.offset,
  #                      y0 = shoulder.height - dy.up.arm / 2 - 3,
  #                      a = up.arm.width.L / 3.6,
  #                      b = up.arm.length.L / 3.6,
  #                      angle = angle.up.arm),
  #                  color = col_arm) +
  #         # forearms
  #    geom_ellipse(aes(x0 = 100 - chest.width * 0.45 - dx.up.arm + x.offset,
  #                      y0 = min.y.up.arm - dy.forearm / 2,
  #                      a = forearm.width.R / 3.6,
  #                      b = forearm.length.R / 3.6,
  #                      angle = -angle.forearm),
  #                  color = col_arm) +
  #    geom_ellipse(aes(x0 = 100 + chest.width * 0.45 + dx.up.arm + x.offset,
  #                      y0 = min.y.up.arm - dy.forearm / 2,
  #                      a = forearm.width.L / 3.6,
  #                      b = forearm.length.L / 3.6,
  #                      angle = angle.forearm),
  #                  color = col_arm)+
  #   # hands
  #  geom_ellipse(aes(x0 = 100 - chest.width * 0.45 - dx.up.arm - dx.forearm / 2 + x.offset,
  #                      y0 = min.y.up.arm - dy.forearm - 1,
  #                      a = forearm.width.R / 3.6,
  #                      b = 5,
  #                      angle = 0),
  #                  color = col_arm) +
  #    geom_ellipse(aes(x0 = 100 + chest.width * 0.45 + dx.up.arm + dx.forearm / 2 + x.offset,
  #                      y0 = min.y.up.arm - dy.forearm - 1,
  #                      a = forearm.width.L / 3.6,
  #                      b = 5,
  #                      angle = 0),
  #                  color = col_arm)+
  #     # shoulders
  #     geom_ellipse(aes(x0 = 100 - chest.width * 0.4 + x.offset,
  #                      y0 = shoulder.height - 3,
  #                      a = 1,
  #                      b = 10,
  #                      angle = -90 + shoulder.angle),
  #                  color = col_shoulder) +
  #    geom_ellipse(aes(x0 = 100 + chest.width * 0.4 + x.offset,
  #                      y0 = shoulder.height - 3,
  #                      a = 1,
  #                      b = 10,
  #                      angle = 90 - shoulder.angle),
  #                  color = col_shoulder)
  #     return(p)
}



library(ggplot2)
library(gridExtra)

# Create a function to generate boxplots using ggplot
CreateBoxplot <- function(data, predictor, title, y_min_lim, y_max_lim) {
  l <- length(data[[predictor]])
  ytitle <- "cm"
  if(predictor == 'age')
  {ytitle <- "years"}
  if(predictor == 'BS_WT')
  {ytitle <- "kg"}

  ggplot(data, aes(x = "", y = data[[predictor]])) +
    geom_boxplot() +
    labs(title = title,x = paste("n = ",l),y = ytitle) +
    theme_minimal() +
    ylim(y_min_lim, y_max_lim) +
    theme(axis.text.x = element_blank())
}

remove_prefix <- function(data) {
  data <- sub("^BS_", "", data)
  data <- sub("^NEW_", "", data)
  return(data)
}


#
# #see boxplot for regession.r
# BoxplotDiabetesMF <- function(data, plot_predictors) {
#
#
# data <- data[which(data$DERIVAT_DIABETES_DS_HISTORY > -1),]
#
# data_no_diabetes <- data[which(data$DERIVAT_DIABETES_DS_HISTORY == 0),]
# data_diabetes <- data[which(data$DERIVAT_DIABETES_DS_HISTORY == 1),]
#
# data_no_diabetes_f <- data_no_diabetes[which(data_no_diabetes$SEX == 2),]
# data_no_diabetes_m <- data_no_diabetes[which(data_no_diabetes$SEX == 1),]
# data_diabetes_f <- data_diabetes[which(data_diabetes$SEX == 2),]
# data_diabetes_m <- data_diabetes[which(data_diabetes$SEX == 1),]
#
# plots <- list()
#
# # Use ggplot to create boxplots
# for(i in 1:length(plot_predictors)){
#
#   p_colname <- plot_predictors[i]
#   y_min_lim <- min(data_diabetes_m[p_colname],
#                    data_no_diabetes_m[p_colname],
#                    data_diabetes_f[p_colname],
#                    data_no_diabetes_f[p_colname],
#                    data_no_diabetes[p_colname],
#                    data_diabetes[p_colname],
#                    na.rm = TRUE)
#   y_max_lim <- max(data_diabetes_m[p_colname],
#                    data_no_diabetes_m[p_colname],
#                    data_diabetes_f[p_colname],
#                    data_no_diabetes_f[p_colname],
#                    data_no_diabetes[p_colname],
#                    data_diabetes[p_colname],
#                    na.rm = TRUE)
#
#   cleaned_p_colname <- remove_prefix(p_colname)
#
#   # diabetes M
#   plot1 <- create_boxplot(data_diabetes_m, p_colname, paste(cleaned_p_colname, "_D_m", sep = ''), y_min_lim, y_max_lim)
#
#   # No diabetes M
#   plot4 <- create_boxplot(data_no_diabetes_m, p_colname, paste(cleaned_p_colname, "_nD_m", sep = ''), y_min_lim, y_max_lim)
#
#   # diabetes F
#   plot2 <- create_boxplot(data_diabetes_f, p_colname, paste(cleaned_p_colname, "_D_f", sep = ''), y_min_lim, y_max_lim)
#
#   # No diabetes F
#   plot3 <- create_boxplot(data_no_diabetes_f, p_colname, paste(cleaned_p_colname, "_nD_f", sep = ''), y_min_lim, y_max_lim)
#
#   # No diabetes
#   plot6 <- create_boxplot(data_no_diabetes, p_colname, paste(cleaned_p_colname, "_nD", sep = ''), y_min_lim, y_max_lim)
#
#   #  diabetes
#   plot5 <- create_boxplot(data_diabetes, p_colname, paste(cleaned_p_colname, "_D", sep = ''), y_min_lim, y_max_lim)
#
#
#
#
#   plots[[i]] <- grid.arrange(plot1 , plot4, plot2 , plot3 ,plot5,plot6, nrow = 1, ncol=6)
# }
# calced_plots <- list()
# for (i in seq_along(plot_predictors)) {
#   calced_plots[[i]] <- plots[[i]]
# }
#
# # Display the combined plot
# combined_plot <- grid.arrange(grobs = eval(calced_plots), nrow = length(plot_predictors), ncol = 1)
# return (combined_plot)
# }


PlotOutlierComparison <- function(data_file, colname,subsetname,i, numberOfIqr) {
  library(ggstatsplot)
  # Load the data from the specified file
  data <- data_file

  # Select the desired columns
  selected_data <- data[, c(subsetname, colname)]

  # Remove rows with missing values
  selected_data <- na.omit(selected_data)


  # Create a boxplot of the dataset with outlier tagging
  unfiltered_plot <- ggbetweenstats(selected_data, x = !!subsetname, y = !!colname, outlier.tagging = TRUE)
  unfiltered_plot
  filename <- paste0("unfiltered_plot_", i,colname,"_numberOfIQR_",numberOfIqr, ".png")
  ggsave(filename, width = 6, height = 4, dpi = 300)

  # Calculate the upper and lower range for outlier elimination
  Q <- quantile(selected_data[[colname]], probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- IQR(selected_data[[colname]], na.rm = TRUE)
  up <- Q[2] + numberOfIqr * iqr
  low <- Q[1] - numberOfIqr * iqr

  # Subset the data by eliminating outliers
  eliminated <- subset(selected_data, selected_data[[colname]] > low & selected_data[[colname]] < up)
  outlier <- subset(selected_data, selected_data[[colname]] < low | selected_data[[colname]] > up)

  # Create a boxplot of the eliminated data with outlier tagging
  filtered_plot <- ggbetweenstats(eliminated, x = !!subsetname, y = !!colname, outlier.tagging = TRUE)
  filtered_plot
  filename <- paste0("filtered_plot_", i,colname,"_numberOfIQR_",numberOfIqr, ".png")
  ggsave(filename, width = 6, height = 4, dpi = 300)
  # Arrange the plots in a grid
  grid_arranged <- grid.arrange(unfiltered_plot, filtered_plot, nrow = 1)

  # Print the grid
  grid_arranged
}

#plot_outlier_comparison(selected_data, predictors[i], "DERIVAT_DIABETES_DS_HISTORY",i, 4)

#plot_outlier_iqr_with_stats(data, "BS_WT", 4)

PlotBoxViolinScatterMean <- function(data, colname = "NEW_BMI", SD_level = 4, seeSics = TRUE) {
  library(ggplot2)
  library(dplyr)
  library(ggrepel )
  # Create a sample dataset
  data_plot <- data.frame(
    Value = unlist(data[,colname])
  )

  # Calculate the count of individuals for each group
  count_data <- data_plot %>%
    summarise(count = n())

  # Create a sample dataset
  data_plot <- data.frame(
    Value = unlist(data[,colname]),
    BS_SIC = data$BS_SIC
  )


  jitterer <- position_jitter(width = .4,seed = 123) #comically large jitter

  # Identify outliers
  outliers <- data_plot %>%
    filter(Value > mean(Value, na.rm = TRUE) + SD_level * sd(Value, na.rm = TRUE) | Value < mean(Value,  na.rm = TRUE) - SD_level * IQR(Value, na.rm = TRUE))

  # Ensure outliers$Value contains only positive indices
  positive_indices <- outliers$Value[outliers$Value > 0]

  # Access BS_SIC values using these positive indices
  outliers_BS_SICs <- list(data$BS_SIC[positive_indices])

  print(outliers_BS_SICs)
  # Identify outliers
  not_outliers <- data_plot %>%
    filter(Value < mean(Value, na.rm = TRUE) + SD_level * sd(Value, na.rm = TRUE) & Value > mean(Value,  na.rm = TRUE) - SD_level * IQR(Value, na.rm = TRUE))

  title = paste0("Ausreißer mit (sd=", SD_level, ")" )[[1]]

  # Create the combined plot
  combined_plot <- ggplot(not_outliers , aes(x = colname, y = Value)) +
    # Violin plot
    geom_violin(fill = "lightblue", alpha = 0.7) +
    # Box plot
    geom_boxplot(width = 0.3, fill = "white", color = "black", outlier.shape = NA) +
    geom_point(data = not_outliers, position = jitterer, color = "green", size = 2,  alpha = 0.2) +
    # Scatter plot
    geom_point(data = outliers, position = jitterer, color = "red", size = 3, alpha = 1)   +
    # Customize labels and title
    xlab("") +
    ylab("") +
    ggtitle(title) +
    theme(legend.position="none")
  #add the BS_SIC as text on all outliers
  if (seeSics == TRUE) {
    combined_plot <- combined_plot + geom_label_repel(data = outliers, aes(label = BS_SIC),
                                                      size = 4,
                                                      box.padding = 1.5,
                                                      point.padding = 0.5,
                                                      force = 100,
                                                      position = jitterer,
                                                      segment.size = 0.2,
                                                      segment.color = "#000000",
                                                      max.overlaps = Inf)
  }


  # Set color palette for the groups
  # combined_plot + scale_color_manual(values = c("Group A" = "red", "Group B" = "blue"))
  # Add outliers to the plot
  return(
    combined_plot

  )
}

PlotBoxViolinScatter <- function(data, colname = "NEW_BMI", iqr_level = 4, seeSics = TRUE) {
  library(ggplot2)
  library(dplyr)
  library(ggrepel )
  # Create a sample dataset
  data_plot <- data.frame(
    Value = unlist(data[,colname])
  )

  # Calculate the count of individuals for each group
  count_data <- data_plot %>%
    summarise(count = n())

  # Create a sample dataset
  data_plot <- data.frame(
    Value = unlist(data[,colname]),
    BS_SIC = data$BS_SIC
  )


  jitterer <- position_jitter(width = .4,seed = 123) #comically large jitter

  # Identify outliers
  outliers <- data_plot %>%
    filter(Value > quantile(Value, 0.75, na.rm = TRUE) + iqr_level * IQR(Value, na.rm = TRUE) | Value < quantile(Value, 0.25, na.rm = TRUE) - iqr_level * IQR(Value, na.rm = TRUE))

  # Ensure outliers$Value contains only positive indices
  positive_indices <- outliers$Value[outliers$Value > 0]

  # Access BS_SIC values using these positive indices
  outliers_BS_SICs <- list(data$BS_SIC[positive_indices])

  print(outliers_BS_SICs)
  # Identify outliers
  not_outliers <- data_plot %>%
    filter(Value < quantile(Value, 0.75, na.rm = TRUE) + iqr_level * IQR(Value, na.rm = TRUE) & Value > quantile(Value, 0.25, na.rm = TRUE) - iqr_level * IQR(Value, na.rm = TRUE))

  title = paste0("The Outliers with quantile filter (iqr=", iqr_level, ")" )[[1]]

  # Create the combined plot
  combined_plot <- ggplot(not_outliers , aes(x = colname, y = Value)) +
    # Violin plot
    geom_violin(fill = "lightblue", alpha = 0.7) +
    # Box plot
    geom_boxplot(width = 0.3, fill = "white", color = "black", outlier.shape = NA) +
    geom_point(data = not_outliers, position = jitterer, color = "green", size = 2,  alpha = 0.2) +
    # Scatter plot
    geom_point(data = outliers, position = jitterer, color = "red", size = 3, alpha = 1)   +
    # Customize labels and title
    xlab("") +
    ylab("") +
    ggtitle(title) +
    theme(legend.position="none")
  #add the BS_SIC as text on all outliers
  if (seeSics == TRUE) {
    combined_plot <- combined_plot + geom_label_repel(data = outliers, aes(label = BS_SIC),
                                                      size = 4,
                                                      box.padding = 1.5,
                                                      point.padding = 0.5,
                                                      force = 100,
                                                      position = jitterer,
                                                      segment.size = 0.2,
                                                      segment.color = "#000000",
                                                      max.overlaps = Inf)
  }


  # Set color palette for the groups
  # combined_plot + scale_color_manual(values = c("Group A" = "red", "Group B" = "blue"))
  # Add outliers to the plot
  return(
    combined_plot

  )
}

PlotOutlierIqrWithStats <- function(data, colname = "NEW_BMI", iqr_level = 4, seeSics = TRUE) {
  library(ggplot2)
  library(dplyr)
  library(ggrepel )
  # Create a sample dataset
  data_plot <- data.frame(
    Group = factor(data$DERIVAT_DIABETES_DS_HISTORY, levels = c(0, 1)),
    Value = unlist(data[,colname])
  )

  # Calculate the count of individuals for each group
  count_data <- data_plot %>%
    group_by(Group) %>%
    summarise(count = n())

  # Create a sample dataset
  data_plot <- data.frame(
    Group = factor(data$DERIVAT_DIABETES_DS_HISTORY, levels = c(0, 1), labels = c(paste("n=", count_data$count[1]), paste("n=", count_data$count[2]))),
    Value = unlist(data[,colname]),
    BS_SIC = data$BS_SIC
  )


  jitterer <- position_jitter(width = .4,seed = 123) #comically large jitter

  # Identify outliers
  outliers <- data_plot %>%
    filter(Value > quantile(Value, 0.75, na.rm = TRUE) + iqr_level * IQR(Value, na.rm = TRUE) | Value < quantile(Value, 0.25, na.rm = TRUE) - iqr_level * IQR(Value, na.rm = TRUE))

  # Identify the BS_SICs of the outliers
  outliers_BS_SICs <- list(data$BS_SIC[outliers$Value])
  print(outliers_BS_SICs)
  # Identify outliers
  not_outliers <- data_plot %>%
    filter(Value < quantile(Value, 0.75, na.rm = TRUE) + iqr_level * IQR(Value, na.rm = TRUE) & Value > quantile(Value, 0.25, na.rm = TRUE) - iqr_level * IQR(Value, na.rm = TRUE))

  title = paste0("The Outliers with quantile filter (iqr=", iqr_level, ")" )[[1]]

  # Create the combined plot
  combined_plot <- ggplot(not_outliers , aes(x = Group, y = Value)) +
    # Violin plot
    geom_violin(fill = "lightblue", alpha = 0.7) +
    # Box plot
    geom_boxplot(width = 0.3, fill = "white", color = "black", outlier.shape = NA) +
    geom_point(data = not_outliers, position = jitterer, color="blue", size = 2,  alpha = 0.2) +
    # Scatter plot
    geom_point(data = outliers, position = jitterer, color="red", size = 3, alpha = 1)   +
    # Customize labels and title
    xlab("diabetes") +
    ylab(colname) +
    ggtitle(title) +
    theme(legend.position="none")
  #add the BS_SIC as text on all outliers
  if (seeSics == TRUE) {
    combined_plot <- combined_plot + geom_label_repel(data = outliers, aes(label = BS_SIC),
                                                      size = 4,
                                                      box.padding = 1.5,
                                                      point.padding = 0.5,
                                                      force = 100,
                                                      position = jitterer,
                                                      segment.size = 0.2,
                                                      segment.color = "#000000",
                                                      max.overlaps = Inf)
  }


  # Set color palette for the groups
  # combined_plot + scale_color_manual(values = c("Group A" = "red", "Group B" = "blue"))
  # Add outliers to the plot
  return(
    combined_plot

  )
}
PlotOutlierWithStats <- function(data, colname = "NEW_BMI", treshold = 4, seeSics = TRUE, title = "") {
  library(ggplot2)
  library(dplyr)
  library(ggrepel )
  # Create a sample dataset
  data_plot <- data.frame(
    Group = factor(data$DERIVAT_DIABETES_DS_HISTORY, levels = c(0, 1)),
    Value = unlist(data[,colname])
  )

  # Calculate the count of individuals for each group
  count_data <- data_plot %>%
    group_by(Group) %>%
    summarise(count = n())

  # Create a sample dataset
  data_plot <- data.frame(
    Group = factor(data$DERIVAT_DIABETES_DS_HISTORY, levels = c(0, 1), labels = c(paste("n=", count_data$count[1]), paste("n=", count_data$count[2]))),
    Value = unlist(data[,colname]),
    BS_SIC = data$BS_SIC
  )


  jitterer <- position_jitter(width = .4,seed = 123) #comically large jitter

  # Identify outliers
  outliers <- data_plot %>%
    filter(Value > treshold)

  # Identify the BS_SICs of the outliers
  outliers_BS_SICs <- list(data$BS_SIC[outliers$Value])
  print(outliers_BS_SICs)
  # Identify outliers
  not_outliers <- data_plot %>%
    filter(Value < treshold)



  # Create the combined plot
  combined_plot <- ggplot(not_outliers , aes(x = Group, y = Value)) +
    # Violin plot
    geom_violin(fill = "lightblue", alpha = 0.7) +
    # Box plot
    geom_boxplot(width = 0.3, fill = "white", color = "black", outlier.shape = NA) +
    geom_point(data = not_outliers, position = jitterer, color="blue", size = 2,  alpha = 0.2) +
    # Scatter plot
    geom_point(data = outliers, position = jitterer, color="red", size = 3, alpha = 0.5)   +
    # Customize labels and title
    xlab("diabetes") +
    ylab(colname) +
    ggtitle(title) +
    theme(legend.position="none")
  #add the BS_SIC as text on all outliers
  if (seeSics == TRUE) {
    combined_plot <- combined_plot + geom_label_repel(data = outliers, aes(label = BS_SIC),
                                                      size = 4,
                                                      box.padding = 1.5,
                                                      point.padding = 0.5,
                                                      force = 100,
                                                      position = jitterer,
                                                      segment.size = 0.2,
                                                      segment.color = "#000000",
                                                      max.overlaps = Inf)
  }


  # Set color palette for the groups
  # combined_plot + scale_color_manual(values = c("Group A" = "red", "Group B" = "blue"))
  # Add outliers to the plot
  return(
    combined_plot

  )
}

PlotDiffWithStats <- function(data, otherData, colname = "NEW_BMI", title, seeSics = TRUE) {
  library(ggplot2)
  library(dplyr)
  library(ggrepel)

  # Create a sample dataset
  data_plot <- data.frame(
    Group = factor(data$DERIVAT_DIABETES_DS_HISTORY, levels = c(0, 1)),
    Value = unlist(data[,colname])
  )

  # Calculate the count of individuals for each group
  count_data <- data_plot %>%
    group_by(Group) %>%
    summarise(count = n())

  # Create a sample dataset
  data_plot <- data.frame(
    Group = factor(data$DERIVAT_DIABETES_DS_HISTORY, levels = c(0, 1), labels = c(paste("n=", count_data$count[1]), paste("n=", count_data$count[2]))),
    Value = unlist(data[,colname]),
    BS_SIC = data$BS_SIC
  )

  jitterer <- position_jitter(width = .4, seed = 123) #comically large jitter

  # Identify outliers
  outliers <- data_plot[-which(data$BS_SIC %in% otherData$BS_SIC),]

  # Identify not outliers
  not_outliers <- data_plot[which(data$BS_SIC %in% otherData$BS_SIC),]

  # Create the combined plot
  combined_plot <- ggplot(not_outliers, aes(x = Group, y = Value)) +
    # Violin plot
    geom_violin(fill = "lightblue", alpha = 0.7) +
    # Box plot
    geom_boxplot(width = 0.3, fill = "white", color = "black", outlier.shape = NA) +
    geom_point(data = not_outliers, position = jitterer, aes(color = Group), size = 1, alpha = 0.2) +
    # Scatter plot for outliers with black color
    geom_point(data = outliers, position = jitterer, color = "black", size = 1, alpha = 1, stroke = 1) +
    # Customize labels and title
    xlab("diabetes") +
    ylab(colname) +
    ggtitle(title) +
    theme(legend.position = "none")

  if (seeSics == TRUE) {
    combined_plot <- combined_plot + geom_label_repel(data = outliers, aes(label = BS_SIC),
                                                      size = 4,
                                                      box.padding = 1.5,
                                                      point.padding = 0.5,
                                                      force = 100,
                                                      position = jitterer,
                                                      segment.size = 0.2,
                                                      segment.color = "#000000",
                                                      max.overlaps = Inf)
  }

  return(combined_plot)
}

PlotBoxViolinScatterMean<- function(data, colname = "NEW_BMI", SD_level = 4, seeSics = TRUE) {
  library(ggplot2)
  library(dplyr)
  library(ggrepel )
  # Create a sample dataset
  data_plot <- data.frame(
    Value = unlist(data[,colname])
  )

  # Calculate the count of individuals for each group
  count_data <- data_plot %>%
    summarise(count = n())

  # Create a sample dataset
  data_plot <- data.frame(
    Value = unlist(data[,colname]),
    BS_SIC = data$BS_SIC
  )


  jitterer <- position_jitter(width = .4,seed = 3) #comically large jitter

  # Identify outliers
  outliers <- data_plot %>%
    filter(Value >= mean(Value, na.rm = TRUE) + SD_level * sd(Value, na.rm = TRUE) | Value <= mean(Value,  na.rm = TRUE) - SD_level * sd(Value, na.rm = TRUE))

  # Ensure outliers$Value contains only positive indices
  positive_indices <- outliers$Value[outliers$Value > 0]

  # Access BS_SIC values using these positive indices
  outliers_BS_SICs <- list(data$BS_SIC[positive_indices])

  print(outliers_BS_SICs)
  # Identify not outliers
  not_outliers <- data_plot %>%
    filter(Value < mean(Value, na.rm = TRUE) + SD_level * sd(Value, na.rm = TRUE) & Value > mean(Value,  na.rm = TRUE) - SD_level * sd(Value, na.rm = TRUE))

  title = paste0("Ausreißer mit (sd=", SD_level, ")" )[[1]]

  # Create the combined plot
  combined_plot <- ggplot(data_plot , aes(x = colname, y = Value)) +
    # Violin plot
    geom_violin(fill = "lightblue", alpha = 0.7) +
    # Box plot
    geom_boxplot(width = 0.3, fill = "white", color = "black", outlier.shape = NA) +
    geom_point(data = not_outliers, position = jitterer, color = "green", size = 2,  alpha = 0.2) +
    # Scatter plot
    geom_point(data = outliers, position = jitterer, color = "red", size = 3, alpha = 1)   +
    # Customize labels and title
    xlab("") +
    ylab("") +
    ggtitle(title) +
    theme(legend.position="none")
  #add the BS_SIC as text on all outliers
  if (seeSics == TRUE) {
    combined_plot <- combined_plot + geom_label_repel(data = outliers, aes(label = BS_SIC),
                                                      size = 4,
                                                      box.padding = 1.5,
                                                      point.padding = 0.5,
                                                      force = 100,
                                                      position = jitterer,
                                                      segment.size = 0.2,
                                                      segment.color = "#000000",
                                                      max.overlaps = Inf)
  }


  # Set color palette for the groups
  # combined_plot + scale_color_manual(values = c("Group A" = "red", "Group B" = "blue"))
  # Add outliers to the plot
  return(
    combined_plot

  )
}


PlotComparison <- function(data_file, filtered_data, colname,subsetname) {
  library(ggstatsplot)
  # Load the data from the specified file
  data <- data_file

  # Select the desired columns
  selected_data <- data[, c(subsetname, colname)]

  # Remove rows with missing values
  #selected_data <- na.omit(selected_data)


  # Create a boxplot of the dataset with outlier tagging
  unfiltered_plot <- ggbetweenstats(selected_data, x = !!subsetname, y = !!colname,   conf.level = 0.95 ,outlier.tagging = FALSE)
  unfiltered_plot
  filename <- paste0("unfiltered_plot_",colname, ".png")
  ggsave(filename, width = 6, height = 4, dpi = 300)

  # Select the desired columns
  filtered_data <- filtered_data[, c(subsetname, colname)]

  # Remove rows with missing values
  #filtered_data <- na.omit(selected_data)



  # Create a boxplot of the eliminated data with outlier tagging
  filtered_plot <- ggbetweenstats(filtered_data, x = !!subsetname, y = !!colname,   conf.level = 0.99,outlier.tagging = FALSE)
  filtered_plot
  filename <- paste0("filtered_plot_",colname, ".png")
  ggsave(filename, width = 6, height = 4, dpi = 300)
  # Arrange the plots in a grid
  grid_arranged <- grid.arrange(unfiltered_plot, filtered_plot, nrow = 1)

  # Print the grid
  grid_arranged
}







PlotBoxViolinDiabetes <- function(data, colname = "NEW_BMI") {

  library(ggplot2)
  library(dplyr)
  library(ggrepel )
  # Create a sample dataset
  data_plot <- data.frame(
    Group = factor(data$DERIVAT_DIABETES_DS_HISTORY, levels = c(0, 1)),
    Value = unlist(data[,colname])
  )

  # Calculate the count of individuals for each group
  count_data <- data_plot %>%
    group_by(Group) %>%
    summarise(count = n())

  # Create a sample dataset
  data_plot <- data.frame(
    Group = factor(data$DERIVAT_DIABETES_DS_HISTORY, levels = c(0, 1), labels = c(paste("n=", count_data$count[1]), paste("n=", count_data$count[2]))),
    Value = unlist(data[,colname]),
    BS_SIC = data$BS_SIC
  )



  # Create the combined plot
  combined_plot <- ggplot(data_plot , aes(x = Group, y = Value)) +
    # Violin plot
    geom_violin(fill = "lightblue", alpha = 0.7) +
    # Box plot
    geom_boxplot(width = 0.3, fill = "white", color = "black", outlier.shape = NA) +

    xlab("diabetes") +
    ylab(colname) +
    theme(legend.position="none")
  #add the BS_SIC as text on all outliers



  # Set color palette for the groups
  combined_plot + scale_color_manual(values = c("Group A" = "red", "Group B" = "blue"))
  # Add outliers to the plot
  return(
    combined_plot

  )
}


PlotBoxViolin <- function(data, colname = "NEW_BMI") {

  library(ggplot2)
  library(dplyr)
  library(ggrepel )
  # Create a sample dataset
  data_plot <- data.frame(
    Value = unlist(data[,colname])
  )

  # Calculate the count of individuals for each group
  count_data <- data_plot %>%
    summarise(count = n())

  # Create a sample dataset
  data_plot <- data.frame(
    Value = unlist(data[,colname]),
    BS_SIC = data$BS_SIC
  )



  # Create the combined plot
  combined_plot <- ggplot(data_plot , aes(x = colname, y = Value)) +
    # Violin plot
    geom_violin(fill = "lightblue", alpha = 0.7) +
    # Box plot
    geom_boxplot(width = 0.3, fill = "white", color = "black", outlier.shape = NA) +

    xlab("") +
    ylab("") +
    theme(legend.position="none")

  return(
    combined_plot

  )
}


ExtractNumericValues <- function(df, col_name, range = c(5, 2000)) {
  library(stringr)

  # Extract numeric values from the specified column
  numeric_values <- str_extract(df[[col_name]], "[0-9]+([.,][0-9]+)?")

  # Replace commas with periods for decimal points
  numeric_values <- gsub(",", ".", numeric_values)

  # Convert the numeric values to numeric type
  numeric_values <- as.numeric(numeric_values)

  # # DELETE non-numeric values with NA
  # numeric_values[is.na(numeric_values)] <- NA

  # Replace values outside the specified range with NA
  numeric_values[numeric_values < range[1] | numeric_values > range[2]] <- NA

  # Create a new data frame with the numeric values column
  new_df <- data.frame(df, numeric_values = numeric_values)

  # Return the new data frame
  return(new_df)
}
ExtractCommentValues <- function(diabetis_life, comment_df) {

  df <- comment_df[, c("SIC", "COMMENTS")]
  kg_comments <- df[which(grepl("kg|KG|Kg|gewicht|Gewicht", df$COMMENTS)),]

  kg_comments <- ExtractNumericValues(kg_comments, "COMMENTS")

  l_wt_bevore <- length(na.omit(diabetis_life$BS_WT))

  BS_WT_before <- diabetis_life$BS_WT[which(diabetis_life$BS_SIC %in% kg_comments$SIC)]

  for (i in 1:nrow(kg_comments)) {
    if (!is.na(kg_comments$numeric_values[i])) {
      diabetis_life$BS_WT[which(diabetis_life$BS_SIC == kg_comments$SIC[i])] <- kg_comments$numeric_values[i]
    }
  }

  diabetis_life$SEX <- diabetis_life$R00001_PBD_GESCHLECHT

  BS_WT_after <- diabetis_life$BS_WT[which(diabetis_life$BS_SIC %in% kg_comments$SIC)]
  l_wt_after <- length(na.omit(diabetis_life$BS_WT))

  length(na.omit(BS_WT_after)) - length(na.omit(BS_WT_before))

  # Convert the birth year/month column to a date format
  diabetis_life$birth_date <- as.Date(paste0(diabetis_life$R00001_PBD_GEBJAHRMON, "01"), format = "%Y%m%d")

  # Calculate the age in years based on the birth date and the audit creation date
  diabetis_life$age <- interval(diabetis_life$birth_date, diabetis_life$R00001_AUDIT_CRE_AT) / years(1)

  diabetis_life$age <- round(diabetis_life$age,0)

  #neu ausrechnen
  diabetis_life$NEW_BMI <- diabetis_life$BS_WT / ((diabetis_life$BS_HT*diabetis_life$BS_HT)/10000)
  #rm BMI over 200 (it can't be)
  diabetis_life$NEW_BMI[which(diabetis_life$NEW_BMI > 200)] <- NA
  length(na.omit(diabetis_life$BS_BMI)) - length(na.omit(diabetis_life$NEW_BMI))

  diabetis_life$NEW_ABSI <- diabetis_life$BS_HIGH_WAIST_GTH/((diabetis_life$NEW_BMI ^ 2/3) * (diabetis_life$BS_HT ^ 1/2))

  #WTH indem man den Bauchumfang$BS_high_waist_gth in Zentimeter durch den Hüftumfang/BS_buttock_gth  in Zentimeter teilt siehe Reliability_of_3D_laser-based_anthropometry_and_co (2021_02_02 20_17_29 UTC)

  diabetis_life$NEW_WTH <- (diabetis_life$BS_HIGH_WAIST_GTH/diabetis_life$BS_BUTTOCK_GTH)

  diabetis_life$NEW_WHT <- (diabetis_life$BS_HIGH_WAIST_GTH/diabetis_life$BS_HT)

  length(which(is.na(diabetis_life$BS_WTH))) -
    length(which(is.na(diabetis_life$NEW_WTH)))
  #diff bmis 15 gerettet

  length(which(is.na(diabetis_life$BS_WTH))) -
    length(which(is.na(diabetis_life$NEW_WTH)))
  #diff wth 14 gerettet

  return(diabetis_life)
}
# CalculateMetrics <- function(predictedProb , actualBin) {#
#   library(caret)
#   library(pROC)    # For roc
#   # Simuliertes Beispiel mit Modellvorhersagen und tatsächlichen Werten
#   predictedBin <- ifelse(predictedProb > 0.5, 1, 0)
#
#   predicted <- factor(unlist(predictedBin), levels = c(0, 1))
#   actual <- factor(unlist(actualBin), levels = c(0, 1))
#
#   # Erstellen einer Konfusionsmatrix
#   conf_matrix <- confusionMatrix(predicted, actual)
#
#   # Berechnung der Genauigkeit
#   accuracy <- conf_matrix$overall["Accuracy"]
#   #cat("Genauigkeit (Accuracy):", accuracy, "\n")
#
#   # Berechnung der Präzision (Precision)
#   precision <- conf_matrix$byClass["Pos Pred Value"]
#   #cat("Präzision (Precision):", precision, "\n")
#
#   # Berechnung des Recall (Sensitivität)
#   recall <- conf_matrix$byClass["Sensitivity"]
#   #cat("Recall (Sensitivität):", recall, "\n")
#
#   # Berechnung des F1-Scores
#   f1_score <- 2 * (precision * recall) / (precision + recall)
#   #cat("F1-Score:", f1_score, "\n")
#
#   # Calculate R2 (Coefficient of Determination)
#   actual_num <- as.numeric(as.character(actual))
#   predicted_num <- as.numeric(as.character(predicted))
#   ss_total <- sum((actual_num - mean(actual_num))^2)
#   ss_residual <- sum((actual_num - predicted_num)^2)
#
#   r_squared <- 1 - (ss_residual / ss_total)
#   RMSE <- sqrt(ss_residual / length(actual))
#
#   auc <- roc(actual_num, predictedProb)$auc
#
#
#   tp <- sum(predicted == 1 & actual == 1)  # True Positives
#   tn <- sum(predicted == 0 & actual == 0)  # True Negatives
#   fp <- sum(predicted == 1 & actual == 0)  # False Positives
#   fn <- sum(predicted == 0 & actual == 1)  # False Negatives
#
#   # Calculate odds ratio manually
#   odds_ratio <- (tp / fn) / (fp / tn)
#
#   return(list("F1-Score" = f1_score, "Recall" = recall , "Precision" = precision, "Accuracy" = accuracy,"R^2" = r_squared, "odds_ratio" = odds_ratio, "RMSE" = RMSE, "AUC" = auc))
# }

