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
      plots[[i]] <- PlotBoxVioliScatterMean(plot_df, unlist(c(datasort$evaluation5))[[i]],number_of_sd,FALSE)
    }


    # Combine plots into a grid
    grid_plot <- do.call(grid.arrange, c(plots, ncol = 6))

    filename <- paste0("outliers_",number_of_sd, "meansd.png")

    ggsave(filename, grid_plot,  width = 18, height = 62, dpi = 300, limitsize = FALSE)
  }

  return(df)
}