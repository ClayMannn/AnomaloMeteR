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
    theme(axis.text.x = element_text(angle = 00, vjust = 0.5, hjust = 1)) +
    guides(fill = FALSE)

  return(violin_plot)
}
