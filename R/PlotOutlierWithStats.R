PlotOutlierWithStats <- function(data, colname = "NEW_BMI", treshold = 4, seeSics = TRUE) {
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

  title = paste0("The Outliers" )[[1]]

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
