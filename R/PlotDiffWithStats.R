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

PlotBoxVioliScatterMean<- function(data, colname = "NEW_BMI", SD_level = 4, seeSics = TRUE) {
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
