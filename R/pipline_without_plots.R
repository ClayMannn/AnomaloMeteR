list.of.packages <- c("glmtoolbox", "glmnet","caret","mdatools","factoextra","corrplot","ggplot2", "readxl","openxlsx", "lubridate", "openxlsx", "reshape2", "gridExtra", "pROC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(readxl)
library(caret)
library(lubridate)
library(openxlsx) #write xlsx
library(ggplot2)
library(reshape2)
library(gridExtra)
library(pROC)
library(corrplot)
library(glmnet) #lasso
library(factoextra)
library(mdatools)#plsda

# library(AnomaloMeteR)
# source(AnomaloMeteR::AnomaloMeteR('R', 'AllFunctions.R'))


cmds <- parse("R/AllFunctions.R")
assign.funs <- sapply(cmds, function(x) {
  if(x[[1]]=="<-") {
    if(x[[3]][[1]]=="function") {
      return(TRUE)
    }
  }
  return(FALSE)
})
eval(cmds[assign.funs])

load("data/datasort.RData")

diabetis_life <- read_excel("data/.xlsx")
comment_df <- read_excel("data/PV0273_T00048.xlsx")

dim(diabetis_life)

# remove useless prefix
colnames(comment_df)[which(colnames(comment_df) == "S010062_SIC")] <- "SIC"
colnames(comment_df)[which(colnames(comment_df) == "S010062_F0011")] <- "COMMENTS"


#write.xlsx(diabetis_life, "diabetis_life_NODUP.xlsx")

diabetis_life <- ExtractCommentValues(diabetis_life, comment_df)



#na filter______________________________________________________________________________________________________________________________________

data <- diabetis_life[which(diabetis_life$DERIVAT_DIABETES_DS_HISTORY > -1),]
filtered_df <- NaFilter(data,27,unlist(datasort$evaluation5))

maha_filtered_df <- filtered_df
#mean sd filter_____________________________________________________________________________________________________________________

mean_filtered_df_5sig <- MeanSdFilter(maha_filtered_df,unlist(datasort$evaluation5),5,FALSE )

mean_filtered_df <- mean_filtered_df_5sig
anzahl_na_gesetzte <- sum(mean_filtered_df_5sig$max_outlier + mean_filtered_df_5sig$min_outlier)

mean_filtered_df$sum_outliers <- mean_filtered_df_5sig$max_outlier + mean_filtered_df_5sig$min_outlier

anzahl_na <- sum(maha_filtered_df$BS_MISSINGS)
anzahl_messwerte <- dim(maha_filtered_df[,unlist(datasort$evaluation5)])[1] * dim(maha_filtered_df[,unlist(datasort$evaluation5)])[2]
anzahl_davor <- anzahl_messwerte - anzahl_na
100/anzahl_davor*anzahl_na_gesetzte

# a_5_maha_filtered_df <- MahaFilter(filtered_df, unlist(datasort$evaluation5), 4973.349)
# sehrFehleranf5Indiv <- mean_filtered_df_5sig[-which(filtered_df$BS_SIC %in% a_5_maha_filtered_df$BS_SIC),unlist(c("min_outlier", "max_outlier", "min_outlier_detail", "max_outlier_detail"))]
# sehrFehleranf5Indiv$max_outlier_detail
#1. LASSO mit allen
## Erforderliches Paket laden
# The first step to build a lasso model is to find the optimal lambda value using the code below. For lasso regression, the alpha value is 1. The output is the best cross-validated lambda, which comes out to be 0.001.
training_data <- mean_filtered_df

summary(mean_filtered_df$BS_CROTCH_LTH_R)
boxplot(mean_filtered_df$BS_CROTCH_LTH_R ~ mean_filtered_df$DERIVAT_DIABETES_DS_HISTORY)
#training_data$NEW_ABSI
#training_data$NEW_ABSI <- training_data$NEW_ABSI*100 # hat einfluss auf die schätzungen bei glm!

#/HT
#training_data[,unlist(c(datasort$evaluation5))] <- training_data[,unlist(c(datasort$evaluation5))] / training_data$BS_HT

#scale
training_data[,unlist(c(datasort$evaluation5,"NEW_BMI", "NEW_WHT", "NEW_WTH", "NEW_ABSI"))] <- as.data.frame(scale(training_data[,unlist(c(datasort$evaluation5,"NEW_BMI", "NEW_WHT", "NEW_WTH", "NEW_ABSI"))]))

# Standardisierung der Daten
#training_data <- scale(training_data[,unlist(c(datasort$evaluation5,"NEW_BMI", "NEW_WHT", "NEW_WTH", "NEW_ABSI"))])  # Z-Score Standardisierung

# Normalisierung der standardisierten Daten (optional, abhängig von der Anwendung)
#normalized_standardized_data <- (standardized_data - min(standardized_data, na.rm = TRUE)) / (max(standardized_data, na.rm = TRUE) - min(standardized_data, na.rm = TRUE))


# Ausgabe der verarbeiteten Daten
#training_data[,unlist(c(datasort$evaluation5,"NEW_BMI", "NEW_WHT", "NEW_WTH", "NEW_ABSI"))] <- normalized_standardized_data

# Calculate number of rows and determine the training set index
num_rows <- nrow(training_data)
index <- sample(num_rows)

# Define split point for a 70%-30% train-test split
split_point <- round(0.7 * num_rows)

# Subset data based on index
train_data <- training_data[index[1:split_point], ]
test_data <- training_data[index[(split_point + 1):num_rows], ]

# Define columns to keep
columns_to_keep <- unlist(c("age", "SEX", datasort$evaluation5, "NEW_BMI", "NEW_WHT", "NEW_WTH", "NEW_ABSI", "DERIVAT_DIABETES_DS_HISTORY"))

# Clean and subset training and test data
train_data <- na.omit(train_data[, columns_to_keep])
test_data <- na.omit(test_data[, columns_to_keep])

# Prepare model matrices for predictors (excluding first dummy variable to avoid dummy variable trap)
x_train <- model.matrix(~ . - 1, data = train_data[, -which(names(train_data) == "DERIVAT_DIABETES_DS_HISTORY")])
y_train <- train_data$DERIVAT_DIABETES_DS_HISTORY

x_test <- model.matrix(~ . - 1, data = test_data[, -which(names(test_data) == "DERIVAT_DIABETES_DS_HISTORY")])
y_test <- test_data$DERIVAT_DIABETES_DS_HISTORY

# Combine train and test data for full dataset analysis
full_data <- na.omit(rbind(train_data, test_data))

# Create full dataset model matrices
x_all <- model.matrix(~ . - 1, data = full_data[, -which(names(full_data) == "DERIVAT_DIABETES_DS_HISTORY")])
y_all <- full_data$DERIVAT_DIABETES_DS_HISTORY


variables_standart <- c("NEW_BMI", "SEX",  "age")
# build the logistic regression model
standart_model_filtered<- glm(DERIVAT_DIABETES_DS_HISTORY ~ .,
                              data = full_data[, c("DERIVAT_DIABETES_DS_HISTORY", variables_standart)],
                              family = binomial)

predictions_test <- predict(standart_model_filtered, newdata = full_data, type = "response")

calculated_metrics_standart <- CalculateMetrics(predictions_test, full_data$DERIVAT_DIABETES_DS_HISTORY, standart_model_filtered)

standart_model_filtered_summary <- summary(standart_model_filtered)

# Convert the named vector to a data frame
pvalue_metrics_table <- data.frame(
  Koeffizienten = names(which(standart_model_filtered_summary$coefficients[,4] <= 0.05)),
  "Pr(>|z|)" = standart_model_filtered_summary$coefficients[which(standart_model_filtered_summary$coefficients[,4] <= 0.05),4],
  Schätzer = standart_model_filtered_summary$coefficients[which(standart_model_filtered_summary$coefficients[,4] <= 0.05),1],
  "Std. Fehler" = standart_model_filtered_summary$coefficients[which(standart_model_filtered_summary$coefficients[,4] <= 0.05),2],
  "odds" = exp(standart_model_filtered_summary$coefficients[which(standart_model_filtered_summary$coefficients[,4] <= 0.05),4])
)
# Print the table
print(pvalue_metrics_table)
# Optionally, save the table to a CSV file
write.xlsx(pvalue_metrics_table, file = "calculated_pvalue_score_standart.xlsx", rowNames = FALSE)




variables_standart2 <- c("NEW_WTH", "SEX",  "age")
# build the logistic regression model
standart2_model_filtered<- glm(DERIVAT_DIABETES_DS_HISTORY ~ .,
                               data = full_data[, c("DERIVAT_DIABETES_DS_HISTORY", variables_standart2)],
                               family = binomial)

predictions_test <- predict(standart2_model_filtered, newdata = full_data, type = "response")

calculated_metrics_standart2 <- CalculateMetrics(predictions_test, full_data$DERIVAT_DIABETES_DS_HISTORY, standart_model_filtered)

standart_model_filtered_summary2 <- summary(standart2_model_filtered)

# Convert the named vector to a data frame
pvalue_metrics_table <- data.frame(
  Koeffizienten = names(which(standart_model_filtered_summary2$coefficients[,4] <= 0.05)),
  "Pr(>|z|)" = standart_model_filtered_summary2$coefficients[which(standart_model_filtered_summary2$coefficients[,4] <= 0.05),4],
  Schätzer = standart_model_filtered_summary2$coefficients[which(standart_model_filtered_summary2$coefficients[,4] <= 0.05),1],
  "Std. Fehler" = standart_model_filtered_summary2$coefficients[which(standart_model_filtered_summary2$coefficients[,4] <= 0.05),2],
  "odds" = exp(standart_model_filtered_summary2$coefficients[which(standart_model_filtered_summary2$coefficients[,4] <= 0.05),4])
)
# Print the table
print(pvalue_metrics_table)
# Optionally, save the table to a CSV file
write.xlsx(pvalue_metrics_table, file = "calculated_pvalue_score_wth.xlsx", rowNames = FALSE)


variables_standart3 <- c("NEW_WTH","NEW_BMI", "SEX",  "age")
# build the logistic regression model
standart3_model_filtered<- glm(DERIVAT_DIABETES_DS_HISTORY ~ .,
                               data = full_data[, c("DERIVAT_DIABETES_DS_HISTORY", variables_standart3)],
                               family = binomial)

predictions_test <- predict(standart3_model_filtered, newdata = full_data, type = "response")

calculated_metrics_standart3 <- CalculateMetrics(predictions_test, full_data$DERIVAT_DIABETES_DS_HISTORY, standart_model_filtered)

standart_model_filtered_summary3 <- summary(standart3_model_filtered)

# Convert the named vector to a data frame
pvalue_metrics_table <- data.frame(
  Koeffizienten = names(which(standart_model_filtered_summary2$coefficients[,4] <= 0.05)),
  "Pr(>|z|)" = standart_model_filtered_summary2$coefficients[which(standart_model_filtered_summary2$coefficients[,4] <= 0.05),4],
  Schätzer = standart_model_filtered_summary2$coefficients[which(standart_model_filtered_summary2$coefficients[,4] <= 0.05),1],
  "Std. Fehler" = standart_model_filtered_summary2$coefficients[which(standart_model_filtered_summary2$coefficients[,4] <= 0.05),2],
  "odds" = exp(standart_model_filtered_summary2$coefficients[which(standart_model_filtered_summary2$coefficients[,4] <= 0.05),4])
)
# Print the table
print(pvalue_metrics_table)
# Optionally, save the table to a CSV file
write.xlsx(pvalue_metrics_table, file = "calculated_pvalue_score_wth.xlsx", rowNames = FALSE)

# not necessary because sex and age gets also involved without forcing
# fixed_vaariables <- rep(1, times = ncol(x_all))
# fixed_vaariables[1:2] <- 0

# Setting alpha = 1 implements lasso regression
# smales best cross-validated lambda
#lasso_reg <- cv.glmnet(x_all, y_all, alpha = 1, nfolds = 10, penalty.factor=fixed_vaariables, family = "binomial")
lasso_reg <- cv.glmnet(x_all, y_all, alpha = 1, nfolds = 10,  family = "binomial")

# Best
lambda_best <- lasso_reg$lambda.min
plot(lasso_reg)

all_models <- glmnet(x_all, y_all, alpha = 1, standardize = TRUE, family = "binomial",  standardize.response=TRUE)

## Modell für die Lasso-Regression anpassen
#lasso_model <- glmnet(x_all, y_all, alpha = 1, lambda = lambda_best, standardize = TRUE, penalty.factor=fixed_vaariables, family = "binomial")

#Ohne Intercept konzentriert sich das Modell vollständig auf die Beziehungen zwischen den Prädiktoren und der Zielvariable, ohne einen konstanten Term zu berücksichtigen.
#Vermeidung von Bias: In bestimmten Datensätzen könnte die Einbeziehung eines Intercepts zu einem Bias in den Schätzungen führen, besonders wenn der wahre Datenprozess keinen Intercept beinhaltet. Durch das Setzen des Intercepts auf FALSE kann man solchen Bias vermeiden.
lasso_model <- glmnet(x_all, y_all, alpha = 1, lambda = lambda_best, standardize = TRUE, family = "binomial", standardize.response=TRUE, intercept = FALSE)

#lasso_model$beta
# . herausnehmen
#
o <- order(abs(coef(lasso_model)), decreasing = TRUE)

coef(lasso_model)[o,]
# Convert the named vector to a data frame
lasso_metrics_table <- data.frame(
  Name = names(coef(lasso_model)[o,]),
  Koeffizient = as.numeric(coef(lasso_model)[o,])
)
# Print the table
head(lasso_metrics_table,20)
# Optionally, save the table to a CSV file
write.xlsx(lasso_metrics_table, file = "calculated_LASSO.xlsx", rowNames = FALSE)

#remove"(Intercept)
lasso_metrics_tableNames = lasso_metrics_table$Name
lasso_metrics_tableValues = lasso_metrics_table$Koeffizient


variables <- lasso_metrics_tableNames[which(lasso_metrics_tableValues != 0)]
# Build the logistic regression model
lasso_model_filtered<- glm(DERIVAT_DIABETES_DS_HISTORY ~ .,
                           data = full_data[, c("DERIVAT_DIABETES_DS_HISTORY", variables)],
                           family = binomial)
lasso_model_filtered_summary <- summary(lasso_model_filtered)

# Convert the named vector to a data frame
pvalue_metrics_table <- data.frame(
  Koeffizienten = names(which(lasso_model_filtered_summary$coefficients[,4] <= 0.05)),
  "Pr(>|z|)" = lasso_model_filtered_summary$coefficients[which(lasso_model_filtered_summary$coefficients[,4] <= 0.05),4],
  Schätzer = lasso_model_filtered_summary$coefficients[which(lasso_model_filtered_summary$coefficients[,4] <= 0.05),1],
  "Std. Fehler" = lasso_model_filtered_summary$coefficients[which(lasso_model_filtered_summary$coefficients[,4] <= 0.05),2],
  "odds" = exp(lasso_model_filtered_summary$coefficients[which(lasso_model_filtered_summary$coefficients[,4] <= 0.05),1])
)
# Print the table
print(pvalue_metrics_table)
# Optionally, save the table to a CSV file
write.xlsx(pvalue_metrics_table, file = "calculated_pvalue_score_lasso.xlsx", rowNames = FALSE)



variables_lasso <- names(which(lasso_model_filtered_summary$coefficients[,4] <= 0.05))
if ( "(Intercept)" %in% variables_lasso )
{
  variables_lasso <- variables_lasso[-which(variables_lasso == "(Intercept)" )]
}
predictions_test <- predict(lasso_model_filtered, newdata = full_data, type = "response")

calculated_metrics_lasso <- CalculateMetrics(predictions_test, full_data$DERIVAT_DIABETES_DS_HISTORY, lasso_model_filtered)
calculated_metrics_lasso$AUC



model_plsda_results = mdatools::plsda(as.matrix(x_all), as.factor(y_all), cv = 10, scale = TRUE)
summary(model_plsda_results)
model_plsda_results$coeffs$values

vip_scores <- mdatools::vipscores(model_plsda_results)


sorted_vip_scores <- sort(vip_scores[,1], decreasing = TRUE)


# Convert the named vector to a data frame
plsda_metrics_table <- data.frame(
  name = names(sorted_vip_scores),
  VIP = as.numeric(sorted_vip_scores)
)
# Print the table
print(plsda_metrics_table)
# Optionally, save the table to a CSV file
write.xlsx(plsda_metrics_table, file = "calculated_VIP_score_pls.xlsx", rowNames = FALSE)


variables_plsda <- plsda_metrics_table$name[1:length(which(plsda_metrics_table$VIP > 1))]

# build the logistic regression model
plsda_model_filtered<- glm(DERIVAT_DIABETES_DS_HISTORY ~ .,
                           data = full_data[, c("DERIVAT_DIABETES_DS_HISTORY", variables_plsda)],
                           family = binomial)

predictions_plsda <- predict(plsda_model_filtered, newdata = full_data, type = "response")

calculated_metrics_plsda <- CalculateMetrics(predictions_plsda, y_all, plsda_model_filtered)
calculated_metrics_plsda
plsda_model_filtered_summary <- summary(plsda_model_filtered)

variables_plsda <- names(which(plsda_model_filtered_summary$coefficients[,4] <= 0.05))
if ( "(Intercept)" %in% variables_plsda )
{
  variables_plsda <- variables_plsda[-which(variables_plsda=="(Intercept)" )]
}
# Convert the named vector to a data frame
pvalue_metrics_table <- data.frame(
  Koeffizienten = names(which(plsda_model_filtered_summary$coefficients[,4] <= 0.05)),
  "Pr(>|z|)" = plsda_model_filtered_summary$coefficients[which(plsda_model_filtered_summary$coefficients[,4] <= 0.05),4],
  Schätzer = plsda_model_filtered_summary$coefficients[which(plsda_model_filtered_summary$coefficients[,4] <= 0.05),1],
  "Std. Fehler" = plsda_model_filtered_summary$coefficients[which(plsda_model_filtered_summary$coefficients[,4] <= 0.05),2],
  "odds" = exp(plsda_model_filtered_summary$coefficients[which(plsda_model_filtered_summary$coefficients[,4] <= 0.05),4])
)
# Print the table
print(pvalue_metrics_table)
# Optionally, save the table to a CSV file
write.xlsx(pvalue_metrics_table, file = "calculated_pvalue_score_plsda.xlsx", rowNames = FALSE)

#
#
# n_rows = nrow(train_data[, c("DERIVAT_DIABETES_DS_HISTORY", variables_plsda)])
# n_cols = ncol(train_data[, c("DERIVAT_DIABETES_DS_HISTORY", variables_plsda)])
# random_matrix <- matrix(sample(1:100, n_rows * n_cols, replace = TRUE), nrow = n_rows, ncol = n_cols)
#
# # Print the matrix
# predictedPercentes <- runif( length(test_data$DERIVAT_DIABETES_DS_HISTORY), min=0, max=1)
# rtestBin<- sample(c(0, 1), length(test_data$DERIVAT_DIABETES_DS_HISTORY), replace = TRUE)
#
# random_model<- glm(rtestBin ~ .,
#                    data = as.data.frame(cbind(random_matrix,rtestBin)),
#                    family = binomial)
#
# calculated_metrics_random <- CalculateMetrics(predictedPercentes, rtestBin, random_model)
# calculated_metrics_random




# Finden der gemeinsamen Elemente
gemeinsame_elemente <- intersect(variables_plsda, variables_lasso)

# Ausgabe der gemeinsamen Elemente
print(gemeinsame_elemente)
# Finden der Elemente, die nur in variables_plsda sind
nur_plsda <- setdiff(variables_plsda, gemeinsame_elemente)

# Finden der Elemente, die nur in variables_lasso sind
nur_lasso <- setdiff(variables_lasso, gemeinsame_elemente)


intersect_table <- list(
  gemeinsame_elemente = sort(gemeinsame_elemente),
  only_in_variables_plsda = sort(nur_plsda),
  only_in_variables_lasso = sort(nur_lasso)
)
# Print the table
print(intersect_table)
# Optionally, save the table to a CSV file
write.xlsx(intersect_table, file = "intersect_table.xlsx", rowNames = FALSE)


####################################################################################
# Kreuzvalidierung durchführen
auc_results_plsda <- numeric(10)
rsquared_results_plsda <- numeric(10)
auc_results_lasso <- numeric(10)
rsquared_results_lasso <- numeric(10)

auc_results_standart <- numeric(10)
rsquared_results_standart <- numeric(10)

auc_results_standart2 <- numeric(10)
rsquared_results_standart2 <- numeric(10)

for(i in 1:10) {
  # Seed für jede Iteration setzen, um unterschiedliche Aufteilungen zu gewährleisten
  set.seed(i * 100)  # Multiplikation stellt sicher, dass jeder Seed unterschiedlich ist

  # Daten mischen und teilen
  train_indices <- sample(1:nrow(full_data), 0.7 * nrow(full_data))
  train_data <- full_data[train_indices, ]
  test_data <- full_data[-train_indices, ]

  # Modell anpassen
  plsda_model_filtered<- glm(DERIVAT_DIABETES_DS_HISTORY ~ .,
                             data = train_data[, c("DERIVAT_DIABETES_DS_HISTORY", variables_plsda)],
                             family = binomial)

  predictions_plsda <- predict(plsda_model_filtered, newdata = test_data, type = "response")

  calculated_metrics_plsda_cv <- CalculateMetrics(predictions_plsda, test_data$DERIVAT_DIABETES_DS_HISTORY, plsda_model_filtered)


  auc_results_plsda[i] <- calculated_metrics_plsda_cv$AUC

  rsquared_results_plsda[i] <- calculated_metrics_plsda_cv$adj_pseudo_r_squared

  # Modell anpassen
  lasso_model_filtered<- glm(DERIVAT_DIABETES_DS_HISTORY ~ .,
                             data = train_data[, c("DERIVAT_DIABETES_DS_HISTORY", variables_lasso)],
                             family = binomial)

  predictions_lasso <- predict(lasso_model_filtered, newdata = test_data, type = "response")

  calculated_metrics_lasso_cv <- CalculateMetrics(predictions_lasso, test_data$DERIVAT_DIABETES_DS_HISTORY, lasso_model_filtered)


  auc_results_lasso[i] <- calculated_metrics_lasso_cv$AUC

  rsquared_results_lasso[i] <- calculated_metrics_lasso_cv$adj_pseudo_r_squared

  variables_standart <- c("NEW_BMI", "SEX",  "age")
  # build the logistic regression model
  standart_model_filtered<- glm(DERIVAT_DIABETES_DS_HISTORY ~ .,
                                data = train_data[, c("DERIVAT_DIABETES_DS_HISTORY", variables_standart)],
                                family = binomial)

  predictions_test <- predict(standart_model_filtered, newdata = test_data, type = "response")

  calculated_metrics_standart <- CalculateMetrics(predictions_test, test_data$DERIVAT_DIABETES_DS_HISTORY, standart_model_filtered)
  auc_results_standart[i] <- calculated_metrics_standart$AUC

  rsquared_results_standart[i] <- calculated_metrics_standart$adj_pseudo_r_squared

  variables_standart2 <- c("NEW_WTH", "SEX",  "age")
  # build the logistic regression model
  standart_model_filtered2<- glm(DERIVAT_DIABETES_DS_HISTORY ~ .,
                                data = train_data[, c("DERIVAT_DIABETES_DS_HISTORY", variables_standart2)],
                                family = binomial)

  predictions_test <- predict(standart_model_filtered2, newdata = test_data, type = "response")

  calculated_metrics_standart2 <- CalculateMetrics(predictions_test, test_data$DERIVAT_DIABETES_DS_HISTORY, standart_model_filtered2)
  auc_results_standart2[i] <- calculated_metrics_standart2$AUC

  rsquared_results_standart2[i] <- calculated_metrics_standart2$adj_pseudo_r_squared

}

# Durchschnitt und Standardabweichung berechnen
mean_auc_lasso <- round(mean(auc_results_lasso),3)
sd_auc_lasso <- round(sd(auc_results_lasso),3)
mean_r2_lasso <- round(mean(rsquared_results_lasso),3)
sd_r2_lasso <- round(sd(rsquared_results_lasso),3)
# Durchschnitt und Standardabweichung berechnen
mean_auc_plsda <- round(mean(auc_results_plsda),3)
sd_auc_plsda <- round(sd(auc_results_plsda),3)
mean_r2_plsda <- round(mean(rsquared_results_plsda),3)
sd_r2_plsda <- round(sd(rsquared_results_plsda),3)
# Durchschnitt und Standardabweichung berechnen
mean_auc_standart <- round(mean(auc_results_standart),3)
sd_auc_standart <- round(sd(auc_results_standart),3)
mean_r2_standart <- round(mean(rsquared_results_standart),3)
sd_r2_standart <- round(sd(rsquared_results_standart),3)
# Convert the named vector to a data frame
mean_auc_standart2 <- round(mean(auc_results_standart2),3)
sd_auc_standart2 <- round(sd(auc_results_standart2),3)
mean_r2_standart2 <- round(mean(rsquared_results_standart2),3)
sd_r2_standart2 <- round(sd(rsquared_results_standart2),3)

metrics_table <- data.frame(
  Metrik = c("Adj. Pseudo R²", "AUC"),
  PLSDA = round(as.numeric(calculated_metrics_plsda),3),
  LASSO = round(as.numeric(calculated_metrics_lasso),3),
  Standart = round(as.numeric(calculated_metrics_standart),3),
  Standart2 = round(as.numeric(calculated_metrics_standart2),3),
  cvPLSDA = c(paste(mean_r2_plsda, "±",sd_r2_plsda),paste(mean_auc_plsda, "±",sd_auc_plsda)),
  cvLASSO = c(paste(mean_r2_lasso, "±",sd_r2_lasso),paste(mean_auc_lasso, "±",sd_auc_lasso)),
  cvStandart = c(paste(mean_r2_standart, "±",sd_r2_standart),paste(mean_auc_standart, "±",sd_auc_standart)),
  cvStandart2 = c(paste(mean_r2_standart2, "±",sd_r2_standart2),paste(mean_auc_standart2, "±",sd_auc_standart2))

  )
# Print the table
print(metrics_table)
# Optionally, save the table to a CSV file
write.xlsx(metrics_table, file = "calculated_metrics_ALLE_table.xlsx", rowNames = FALSE)



if (!require("VennDiagram")) {
  install.packages("VennDiagram")
  library(VennDiagram)
}
# Create a Venn diagram
venn.plot <- venn.diagram(
  x = list(
    plsda = sort(variables_plsda),
    lasso = sort(variables_lasso),
    standart = sort(variables_standart),
  ),
  filename = NULL, # NULL means it will plot to the RStudio plot pane or your default graphic device
  category.names = c("PLS-DA", "LASSO", "Standard"),
  fill = c("blue", "orange", "yellow"),
  alpha = 0.5,
  cex = 2,
  cat.cex = 1.5,
  cat.col = c("black", "black", "black")
)
# Plot the diagram
dev.off()
grid.draw(venn.plot)
dev.off()



print(datasort$evaluation5)
# Combine elements with descriptions and any other metadata
all_elements <- unlist(datasort$evaluation5)  # Assuming evaluation5 contains actual measurement names or IDs
descriptions <- datasort$descriptions$beschreibung[match(all_elements, datasort$descriptions$kuerzl)]

# Create a data frame with additional column for descriptions
elements_df <- data.frame(
  Category = rep(names(all_elements), sapply(all_elements, length)),  # Adjust this if the structure is different
  Element = all_elements,
  Description = descriptions
)

# Print the data frame to check its structure
print(elements_df)

# Write the data frame to an Excel file
write.xlsx(elements_df, file = "DetailedElementsList.xlsx", rowNames = FALSE)






# Kreuzvalidierung durchführen
auc_results <- list()
rsquared_results <- list()

# Define predictor sets
predictor_sets <- list(
  BS_NECK_AT_BASE_GTH = c("SEX", "age", "BS_NECK_AT_BASE_GTH"),
  BS_THIGH_GTH_L_HZ = c("SEX", "age", "BS_THIGH_GTH_L_HZ"),
  NEW_WTH = c("SEX", "age", "NEW_WTH"),
  BS_WAIST_T_BUTTOCK = c("SEX", "age", "BS_WAIST_T_BUTTOCK"),
  BS_SIDE_UP_TORSO_LTH_R = c("SEX", "age", "BS_SIDE_UP_TORSO_LTH_R"),
  BS_WAISTBAND_F_HT = c("SEX", "age", "BS_WAISTBAND_F_HT"),
  BS_HIP_HT = c("SEX", "age", "BS_HIP_HT"),
  BS_KNEE_GTH_L = c("SEX", "age", "BS_KNEE_GTH_L")
)

# Initialize results storage
for (key in names(predictor_sets)) {
  auc_results[[key]] <- numeric(10)
  rsquared_results[[key]] <- numeric(10)
}

for (i in 1:10) {
  # Seed für jede Iteration setzen, um unterschiedliche Aufteilungen zu gewährleisten
  set.seed(i * 100)  # Multiplikation stellt sicher, dass jeder Seed unterschiedlich ist

  # Daten mischen und teilen
  train_indices <- sample(1:nrow(full_data), 0.7 * nrow(full_data))
  train_data <- full_data[train_indices, ]
  test_data <- full_data[-train_indices, ]

  for (key in names(predictor_sets)) {
    variables <- predictor_sets[[key]]
    model <- glm(DERIVAT_DIABETES_DS_HISTORY ~ ., data = train_data[, c("DERIVAT_DIABETES_DS_HISTORY", variables)], family = binomial)
    predictions <- predict(model, newdata = test_data, type = "response")
    calculated_metrics <- CalculateMetrics(predictions, test_data$DERIVAT_DIABETES_DS_HISTORY, model)
    auc_results[[key]][i] <- calculated_metrics$AUC
    rsquared_results[[key]][i] <- calculated_metrics$adj_pseudo_r_squared
  }
}

# Calculate mean and standard deviation for each model
summary_metrics <- function(metric_results) {
  return(list(mean = round(mean(metric_results), 3), sd = round(sd(metric_results), 3)))
}

metrics <- lapply(names(predictor_sets), function(key) {
  auc = summary_metrics(auc_results[[key]])
  r2 = summary_metrics(rsquared_results[[key]])
  return(c(r2_mean = auc$mean, r2_sd = auc$sd, auc_mean = r2$mean, auc_sd = r2$sd))
})

# Convert the results to a data frame
metrics_df <- do.call(rbind, metrics)
metrics_df <- as.data.frame(metrics_df)
rownames(metrics_df) <- names(predictor_sets)

# Create metrics table
metrics_table <- data.frame(
  Metrik = c( "AUC","Adj. Pseudo R²"),
  BS_NECK_AT_BASE_GTH = c(paste(metrics_df[1, 1], "±", metrics_df[1, 2]), paste(metrics_df[1, 3], "±", metrics_df[1, 4])),
  BS_THIGH_GTH_L_HZ = c(paste(metrics_df[2, 1], "±", metrics_df[2, 2]), paste(metrics_df[2, 3], "±", metrics_df[2, 4])),
  NEW_WTH = c(paste(metrics_df[3, 1], "±", metrics_df[3, 2]), paste(metrics_df[3, 3], "±", metrics_df[3, 4])),
  BS_WAIST_T_BUTTOCK = c(paste(metrics_df[4, 1], "±", metrics_df[4, 2]), paste(metrics_df[4, 3], "±", metrics_df[4, 4])),
  BS_SIDE_UP_TORSO_LTH_R = c(paste(metrics_df[5, 1], "±", metrics_df[5, 2]), paste(metrics_df[5, 3], "±", metrics_df[5, 4])),
  BS_WAISTBAND_F_HT = c(paste(metrics_df[6, 1], "±", metrics_df[6, 2]), paste(metrics_df[6, 3], "±", metrics_df[6, 4])),
  BS_HIP_HT = c(paste(metrics_df[7, 1], "±", metrics_df[7, 2]), paste(metrics_df[7, 3], "±", metrics_df[7, 4])),
  BS_KNEE_GTH_L = c(paste(metrics_df[8, 1], "±", metrics_df[8, 2]), paste(metrics_df[8, 3], "±", metrics_df[8, 4]))
)

# Print the table
print(metrics_table)

# Optionally, save the table to an Excel file
write.xlsx(metrics_table, file = "calculated_metrics_ALLE_ALLE_table.xlsx", rowNames = FALSE)

