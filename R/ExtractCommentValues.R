

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