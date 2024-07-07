
NaFilter <- function(df, max_na, colnames, filter = TRUE) {
  df$BS_MISSINGS <- apply(df[unlist(colnames)], 1, function(x) sum(is.na(x)))
  if(filter){    df <- df[which(df$BS_MISSINGS <= max_na),]    }
  return(df)
}

