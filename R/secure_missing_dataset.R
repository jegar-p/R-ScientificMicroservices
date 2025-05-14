secure_missing_dataset <- function(missing_data)
  # This function is used internally by the SM_MissingRowsCols function (so load it too!) , but feel free to use it for missingness stuff where you don't want to send values.
  {return(replace(data.frame(lapply(missing_data, as.character), stringsAsFactors = FALSE),
    !is.na(missing_data), "1"))}