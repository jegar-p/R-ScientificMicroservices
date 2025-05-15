#' Get a report of missing values in a dataframe by row and column
#'
#' @description MissingRowsCols takes a dataframe and reports the percentage of missing data by row and column.
#' @param missing_data A data.frame with potentially missing data.
#' @return A summary of percentage of missing data by row and column.
#' @examples MissingRowsCols(data.frame(c(10, 11, 12), c(50, NA, 50), c(NA, NA, 14), c(50, 50, NA)))
#' @export
MissingRowsCols <- function(missing_data, key = "")
  # Get a summary of the missing data in your dataset by row and column.
{
  if(key == "")
  {key <- test_key()}
  x <- jsonlite::fromJSON(rawToChar(httr::POST("https://missingrowscols.p.rapidapi.com/MissingRowsCols",
                                                 httr::add_headers(`x-rapidapi-host` = 'missingrowscols.p.rapidapi.com/' ,
                                                                   `x-rapidapi-key`= key,
                                                                    `Content-Type` = "application/json"),
                                                 body = jsonlite::toJSON(secure_missing_dataset(missing_data)),
                                                 encode = 'json')$content))
x$rows$row <- x$rows$row +1
return(x)
}


