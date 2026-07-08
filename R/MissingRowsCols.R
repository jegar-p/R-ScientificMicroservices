#' Get a report of missing values in a dataframe by row and column
#'
#' @description MissingRowsCols takes a dataframe and reports the percentage of missing data by row and column.
#' @param missing_data A data.frame with potentially missing data.
#' @param key The API key received from scientificmicroservices.com . Can be left blank if the value is stored as 'scimicro_key' in .Renviron
#' @param email The user email provided to receive the API key.
#' @return A summary of percentage of missing data by row and column.
#' @examples 
#' \dontrun{MissingRowsCols(missing_data = data.frame(c(10, 11, 12), c(50, NA, 50), c(NA, NA, 14), c(50, 50, NA)), email = 'info@scientificmicroservices.com')}
#' @export
MissingRowsCols <- function(missing_data, email = "", key = "")
  # Get a summary of the missing data in your dataset by row and column.
{
  if(key == "")
  {key <- test_key()}
  x <- jsonlite::fromJSON(rawToChar(httr::POST("https://api.scientificmicroservices.com/missingrowscols",
                                                 httr::add_headers('email' = email ,
                                                                   'key'= key,
                                                                   'Content-Type' = "application/json"),
                                                 body = jsonlite::toJSON(secure_missing_dataset(missing_data)),
                                                 encode = 'json')$content))
x$rows$row <- x$rows$row +1
return(x)
}


