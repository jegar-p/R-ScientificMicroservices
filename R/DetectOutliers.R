#' Detect outliers in a vector of values
#'
#' @description DetectOutliers runs three statistical tests and returns the position and value of outlier (unusual) values in a vector.
#' @param n A vector of values
#' @param key The API key received from scientificmicroservices.com . Can be left blank if the value is stored as 'scimicro_key' in .Renviron
#' @param email The user email provided to receive the API key.
#' @return The position and value of outliers
#' @examples 
#' \dontrun{DetectOutliers(c(9.9815,10.3615,9.3237,1135.3029,10.4203,9.3234,8.5119,9.2336,7.9625,7.1538), email = 'info@scientificmicroservices.com')}
#' @export
DetectOutliers <- function(n, email = "", key = "")
{
  if(key == "")
  {key <- test_key()}
  x <- jsonlite::fromJSON(rawToChar(httr::POST("https://api.scientificmicroservices.com/detectoutliers",
                                                 httr::add_headers('email' = email ,
                                                                   'key'= key,
                                                                   'Content-Type' = "application/json"),
                                                 body = jsonlite::toJSON(n), 
                                                 encode = 'json')$content))
  x$position <- x$position +1 #Position is 0-indexed normally
  return(x)
}
