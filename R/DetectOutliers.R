#' Detect outliers in a vector of values
#'
#' @description DetectOutliers runs three statistical tests and returns the position and value of outlier (unusual) values in a vector.
#' @param n A vector of values
#' @return The position and value of outliers
#' @examples DetectOutliers(c(9.9815,10.3615,9.3237,1135.3029,10.4203,9.3234,8.5119,9.2336,7.9625,7.1538))
#' @export
DetectOutliers <- function(n, key = "")
{
  if(key == "")
  {key <- test_key()}
  x <- data.table::setDT(jsonlite::fromJSON(rawToChar(httr::POST("https://missingbias.p.rapidapi.com/DetectOutliers",
                                                 httr::add_headers(`x-rapidapi-host` = 'detectoutliers.p.rapidapi.com/' ,
                                                                   `x-rapidapi-key`= key,
                                                                   `Content-Type` = "application/json"),
                                                 body = as.list(n), encode = 'json')$content)))
  x[, position := position +1] #Position is 0-indexed normally
  return(x)
}
