#' Detect whether there is a bias in the missingness of values in a vector.
#'
#' @description MissingBias returns a 1 (is biased) or 0 (is not biased) indicating whether the values in the biased column are more likely to be missing depending on the value of the predictor column.
#' @param biased Count of responses in the base condition
#' @param predictor Count of visitors exposed to the base condition
#' @param key The API key received from scientificmicroservices.com . Can be left blank if the value is stored as 'scimicro_key' in .Renviron
#' @param email The user email provided to receive the API key.
#' @return A numeric 1 or 0 output indicating whether data is more likely to be missing in the biased column with particular values of the predictor column.
#' @examples 
#' \dontrun{MissingBias(biased = c(1, 4, 6, NA, 10), predictor = c(5, 6, 7, 25, 5), email = 'info@scientificmicroservices.com')}
#' @export
MissingBias <- function(biased, predictor, email = "", key = "")
{
  if(key == "")
  {key <- test_key()}
  y <- list("col1" = biased, "col2" = predictor)
  return(jsonlite::fromJSON(rawToChar(httr::POST("https://api.scientificmicroservices.com/missingbias", 
                                                httr::add_headers('email' = email ,
                                                                   'key'= key,
                                                                   'Content-Type' = "application/json"), 
                                                                   body = jsonlite::toJSON(y), encode = 'json')$content)))
}

