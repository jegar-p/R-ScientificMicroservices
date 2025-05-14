#' Detect whether there is a bias in the missingness of values in a vector.
#'
#' @description MissingBias returns a 1 (is biased) or 0 (is not biased) indicating whether the values in the biased column are more likely to be missing depending on the value of the predictor column.
#' @param biased Count of responses in the base condition
#' @param predictor Count of visitors exposed to the base condition
#' @return A numeric 1 or 0 output indicating whether data is more likely to be missing in the biased column with particular values of the predictor column.
#' @examples ABSynthesis_advanced(c(10, 10), c(50, 50), c(12, 12), c(50, 50))
#' @export
MissingBias <- function(biased, predictor, key = "")
{
  if(key == "")
  {key <- test_key()}
  y <- list("col1" = biased, "col2" = predictor)
  return(jsonlite::fromJSON(rawToChar(httr::POST("https://missingbias.p.rapidapi.com/MissingBias", httr::add_headers(`x-rapidapi-host` = 'missingbias.p.rapidapi.com/' , `x-rapidapi-key`= key, `Content-Type` = "application/json"), body = jsonlite::toJSON(y), encode = 'json')$content))$missing_is_biased)
}

