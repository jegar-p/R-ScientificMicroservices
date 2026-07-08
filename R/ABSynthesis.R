#' Combine AB test results for a summary uplift and p-value
#'
#' @description ABSynthesis combines experiments with binomial outcomes (successes and trials), reporting the summary uplift and p-value.
#' @param successes_base Count of responses in the base condition
#' @param base_n Count of visitors exposed to the base condition
#' @param successes_variant Count of responses in the base condition
#' @param variant_n Count of visitors exposed to the base condition
#' @param key The API key received from scientificmicroservices.com . Can be left blank if the value is stored as 'scimicro_key' in .Renviron
#' @param email The user email provided to receive the API key.
#' @return Summary uplift and p-value of a meta-analysis of online controlled experiments
#' @examples 
#' \dontrun{ABSynthesis(c(10, 11, 12), c(50, 50, 50), c(12, 13, 14), c(50, 50, 50), email = 'info@scientificmicroservices.com')}
#' @export
ABSynthesis <- function(successes_base, base_n, successes_variant, variant_n, email = "", key = "")
    {
    if(key == "")
        {key <- test_key()}
    mydata <- data.table::data.table("successes_base"  = successes_base, "trials_base" = base_n, "successes_variant" = successes_variant, "trials_variant" = variant_n)
        jsondataset <- jsonlite::toJSON(mydata)
        return(jsonlite::fromJSON(rawToChar(httr::POST("https://api.scientificmicroservices.com/absynthesis",
                                                       httr::add_headers('email' = email ,
                                                                   'key'= key,
                                                                   'Content-Type' = "application/json"),
                                                       body = jsondataset, encode = 'json')$content))
              )
}
