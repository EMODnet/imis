imis_url <- function() {
  getOption("imis_url", "http://www.vliz.be/nl/imis")
}

imis_request <- function(parameters, verbose = FALSE) {
  url <- imis_url()
  response <- GET(url, user_agent("imis - https://github.com/iobis/imis"), query = parameters)
  if (verbose) {
    print(response)
  }
  return(response)
}
