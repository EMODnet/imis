imis_url <- function() {
  getOption("imis_url", "http://www.vliz.be/en/imis")
}

imis_request <- function(parameters, verbose = FALSE) {
  url <- imis_url()
    response <- GET(url, user_agent("IMIS R client (https://github.com/vlizBE/imis)"), query = parameters)
  if (verbose) {
    print(response)
  }
  return(response)
}
