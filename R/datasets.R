#' Get datasets by special collection.
#'
#' @param id Special collection id.
#' @param verbose Verbose.
#' @return Datasets data frame.
#' @export
datasetsSpecialCollection <- function(id, verbose = FALSE) {

  pagesize <- 50
  result <- NULL
  i <- 1

  while (TRUE) {

    parameters <- list(
      module = "dataset",
      spcolid = id,
      show = "json",
      count = pagesize,
      start = i
    )

    response <- imis_request(parameters, verbose)
    json <- content(response, "text", encoding = "UTF-8")
    datasets <- fromJSON(json, simplifyVector = TRUE)

    if (length(datasets) > 0) {
      result <- bind_rows(result, datasets)
    } else {
      break
    }

    i <- i + pagesize

  }

  return(result)

}

#' Get datasets by ID.
#'
#' @param ids Dataset IDs.
#' @param verbose Verbose.
#' @return List of datasets.
#' @export
datasets <- function(ids, verbose = FALSE) {

  result <- list()

  for (i in 1:length(ids)) {

    parameters <- list(
      dasid = ids[i],
      show = "json"
    )

    response <- imis_request(parameters, verbose)
    json <- content(response, "text", encoding = "UTF-8")
    dataset <- fromJSON(json, simplifyVector = FALSE)

    result[[i]] <- dataset

  }

  return(result)

}
