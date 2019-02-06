#' Get the urls from IMIS using the output of the datasets function
#'
#' This function gets the themes associated to datasets using the output of the datasets function
#' @param datasetrecords mandatory parameter, the output of the datasets function
#' @import dplyr hlptools
#' @export
#' @examples
#' dasurls <- dataseturls(datasets(4662))
#' dasurls <- dataseturls(datasets(c("1884","618", "5780" )))



getdasurls <- function (datasetrecords) {


  for (i in 1:length(datasetrecords)){
    for (j in 1:length(datasetrecords[[i]]$urls)){
      l <- lapply(datasetrecords, function(term) {
        tryCatch({element <- rlang::as_list(term)}, error=function(x){ })
        return(list(
          dasid = unlist(datasetrecords[[i]]$datasetrec$DasID),
          title = unlist(datasetrecords[[i]]$datasetrec$StandardTitle),
          url = unlist(datasetrecords[[i]]$urls[[j]]$URL),
          urltype = unlist(datasetrecords[[i]]$urls[[j]]$URLType)
                  ))})

      r1 <-bind_rows(unlist(l))
      if (exists("r2")){
        r2 <- bind_rows(r2, r1)
      } else {
        r2<-r1
      }
    }}

  dataseturls <- r2

  ##### add filter on contacts + add get emailadress from persons module


  return(dataseturls)
}
