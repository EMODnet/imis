#' Get the keywords from IMIS using the output of the datasets function
#'
#' This function gets the keywords associated to datasets using the output of the datasets function
#' @param datasetrecords mandatory parameter, the output of the datasets function
#' @import dplyr hlptools
#' @export
#' @examples
#' dasthemes <- getdaskeywords(datasets(4662))
#' dasthemes <- getdaskeywords(datasets(c("1884","618", "5780" )))



getdaskeywords <- function (datasetrecords) {


  for (i in 1:length(datasetrecords)){
    for (j in 1:length(datasetrecords[[i]]$keywords)){
      l <- lapply(datasetrecords, function(term) {
        tryCatch({element <- rlang::as_list(term)}, error=function(x){ })
        return(list(
          dasid = unlist(datasetrecords[[i]]$datasetrec$DasID),
          title = unlist(datasetrecords[[i]]$datasetrec$StandardTitle),
          keyword = unlist(datasetrecords[[i]]$keywords[[j]]$ThesaurusTerm)
        ))})

      r1 <-bind_rows(unlist(l))
      if (exists("returndatasets")){
        returndatasets <- bind_rows(returndatasets, r1)
      } else {
        returndatasets<-r1
      }
    }}

  returndatasets <- returndatasets %>% fncols(c("dasid","title","keyword"))

  datasetkeywords <- returndatasets %>% select (dasid, title, keyword) %>% distinct()

  return(datasetkeywords)
}
