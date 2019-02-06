#' Get the themes from IMIS using the output of the datasets function
#'
#' This function gets the themes associated to datasets using the output of the datasets function
#' @param datasetrecords mandatory parameter, the output of the datasets function
#' @import dplyr hlptools
#' @export
#' @examples
#' dasthemes <- getdasthemes(datasets(4662))
#' dasthemes <- getdasthemes(datasets(c("1884","618", "5780" )))



getdasthemes <- function (datasetrecords) {


  for (i in 1:length(datasetrecords)){
    for (j in 1:length(datasetrecords[[i]]$dasthemes)){
        l <- lapply(datasetrecords, function(term) {
          tryCatch({element <- rlang::as_list(term)}, error=function(x){ })
          return(list(
            dasid = unlist(datasetrecords[[i]]$datasetrec$DasID),
            title = unlist(datasetrecords[[i]]$datasetrec$StandardTitle),
            theme2 = unlist(datasetrecords[[i]]$dasthemes[[j]]$DasTheme),
            theme = unlist(datasetrecords[[i]]$dasthemes[[j]]$DasTheme0)
          ))})

      r1 <-bind_rows(unlist(l))
      if (exists("returndatasets")){
        returndatasets <- bind_rows(returndatasets, r1)
      } else {
        returndatasets<-r1
      }
  }}

  returndatasets <- returndatasets %>% fncols(c("dasid","title","theme", "theme2"))

  datasetthemes <- returndatasets %>% select (dasid, title, theme,theme2) %>% distinct()

  return(datasetthemes)
}
