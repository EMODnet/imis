#' Get the themes from IMIS based on dasids
#'
#' This function gets the themes associated to  dasids
#' @param dasid mandatory parameter, the dasid of the dataset
#' @import dplyr hlptools
#' @export
#' @examples
#' dasthemes <- getdasthemes(4662)
#' dasthemes <- getdasthemes(dasid = c("1884","618", "5780" ))



getdasthemes <- function (dasid) {
  dasid <- unique(dasid[!is.na(dasid)])
  dasid <- dasid[(dasid != "")]

  datasetrecords <- datasets(dasid)


  for (i in 1:length(datasetrecords)){
    for (j in 1:length(datasetrecords[[i]]$dasthemes)){
        l <- lapply(datasetrecords, function(term) {
          tryCatch({element <- rlang::as_list(term)}, error=function(x){ })
          return(list(
            dasid = unlist(datasetrecords[[i]]$datasetrec$DasID),
            title = unlist(datasetrecords[[i]]$datasetrec$StandardTitle),
            theme = unlist(datasetrecords[[i]]$dasthemes[[j]]$DasTheme),
            theme0 = unlist(datasetrecords[[i]]$dasthemes[[j]]$DasTheme0)
          ))})

      r1 <-bind_rows(unlist(l))
      if (exists("returndatasets")){
        returndatasets <- bind_rows(returndatasets, r1)
      } else {
        returndatasets<-r1
      }
  }}

  returndatasets <- returndatasets %>% fncols(c("dasid","title","theme", "theme0"))

  datasetthemes <- returndatasets %>% select (dasid, title, theme,theme0) %>% distinct()

  return(datasetthemes)
}
