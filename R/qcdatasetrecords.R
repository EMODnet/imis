#' Get a QC value for datasets
#'
#' This generates a flat table of an IMIS dataset record using DasIDs
#' @param flatdatasetrecords mandatory parameter, the output of the flatdatasetrecords function
#' @import dplyr hlptools
#' @export
#' @examples
#' QCedrecords <- qcdatasetrecords(flatdatasetrecords(dasid = 4662))
#' QCedrecords <- qcdatasetrecords(flatdatasetrecords = c("1884","618", "5780" ))



qcdatasetrecords <- function (flatdatasetrecords) {

  QCrecords <- flatdatasetrecords %>% mutate (QC = if_else(is.na(citation) |is.na(title) |is.na(licence)|is.na(abstract)  |is.na(contact), "F",
                                                       if_else (!licence %in% c("Attribution (CC BY)", "Attribution-NonCommercial (CC BY-NC)", "CC 0 (No Rights Reserved)"), "C",
                                                                if_else (nchar(abstract) > 200 | (!is.na(description) & nchar(description)) > 200 , "A", "B"))))
  return(QCrecords)

}
