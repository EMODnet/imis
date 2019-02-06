#' Get a flat table of an IMIS datasets record using DasIDs
#'
#' This generates a flat table of an IMIS dataset record using DasIDs
#' @param datasetrecords mandatory parameter, the output of the datasets function
#' @import dplyr hlptools
#' @export
#' @examples
#' dasrecord <- flatdatasetrecord(dasid = 4662)
#' dasrecord <- flatdatasetrecord(dasid = c("1884","618", "5780" ))




flatdatasetrecord <- function (dasid) {

datasetrecords <- datasets(dasid)

dascitations <- getdascitations(datasetrecords)

dastheme <- getdasthemes(datasetrecords)
daskeywords <- getdaskeywords(datasetrecords)
dascontacts <- getdascontacts(datasetrecords)


dastheme2 <- aggregate(theme  ~ dasid, data = dastheme, paste, collapse = " , ")
daskeywords2 <- aggregate(keyword  ~ dasid, data = daskeywords, paste, collapse = " , ")
dascontacts2 <- aggregate(contact  ~ dasid, data = dascontacts, paste, collapse = " , ")


output <- dascitations %>% left_join(dascontacts2, by ="dasid") %>% left_join(dastheme2, by="dasid") %>% left_join(daskeywords2, by ="dasid")

return(output)

}
