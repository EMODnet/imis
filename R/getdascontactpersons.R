#' Get the contacts from IMIS using the output of the datasets function
#'
#' This function gets the themes associated to datasets using the output of the datasets function
#' @param datasetrecords mandatory parameter, the output of the datasets function
#' @import dplyr hlptools
#' @export
#' @examples
#' dasthemes <- getdascontacts(datasets(4662))
#' dasthemes <- getdascontacts(datasets(c("1884","618", "5780" )))



getdascontacts <- function (datasetrecords) {


  for (i in 1:length(datasetrecords)){
    for (j in 1:length(datasetrecords[[i]]$ownerships)){
      l <- lapply(datasetrecords, function(term) {
        tryCatch({element <- rlang::as_list(term)}, error=function(x){ })
        return(list(
          dasid = unlist(datasetrecords[[i]]$datasetrec$DasID),
          title = unlist(datasetrecords[[i]]$datasetrec$StandardTitle),
          ContactEmail = unlist(datasetrecords[[i]]$datasetrec$ContactEmail),
          Firstname = unlist(datasetrecords[[i]]$ownerships[[j]]$Firstname),
          Surname = unlist(datasetrecords[[i]]$ownerships[[j]]$Surname),
          PersID = unlist(datasetrecords[[i]]$ownerships[[j]]$PersID),
          Role = unlist(datasetrecords[[i]]$ownerships[[j]]$Role),
          FullInstitute = unlist(datasetrecords[[i]]$ownerships[[j]]$FullInstitute)
          ))})

      r1 <-bind_rows(unlist(l))
      if (exists("r2")){
        r2 <- bind_rows(r2, r1)
      } else {
        r2<-r1
      }
    }}

  c1 <- r2 %>% fncols(c("dasid","title","ContactEmail")) %>% filter (!is.na(ContactEmail)) %>% distinct() %>% mutate (contact = ContactEmail)
  c2 <- r2 %>% fncols(c("dasid","title","Firstname", "Surname", "Role", "FullInstitute")) %>% filter (Role=="Contact" & !is.na(Firstname)) %>% mutate (contact = paste(Firstname, Surname))  %>% distinct()
  c3 <- r2 %>% fncols(c("dasid","title","Firstname", "Surname", "Role", "FullInstitute")) %>% filter (Role=="Contact" & is.na(Firstname)) %>% mutate (contact = FullInstitute) %>% distinct()

  datasetcontacts <- bind_rows(c1, c2, c3) %>% select( "dasid","title", "contact")%>% distinct()

  ##### add filter on contacts + add get emailadress from persons module


  return(datasetcontacts)
}
