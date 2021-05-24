# prjOcc
# split occurrenceID string in a project and return a vector of ocurrence IDs
# Parameters
# CONN | database connection
# projectTitle | string specifying projectTitle

prjOcc <- function(CONN, projectTitle) {
  query <- dbGetQuery(CONN, paste("SELECT occurrenceID FROM Projects WHERE projectTitle ='", projectTitle, "'", sep = ""))
  if(isFALSE(length(query[,1]) == 0)) {
    id_vector <- strsplit(query[,1], "\\|")[[1]] # split query result by "|"
  } else {
    id_vector <- NULL
  }
  return(id_vector)
}