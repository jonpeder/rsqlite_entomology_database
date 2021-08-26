# dsOcc
# split occurrenceID string in a dataset and return a vector of ocurrence IDs
# Parameters
# CONN | database connection
# datasetName | string specifying datasetName

dsOcc <- function(CONN, datasetName) {
  query <- dbGetQuery(CONN, paste("SELECT occurrenceID FROM Datasets WHERE datasetName ='", datasetName, "'", sep = ""))
  if(isFALSE(length(query[,1]) == 0)) {
    id_vector <- strsplit(query[,1], "\\|")[[1]] # split query result by "|"
  } else {
    id_vector <- NULL
  }
  return(id_vector)
}