# alterPrjOcc, Jon Peder Lindemann, 03.03.2021
# add or remove occurrence IDs to or from a project 
# Parameters:
# CONN | database connection
# occurrenceID | a vector of occurrence IDs
# datasetName | a string that specifies the project title
# delete_IDs   | a logical variable. If TRUE the input occurrenceIDs will be deleted from the project
#CONN = tmu_db
#datasetName = "Insect collection, TSZ"
#occurrenceID = prjOcc(tmu_db, projectTitle = "Exechia_determinations")
#delete_IDs = FALSE

alterDsOcc <- function(CONN, datasetName, occurrenceID, delete_IDs = FALSE) {
  source("functions/dsOcc.R")
  old_IDs <- dsOcc(CONN, datasetName) # Get occurrences already in dataset
  
  if (isFALSE(delete_IDs)) {
    new_ids <- unique(c(old_IDs, occurrenceID)) # Combine unique occurrence IDs of a combination of old and input occurrence IDs 
  }
  if  (isTRUE(delete_IDs)) {
    new_ids <- old_IDs[is.na(match(old_IDs, occurrenceID))] # Remove occurrence IDs from vector by matching input with old occurrence IDs
  }
  new_ids_string <- paste(new_ids, collapse = "|") # Collapse vector and add "|" as separator
  # Update table with new occurrenceID string
  dbExecute(CONN, paste("UPDATE Datasets SET occurrenceID = '", new_ids_string, "' WHERE datasetName = '", datasetName, "'", sep = ""))
}

