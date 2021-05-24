# alterPrjOcc, Jon Peder Lindemann, 03.03.2021
# add or remove occurrence IDs to or from a project 
# Parameters:
# CONN | database connection
# occurrenceID | a vector of occurrence IDs
# projectTitle | a string that specifies the project title 

alterPrjOcc <- function(CONN, projectTitle, occurrenceID, delete_IDs = FALSE) {
  source("functions/prjOcc.R")
  old_IDs <- prjOcc(CONN, projectTitle) # Get occurrences already in project
  
  if (isFALSE(delete_IDs)) {
    new_ids <- unique(c(old_IDs, occurrenceID)) # Combine unique occurrence IDs of a combination of old and input occurrence IDs 
  }
  if  (isTRUE(delete_IDs)) {
    new_ids <- old_IDs[is.na(match(old_IDs, occurrenceID))] # Remove occurrence IDs from vector by matching input with old occurrence IDs
  }
  new_ids_string <- paste(new_ids, collapse = "|") # Collapse vector and add "|" as separator
  # Update table with new occurrenceID string
  dbExecute(CONN, paste("UPDATE Projects SET occurrenceID = '", new_ids_string, "' WHERE projectTitle = '", projectTitle, "'", sep = ""))
}

