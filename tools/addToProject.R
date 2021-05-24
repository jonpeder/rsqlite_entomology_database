# addToProject
# Add records to project by reading occurrenceIDs or (not implemented yet) unit_ids from QR codes
# Parameters:
# CONN
# projectTitle
# occurrenceID

addToProject <- function(CONN, occurrenceID, projectTitle) {
  source("functions/splitIDs.R")
  occ_ids <- splitIDs(occurrenceID)
  # Update projectTitle
  for (i in 1:length(occ_ids)) {
    dbExecute(CONN, paste("UPDATE Occurrences SET projectTitle = '", projectTitle, "' WHERE occurrenceID = '", occ_ids[i], "'", sep = ""))
  }
}
