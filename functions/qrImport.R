# qrImport, Jon Peder Lindemann, 24.02.2021
# Pupulate 'Occurrences' table in database with scanned QR codes from specimen labels and one det. label
# Parameters:
# CONN              | a string that specifies the database connection
# qr_data           | a string of scanned qr codes from specimen labels and one det. label, delimited by a newline character "\n"
# identifiedBy      | a string specifying name of person who identified the specimen
# sex               | a string specifying the sex ("male"/"female"/"")
# lifeStage         | a string specifying the lifestage ("larva"/"pupa"/"adult"/"")
# preparations      |
# occurrenceRemarks |


qrImport <- function (CONN,
                      qr_data,
                      sex = "",
                      lifeStage = "adult", 
                      preparations = "pinned", 
                      identifiedBy = "",
                      projectTitle = "",
                      occurrenceRemarks = "") {

  # split input string by "det."
  qr_data2 <- strsplit(qr_data, "det\\.")[[1]]
  for (i in 2:length(qr_data2)) {
    det_occ <- strsplit(qr_data2[i], "\n")[[1]] # split elements by newline characters
    det_occ <- det_occ[det_occ != "" & det_occ != " "] # Remove empty rows
    qr_det <- det_occ[1] # Extract first element as determination data
    qr_occ <- det_occ[2:length(det_occ)] # Extract second to last element as ocurrenceIDs
    if(is.na(qr_occ[1])) { # If only determination label is scanned move to next label-subset
      next
    }
    
    occ <- do.call(rbind.data.frame, strsplit(qr_occ, ";")) # Extract eventID from occurreceID
    occ_df <- data.frame (eventID = occ[,1], occurrenceID = qr_occ) # Create a table of eventIDs and occurrenceIDs
    if (nrow(occ_df[occ_df$eventID == occ_df$occurrenceID,]) >= 1) {
      occ_df[occ_df$eventID == occ_df$occurrenceID,][1] = "" # Remove eventIDs that equals occurrenceIDs
    }
    
    detlabel <- strsplit(qr_det, ";")[[1]] # Split det-label data delimited by ";"
    
    # If sex is stated, replace sex from detlabel
    if (sex != "") {
      detlabel[5] = sex
    }
    
    # Abort if any occurrenceID are longer than 90 characters
    for (u in 1:nrow(occ_df)) {
      if (nchar(occ_df$occurrenceID[u]) > 90) {
        stop("At least on occurrenceID are longer than 90 characters.\n  A newline character is possibly missing")
      }
    }
    
    cat("\nTaxon: ", detlabel[1], "\n", sep = "")
    
    # Put into addOccurrences function
    source("functions/addOccurrences.R")
    addOccurrences(CONN = CONN, 
                   occurrenceID = occ_df$occurrenceID, 
                   eventID = occ_df$eventID, 
                   scientificName = detlabel[1], 
                   taxonRank = detlabel[2], 
                   family = detlabel[4], 
                   order = detlabel[3], 
                   identifiedBy = identifiedBy, 
                   individualCount = 1, 
                   sex = detlabel[5], 
                   lifeStage = lifeStage, 
                   preparations = preparations, 
                   unit_id = detlabel[7],
                   ownerInstitutionCode = detlabel[6],
                   occurrenceRemarks = occurrenceRemarks)
    
  }
  # Add occurrences to project if project title is specified
  if (isTRUE(nchar(projectTitle) > 0)) {
    source("functions/alterPrjOcc.R")
    source("functions/splitIDs.R")
    alterPrjOcc(CONN, occurrenceID = splitIDs(qr_data), projectTitle = projectTitle)
  }
}

