# addOccurrences, Jon Peder Lindemann, 18.02.2021
# Populate Occurrences and Taxa table in database 
# Parameters:
# CONN | string specifying database connection
# occurrenceID | string specifying a unique occurrence ID. Usually a UUID.
# eventID | string specifying a collecting-event ID. Mandatory when occurrenceID is specified.
# scientificName | character string specifying a scientific taxon name. Mandatory.
# taxonRank | character string specifying the taxonomic rank of the scientific name

addOccurrences <- function(CONN, 
                            occurrenceID = "",
                            catalogNumber = "",
                            eventID = "",
                            scientificName,
                            taxonRank = "",
                            family = "",
                            order = "",
                            identifiedBy = "",
                            individualCount = 1,
                            sex = "",
                            lifeStage = "adult",
                            preparations = "pinned",
                            unit_id = "",
                            occurrenceRemarks = "",
                            ownerInstitutionCode = "") {
  
  # Abort if no 'scientificName' is specified 
  if (length(scientificName) < 1) {
    stop("ERROR : 'scientificName' should be specified ")
  }
  
  # If 'occurrenceID' includes a newline character, use 'specimenIDs' function to split into a vector
  if (isTRUE(grep("\n", occurrenceID) >= 1)) {
    source("functions/splitIDs.R")
    occurrenceID <- splitIDs(occurrenceID)
  }
  
  # Abort if 'occurrenceID' but not 'eventID' is specified
  if (isFALSE(occurrenceID == "") & isTRUE(eventID == "")) {
    for (i in 1:length(occurrenceID)) {
      test <- dbGetQuery(CONN, paste("SELECT occurrenceID FROM Occurrences WHERE occurrenceID = '", occurrenceID[i], "'", sep = ""))
      if(identical(test[[1]], character(0))) {
        stop("ERROR : 'occurrenceID' with unknown 'eventID' detected")
      }
    }
  }
  
  # Create variables 'scientificNameAuthorship', 'specificEpithet' and 'genus' 
  scientificNameAuthorship <-  ""
  specificEpithet <- ""
  genus <- ""
  
  # Add input to dataframe. Adds default values to all rows
  y <- data.frame(occurrenceID,
                  catalogNumber,
                  eventID,
                  identifiedBy,
                  individualCount,
                  sex,
                  lifeStage, 
                  preparations,
                  unit_id,
                  ownerInstitutionCode,
                  occurrenceRemarks,
                  scientificName,
                  scientificNameAuthorship,
                  specificEpithet,
                  genus,
                  family,
                  order,
                  taxonRank)
  
  # For each record extract information from 'scientificName' column and add to 'genus', 'specificEpiteth' and 'scientificNameAuthorship' columns
  for (u in 1:nrow(y)) {
    ## Where 'taxonRank' equals "species" or "spnov", extract 'genus', 'specificEpiteth' and (if present) 'scientificNameAuthorship'
    if (y$taxonRank[u] == "species" | y$taxonRank[u] == "spnov") {
      sp_gen_aut <- regexec("^([a-zA-Z]+)\ ([a-zA-Z\\>\\<\\-]+)\ ?([a-zA-Z0-9\\,\ \\(\\)æøåöèéü&]*)", y$scientificName[u])
      y$genus[u] <- regmatches(y$scientificName[u], sp_gen_aut)[[1]][2]
      y$specificEpithet[u] <- regmatches(y$scientificName[u], sp_gen_aut)[[1]][3]
      y$scientificNameAuthorship[u] <- regmatches(y$scientificName[u], sp_gen_aut)[[1]][4]
    }
    ## Where 'taxonRank' equals "genus" or "species-group", extract 'genus'
    if (y$taxonRank[u] == "genus" | y$taxonRank[u] == "species-group") {
      gen <- regexec("^([a-zA-Z]+)", y$scientificName[u])
      y$genus[u] <- regmatches(y$scientificName[u], gen)[[1]][2]
    }
  }


  # Before populating database tables, count number of current rows
  # Taxa table
  tax1 <- nrow(dbGetQuery(CONN, "SELECT * FROM Taxa"))
  # Occurrences table
  occ1 <- nrow(dbGetQuery(CONN, "SELECT * FROM Occurrences"))
  
  # Turn foreign keys on
  dbExecute(CONN,"PRAGMA foreign_keys = ON")
  
  # Paste input data, row by row, into two SQLite commands, one for each table 
  for (i in 1:length(occurrenceID)) {
    # Insert Taxa-row or ignore if 'scientificName' is already present
    # Paste input data into an SQL command 
    taxa_tmp <- paste(
      "INSERT OR IGNORE INTO Taxa (scientificName, scientificNameAuthorship, specificEpithet, genus, family, 'order', taxonRank) VALUES ('", 
      paste(
        y$scientificName[i], 
        y$scientificNameAuthorship[i], 
        y$specificEpithet[i], 
        y$genus[i], 
        y$family[i], 
        y$order[i], 
        y$taxonRank[i], 
        sep = "', '"), 
      "')", 
      sep = "")
    
    # Paste input data into an SQL command
    # Insert Occurrences-row or replace (update) if 'occurrenceID' is already present.
    occurrences_tmp <- paste(
      "INSERT INTO Occurrences (occurrenceID, catalogNumber, eventID, scientificName, identifiedBy, individualCount, sex, lifeStage, preparations, occurrenceRemarks, unit_id, ownerInstitutionCode, modified) VALUES ('", 
      paste(
        y$occurrenceID[i],
        y$catalogNumber[i],
        y$eventID[i],
        y$scientificName[i],
        y$identifiedBy[i],
        y$individualCount[i],
        y$sex[i],
        y$lifeStage[i], 
        y$preparations[i],
        y$occurrenceRemarks[i],
        y$unit_id [i],
        y$ownerInstitutionCode[i],
        sep = "', '"),
      "', CURRENT_TIMESTAMP) ON CONFLICT (occurrenceID) DO UPDATE SET modified = excluded.modified, scientificName = excluded.scientificName, identifiedBy = excluded.identifiedBy, sex = excluded.sex, unit_id = excluded.unit_id",
      sep = "")
    
    # Populate Taxa and Occurrences tables row by row, first the 'Taxa' table . Add 'tryCatch' function for error handeling
    ## Taxa
    tryCatch({
      dbExecute(CONN, taxa_tmp)
    }, error=function(e){cat("ERROR :",conditionMessage(e), " on row",i," when populating the 'Taxa' table.\nOccurrenceID:",y$occurrenceID[i],"\n-The error may be associated with 'taxonRank' input\n\n")})
    ## Occurrences. Skip if no occurrenceID is specified. 
    if (isFALSE(isTRUE(y$occurrenceID == ""))) {
      tryCatch({
        dbExecute(CONN, occurrences_tmp)
      }, error=function(e){cat("ERROR :",conditionMessage(e), " on row",i," when populating the 'Occurrences' table.\nOccurrenceID:",y$occurrenceID[i],"\n-The error may be associated with 'eventID', 'sex' or 'lifeStage' input\n\n")})
    }
  }
  
  # Calculate the number of rows added to each of the tables 
  ## Number of taxa after
  tax2 <- nrow(dbGetQuery(CONN, "SELECT * FROM Taxa"))
  ## Number of occurrences after
  occ2 <- nrow(dbGetQuery(CONN, "SELECT * FROM Occurrences"))
  ## Write message to output
  writeLines(paste("Occurrences added:", occ2-occ1, ("\nTaxa added:"), tax2-tax1))
  
}
