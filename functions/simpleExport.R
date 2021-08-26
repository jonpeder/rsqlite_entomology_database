# simpleExport, Jon Peder Lindemann, 18 Feb. 2021
# Export occurrences to simple darwin core XML based on a database query. Update the 'last_export' value for exported records. 
# Parameters:
# CONN | Database connection
# new_records | logical, if TRUE, restrict query to include only records that have not previously been exported
# rank | a vector of character strings that specifies what taxon ranks to be exported, usually "species", "genus", "family", and/or "order"
# occurrenceIDs
# datasetName
# write_dwca

simpleExport_1 <- function(CONN,
                         rank = NULL,
                         occurrenceID = NULL,
                         datasetName = NULL,
                         write_dwca = FALSE) {
  
  # If occurrence IDs or a dataset is specified, make a vector of occurrenceIDs
  if (isFALSE(is.null(datasetName))) {
    source("functions/dsOcc.R")
    occIDs <- dsOcc(CONN, datasetName = datasetName)
  } 
  else if (isFALSE(is.null(occurrenceID))) {
    occIDs <- occurrenceID
  }
  else {
    occIDs <- ""
  }
  # Prepare a WHERE query statement for selecting records with specified occurrenceIDs and/or with specified taxonomic rank
  if (is.null(rank)) {
    query_statement <- paste(" WHERE Occurrences.occurrenceID IN ('", paste(occIDs, collapse = "', '"), "')", sep = "")
  }
  else {
    query_statement <- paste(" WHERE Occurrences.occurrenceID IN ('", paste(occIDs, collapse = "', '"), "') AND Taxa.taxonRank = '", rank[1], "'", sep = "")
    if (isTRUE(length(rank) > 1)) {
      for (i in 2:length(rank)) {
        query_statement_tmp <- paste("Occurrences.occurrenceID IN ('", paste(occIDs, collapse = "', '"), "') AND Taxa.taxonRank = '", rank[i], "'", sep = "")
        query_statement <- paste(query_statement, query_statement_tmp, sep = " OR ")
      }
    }
  }
  
  # Make query
  query <- dbGetQuery(CONN, paste("
    SELECT 
      Occurrences.modified,
      Occurrences.occurrenceID,
      Occurrences.catalogNumber,
      Taxa.scientificName,
      Taxa.taxonRank,
      Taxa.scientificNameAuthorship,
      Taxa.specificEpithet,
      Taxa.genus,
      Taxa.family,
      Taxa.'order',
      Taxa.kingdom,
      Taxa.phylum,
      Taxa.class,
      Taxa.nomenclaturalCode,
      Occurrences.identifiedBy,
      Occurrences.dateIdentified,
      Occurrences.individualCount,
      Occurrences.sex,
      Occurrences.lifeStage,
      Occurrences.preparations,
      Occurrences.ownerInstitutionCode,
      Occurrences.occurrenceRemarks,
      Occurrences.associatedTaxa,
      Occurrences.associatedReferences,
      Collecting_events.eventID,
      Country_codes.countryCode,
      Country_codes.country,
      Collecting_events.stateProvince,
      Collecting_events.county,
      Collecting_events.municipality,
      CASE WHEN Collecting_events.locality_2 != '' THEN Collecting_events.locality_1 || ', ' ELSE Collecting_events.locality_1 END || Collecting_events.locality_2 'locality',
      Collecting_events.habitat,
      Collecting_events.decimalLatitude,
      Collecting_events.decimalLongitude,
      Collecting_events.coordinateUncertaintyInMeters,
      Collecting_events.geodeticDatum,
      Collecting_methods.samplingProtocol,
      CASE WHEN Collecting_events.eventDate_2 != '' THEN Collecting_events.eventDate_1 || '/' || Collecting_events.eventDate_2 ELSE Collecting_events.eventDate_1 END 'eventDate',
      Collecting_events.recordedBy,
      Collecting_events.eventRemarks
    FROM 
      Taxa 
    INNER JOIN 
      Occurrences 
    ON  
      Taxa.scientificName = Occurrences.scientificName
    INNER JOIN 
      Collecting_events
    ON 
      Collecting_events.eventID = Occurrences.eventID
    INNER JOIN
      Country_codes
    ON
      Country_codes.countryCode = Collecting_events.countryCode
    INNER JOIN
      Collecting_methods
    ON 
      Collecting_methods.ID = Collecting_events.samplingProtocol", 
    query_statement, sep = ""))
  
  # If write_dwca is FALSE, return query result
  if (isFALSE(write_dwca)) {
    return(query)
  } 
  else {
    # Get dataset data and add combine with the dataframe from the query
    query_dataset <- dbGetQuery(CONN, paste("SELECT * FROM Datasets WHERE datasetName = '", datasetName, "'", sep = ""))
    query = data.frame(modified = paste(substr(query$modified, 1, 10),'T',substr(query$modified, 12, 16), 'Z', sep = ""), 
                       query_dataset[,c(4,2,3)], 
                       query[, !(names(query) %in% "modified")])
  
    
    
    # Export Darwin Core archive  
    if (isTRUE(write_dwca)) {
      source("functions/dwcaExport.R")
      dwcaExport(db_query = query)
    }
    # Update 'last_export' column in Occurrences table. Use a FROM statement because UPDATE statements are not supported by JOINs in SQLite
    dbExecute(CONN, paste("
    UPDATE 
      Occurrences
    SET 
      last_export = CURRENT_TIMESTAMP
    WHERE 
      scientificName IN (
        SELECT 
          scientificName 
        FROM 
          Taxa", 
        query_statement, ")", sep = "")
    )
  }
} 
