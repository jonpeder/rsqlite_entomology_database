# simpleExport, Jon Peder Lindemann, 18 Feb. 2021
# Export occurrences to simple darwin core XML based on a database query. Update the 'last_export' value for exported records. 
# Parameters:
# CONN | Database connection
# new_records | logical, if TRUE, restrict query to include only records that have not previously been exported
# rank | a vector of character strings that specifies what taxon ranks to be exported, usually "species", "genus", "family", and/or "order"

## Lag en løsning for å eksportere data basert på projectName

simpleExport <- function(CONN, 
                        file_name, 
                        new_records = TRUE, 
                        rank = NULL, 
                        write_simple_dwc = FALSE,
                        write_dwc_archive = FALSE,
                        datasetName) {
  
  # Prepare query statement for selecting only records not previously exported
  if (isTRUE(new_records)) {
    statement_1 <- "Occurrences.last_export IS NULL AND "
  } else {
    statement_1 <- ""
  }
  
  # Prepare query statement for selecting taxon rank. If taxon rank is not specified, select all records with a (non-empty) taxonRank value.
  if (is.null(rank)) {
    statement_2 <-  "Taxa.taxonRank != '' AND Taxa.taxonRank IS NOT NULL"
  } else {
    statement_2 <- paste("Taxa.taxonRank = '",rank[1] ,"' ", sep = "")
    for (i in 2:length(rank)) {
      statement_2 <- paste(statement_2, "OR ", statement_1, "Taxa.taxonRank = '",rank[i] ,"' ", sep = "")
    }
  }
  
  # Make query
  query <- dbGetQuery(CONN, paste("
    SELECT 
      Occurrences.modified,
      Occurrences.occurrenceID,
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
      Collecting_methods.ID = Collecting_events.samplingProtocol
    WHERE ", paste(statement_1, statement_2, sep =""), sep = ""))
  
  # If write_simple_dwc and write_dwc_archive is FALSE, return query result
  if (isFALSE(write_simple_dwc) & isFALSE(write_dwc_archive)) {
    return(query)
  }
  else {
      # Query dataset data and add combine the two queries
      query_dataset <- dbGetQuery(CONN, paste("SELECT * FROM Datasets WHERE datasetName = '", datasetName, "'", sep = ""))
      query = data.frame(modified = paste(substr(query$modified, 1, 10),'T',substr(query$modified, 12, 16), 'Z', sep = ""), 
                         query_dataset[,c(4,1,2,3)], 
                         query[, !(names(query) %in% "modified")])
    
    # Export a Simple Darwin Core XML file
    if (isTRUE(write_simple_dwc)) {
      source("functions/sdwcExport.R")
      sdwcExport(db_query = query, file_name = file_name) 
    }
    
    # Export Darwin Core archive  
    if (isTRUE(write_dwc_archive)) {
      source("functions/dwcaExport.R")
      dwcaExport(db_query = query, file_name = file_name)
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
          Taxa 
        WHERE ", paste(statement_1, statement_2, sep =""), ")", sep = "")
    )
  }
} 
   