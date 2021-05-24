# eventTable, Jon Peder Lindemann, 21.02.2021
# Populate database with input locality data
# Parameters:
# CONN | string specifying database connection
# eventID | string specifying a unique collecting-event ID. This is mandatory 
# UPDATE | logical value. If TRUE, collecting events may be updated where non-unique eventIDs are specified



eventTABLE <- function(CONN,
                       eventID = NULL,
                       country = "Norway",
                       stateProvince = "",
                       county = "",
                       strand_id = "",
                       municipality = "",
                       locality_1 = "",
                       locality_2 = "",
                       habitat = "",
                       decimalLatitude = "",
                       decimalLongitude = "",
                       coordinateUncertaintyInMeters = "",
                       samplingProtocol = "",
                       eventDate_1 = "",
                       eventDate_2 = "",
                       recordedBy = "",
                       type = "",
                       orient = "",
                       dist_m = "",
                       eventRemarks = "",
                       geodeticDatum = "WGS84",
                       UPDATE = FALSE
                       ) {
  
  # Abort if 'eventID' is not specified
  if (is.null(eventID)) {
    stop("ERROR : 'eventID' is not specified")
  }
  
  # Before populating 'Collecting_events' table, count number of current rows
  count1 <- nrow(dbGetQuery(CONN, "SELECT * FROM Collecting_events"))
  
  # If UPTATE is TRUE, prepare update SQL statement
  if(isTRUE(UPDATE)) {
    update_statement <- " ON CONFLICT (eventID) DO UPDATE SET 
                country = excluded.country, 
                stateProvince = excluded.stateProvince, 
                county = excluded.county, 
                strand_id = excluded.strand_id, 
                municipality = excluded.municipality, 
                locality_1 = excluded.locality_1,
                locality_2 = excluded.locality_2,
                habitat = excluded.habitat,
                decimalLatitude = excluded.decimalLatitude,
                decimalLongitude = excluded.decimalLongitude,
                coordinateUncertaintyInMeters = excluded.coordinateUncertaintyInMeters,
                samplingProtocol = excluded.samplingProtocol,
                eventDate_1 = excluded.eventDate_1,
                eventDate_2 = excluded.eventDate_2,
                recordedBy = excluded.recordedBy,
                type = excluded.type,
                orient = excluded.orient,
                dist_m = excluded.dist_m,
                eventRemarks = excluded.eventRemarks,
                geodeticDatum = excluded.geodeticDatum"
  }
  else {
    update_statement <- ""
  }
  
  # Add input to dataframe.This adds default values to all rows
  y <- data.frame(eventID,
                  country,
                  stateProvince,
                  county,
                  strand_id,
                  municipality,
                  locality_1,
                  locality_2,
                  habitat,
                  decimalLatitude,
                  decimalLongitude,
                  coordinateUncertaintyInMeters,
                  samplingProtocol,
                  eventDate_1,
                  eventDate_2,
                  recordedBy,
                  type,
                  orient,
                  as.integer(dist_m),
                  eventRemarks,
                  geodeticDatum, stringsAsFactors = FALSE)
  
  #for(i in 1:nrow(y)) {
  #  dbAppendTable(CONN, "Collecting_events", y[i], append = TRUE)
  #}
#}
  # Turn foreign keys on
  dbExecute(CONN,"PRAGMA foreign_keys = ON")
  
  # Populate data, row by row
  for (i in 1:nrow(y)) {
  event_tmp <- paste(
    "INSERT INTO Collecting_events (eventID,
                  country,
                  stateProvince,
                  county,
                  strand_id,
                  municipality,
                  locality_1,
                  locality_2,
                  habitat,
                  decimalLatitude,
                  decimalLongitude,
                  coordinateUncertaintyInMeters,
                  samplingProtocol,
                  eventDate_1,
                  eventDate_2,
                  recordedBy,
                  type,
                  orient,
                  dist_m,
                  eventRemarks,
                  geodeticDatum) VALUES ('", 
    paste(
      y$eventID[i],
      y$country[i],
      y$stateProvince[i],
      y$county[i],
      y$strand_id[i],
      y$municipality[i],
      y$locality_1[i],
      y$locality_2[i],
      y$habitat[i],
      y$decimalLatitude[i],
      y$decimalLongitude[i],
      y$coordinateUncertaintyInMeters[i],
      y$samplingProtocol[i],
      y$eventDate_1[i],
      y$eventDate_2[i],
      y$recordedBy[i],
      y$type[i],
      y$orient[i],
      y$dist_m[i],
      y$eventRemarks[i],
      y$geodeticDatum[i],
      sep = "', '"),
      "')", update_statement,
    sep = "")
  
  # Populate 'Collecting_events' table row by row. Use the 'tryCatch' function
  tryCatch({
    dbExecute(CONN, event_tmp)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n-The error occurs on row",i," when populating the 'Collecting_events' table.\n-Check primary key 'eventID', and foreign key 'samplingProtocol'.\n\n")})
  }
  
  # Calculate the number of rows added to each of the tables 
  count2 <- nrow(dbGetQuery(CONN, "SELECT * FROM Collecting_events"))
  # Write message to output
  writeLines(paste("Collecting events added:", count2-count1))
}




