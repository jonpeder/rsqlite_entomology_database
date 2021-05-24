# addEvents, Jon Peder Lindemann, 21.02.2021
# Populate database with input locality data
# Parameters:
# CONN | string specifying database connection
# eventID | string specifying a unique collecting-event ID. This is mandatory 


addEvents <- function(CONN,
                       eventID,
                       countryCode = "NO",
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
                       eventRemarks = "",
                       geodeticDatum = "WGS84"
                       ) {
  
  # Before populating 'Collecting_events' table, count number of current rows
  count1 <- nrow(dbGetQuery(CONN, "SELECT * FROM Collecting_events"))
  
  # Add input to dataframe.This adds default values to all rows
  y <- data.frame(eventID,
                  countryCode,
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
                  eventRemarks,
                  geodeticDatum)
  
  # Turn foreign keys on
  dbExecute(CONN,"PRAGMA foreign_keys = ON")
  
 # for(i in 1:nrow(y)) {
    dbAppendTable(CONN, "Collecting_events", y, append = TRUE)
  #}
}
    