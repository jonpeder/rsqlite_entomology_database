# occIdLabels, Jon Peder Lindemann, 14.06.2021
# Print insectlabels from database query
# CONN | SQLite database connection
# occurrenceID | a vector or character string, specifying occurrence ID of records to be printed
# filename | character string, specifying file name of output pdf


catalogLabels <- function(CONN, 
                          occurrenceID, 
                          filename = "cataloglabels.pdf", 
                          type = "type",
                          font_family = "sans") {
  
  # Make SQL query for each collecting event
  for (i in 1:length(occurrenceID)) {
    query_tmp <- dbGetQuery(CONN, paste("SELECT
      Collecting_events.eventID,
      Occurrences.catalogNumber,
      Collecting_events.countryCode,
      Collecting_events.stateProvince,
      Collecting_events.strand_id,
      Collecting_events.municipality,
      Collecting_events.locality_1,
      Collecting_events.locality_2,
      Collecting_events.habitat,
      Collecting_events.decimalLatitude,
      Collecting_events.decimalLongitude,
      Collecting_events.coordinateUncertaintyInMeters,
      Collecting_methods.samplingProtocol,
      Collecting_events.eventDate_1,
      Collecting_events.eventDate_2,
      Collecting_events.recordedBy
      FROM Collecting_events 
      INNER JOIN Collecting_methods 
      ON Collecting_methods.ID = Collecting_events.samplingProtocol
      Inner JOIN Occurrences
      ON Occurrences.eventID = Collecting_events.eventID
      WHERE Occurrences.catalogNumber = '", occurrenceID[i], "'", sep = ""))
    if (isTRUE(exists("query"))) {
      query = rbind(query, query_tmp)
    } else { query = query_tmp}
  }
  
  # Add some details to some of the columns
  ## Concatenate StateProvince and CountryCode if stateProvince is recorded
  for (i in 1:nrow(query)){
    if (query$stateProvince[i] != "" & isFALSE(is.na(query$stateProvince[i]))) {
      query$countryCode[i] <- paste(query$countryCode[i], ", ", query$stateProvince[i], ": ", sep = "")
    }
  }
  ## Country in capital letters
  query$countryCode <- toupper(query$countryCode) 
  ## Add "±" and "m" to radius if radius is recorded
  for (i in 1:nrow(query)){
    if (isFALSE(is.na(query$coordinateUncertaintyInMeters[i])) & query$coordinateUncertaintyInMeters[i] != "") {
      query$coordinateUncertaintyInMeters[i] <- paste("\u00B1",query$coordinateUncertaintyInMeters[i],"m",sep = "") 
    }
  }
  ## Add colon after method
  query$samplingProtocol <- paste(query$samplingProtocol, ":", sep = "") 
  ## Add colon after municipality
  query$municipality <- paste(query$municipality, ":", sep = "") 
  # Put habitat within parenthesis where habitat is recorded
  for (i in 1:nrow(query)) {
    if (query$habitat[i] != "") {
      query$habitat[i] <- paste("(", query$habitat[i], ")", sep = "") 
    } 
  }
  for (i in 1:nrow(query)) {
    if (query$locality_2[i] != "") {
      query$locality_1[i] <- paste(query$locality_1[i], ",", sep = "") # Add comma after Loc1 when Loc2 is present
    } 
  }
  
  # If more than one collector (separated by '|'), use last names. Add "Leg." to beginning of strings
  for (i in 1:nrow(query)) {
    if (isTRUE(grep("\\|", query$recordedBy[i]) == 1)) {
      tmp <- gsub("^.* ", "", strsplit(query$recordedBy[i], " \\| |\\||\\| | \\|")[[1]])
      if (length(tmp) == 2) {
        query$recordedBy[i] <- paste("Leg. ", paste(tmp, collapse = " & ", sep = ""), sep = "")
      } else {
        query$recordedBy[i] <- paste("Leg. ", tmp[1], " et al.", sep = "")
      }
    } else {
      query$recordedBy[i] <- paste("Leg. ", query$recordedBy[i], sep = "") 
    }
  }
  
  # Create a date-column with concatinated eventDate_1 and eventDate_2
  query$eventDate <- ""
  for (i in 1:nrow(query)){
    tmp1 <- regexec("^([0-9]{4})-([0-9]{2})-([0-9]{2})", query$eventDate_1[i])
    year1 <- regmatches(query$eventDate_1[i], tmp1)[[1]][2]
    month1 <- regmatches(query$eventDate_1[i], tmp1)[[1]][3]
    date1 <- regmatches(query$eventDate_1[i], tmp1)[[1]][4]
    if (query$eventDate_2[i] != "") {
      tmp2 <- regexec("^([0-9]{4})-([0-9]{2})-([0-9]{2})", query$eventDate_2[i])
      year2 <- regmatches(query$eventDate_2[i], tmp2)[[1]][2]
      month2 <- regmatches(query$eventDate_2[i], tmp2)[[1]][3]
      date2 <- regmatches(query$eventDate_2[i], tmp2)[[1]][4]
      if (year1 == year2) {
        if (month1 == month2) {
          query$eventDate[i] <- paste(date1, "/", date2, ".", month2, ".", year2, sep = "")}
        else { query$eventDate[i] <- paste(date1, ".", month1, "/", date2, ".", month2, ".", year2, sep = "")}}
      else {query$eventDate[i] <- paste(date1, ".", month1, ".", year1, "/", date2, ".", month2, ".", year2, sep = "")}
    } else {query$eventDate[i] <- paste (date1, "." , month1, ".", year1, sep = "")}
  }
  
  if (type == "type") {
    ## Mycetophilidae. Labels without habitat.
    insectlabel::insectlabel(label_df = query, n = 1, x = 40, y = 8, filename = filename, family = font_family, text_order = list(c(3,4,5,6),c(7,8),c(10,11,12),c(13,17),16), 
                             QR_data = c(2,3), fontsize = 3, linedist = 1, tx = 11, ty = -1, QRd = 1.6, QRx = 4.4, 
                             QRy = 0, delim = ";", qrlevel = 2)
  }
}

