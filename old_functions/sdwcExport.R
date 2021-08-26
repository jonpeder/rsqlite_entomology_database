# sdwcExport
# Jon Peder Lindemann, 25.02.2021
# Write Simple Darwin Core XML file
# Parameters:
# db_query | a table that contains occurrence data, with header names in Darwin Core terms

sdwcExport <- function(db_query, file_name) {
  
  # Add SimpleDarwinRecordSet
  xml1 <- paste('<?xml version="1.0" encoding="UTF-8"?>\n<SimpleDarwinRecordSet\n',
                '\txmlns="http://rs.tdwg.org/dwc/xsd/simpledarwincore/"\n',
                '\txmlns:dc="http://purl.org/dc/terms/"\n',
                '\txmlns:dc="http://purl.org/dc/elements/1.1/"\n',
                '\txmlns:dwc="http://rs.tdwg.org/dwc/terms/"\n',
                '\txmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"\n',
                '\txsi:schemaLocation="http://rs.tdwg.org/dwc/xsd/simpledarwincore/ http://rs.tdwg.org/dwc/xsd/tdwg_dwc_simple.xsd">\n', 
                sep = "")
  
  # Add SimpleDarwinCoreRecords              
  # Loop over each record in the input table
  for (u in 1:nrow(db_query)) {
    db_query_row <- db_query[u, c(3:ncol(db_query))]
    # Add the the modification date-time, and the global values, 'rightsHolder', 'rights' and 'institutionCode' 
    xml2 <- ""
    xml2 <- paste('\t<SimpleDarwinRecord>\n',
                  '\t\t<dc:language>en</dc:language>\n',
                  '\t\t<dwc:basisOfRecord>PreservedSpecimen</dwc:basisOfRecord>\n',
                  '\t\t<dc:modified>', db_query$modified[u], '</dc:modified>\n',
                  '\t\t<dc:rightsHolder>', db_query$rightsHolder[u], '</dc:rightsHolder>\n',
                  sep = "")
    # Loop over each row in a record, and add available values (when not "", NULL or N/A)
    for (i in 1:ncol(db_query_row)) {
      if (isFALSE(db_query_row[1,i] == "" | is.null(db_query_row[1,i]) | is.na(db_query_row[1,i]))) {
        xml2 <- paste(xml2, "\t\t<dwc:", names(db_query_row)[i], ">", db_query_row[1, i], "</dwc:", names(db_query_row)[i], ">\n", sep = "")
      }
      else {
        next
      }
    }
    # Add end of record
    xml1 <- paste(xml1, xml2, '\t</SimpleDarwinRecord>\n', sep = "")
  }
  
  # Add end of record set
  xml1 <- paste(xml1, '</SimpleDarwinRecordSet>', sep = "")
  # Save xml file
  fileConn<-file(paste(file_name, ".xml", sep = ""), encoding = "UTF-8")
  writeLines(xml1, fileConn)
  close(fileConn)
  # Write message to output
  cat(paste("Records written to xml:", nrow(db_query)))

}

