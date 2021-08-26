# dwcaExport
# Jon Peder Lindemann, 25.02.2021
# Export Darwin Core Archive from database query
# Parameters:
# db_query | a table that contains occurrence data, with header names in Darwin Core terms
# file_name | a string that specifies the export file name

dwcaExport <- function(db_query) {

file_name <- timestamp(prefix = "dwca_export_", suffix = ".txt")
  
xml_dwca <- paste('<?xml version="1.0"?>\n',
              '<archive xmlns="http://rs.tdwg.org/dwc/text/">\n',
              '\t<core encoding="UTF-8" linesTerminatedBy="\\n" fieldsTerminatedBy="\\t" fieldsEnclosedBy="" ignoreHeaderLines="1" rowType="http://rs.tdwg.org/dwc/terms/Occurrence">\n',
              '\t\t<files>\n',
              '\t\t\t<location>', file_name, '.txt</location>\n',
              '\t\t</files>\n',
              '\t\t<id index="0"/>\n',
              '\t\t<field index="1" term="http://rs.tdwg.org/dwc/terms/basisOfRecord"/>\n',
		          '\t\t<field index="2" term="http://purl.org/dc/terms/language"/>\n',
              '\t\t<field index="3" term="http://purl.org/dc/terms/modified"/>\n',
		          '\t\t<field index="4" term="http://purl.org/dc/terms/rightsHolder"/>\n',sep = "")


# Add columns defined by Darwin Core terms
for (i in 3:ncol(db_query)) {
  xml_dwca <- paste(xml_dwca, '\t\t<field index="',2+i,'" term="http://rs.tdwg.org/dwc/terms/',names(db_query)[i],'"/>\n', sep = "")
}

# Add end of XML
xml_dwca <- paste(xml_dwca, "\t</core>\n</archive>", sep = "")

# write xml file
fileConn<-file("meta.xml", encoding = "UTF-8")
writeLines(xml_dwca, fileConn)
close(fileConn)

# write tab delimited table
export_df <- data.frame (id = db_query$occurrenceID, basisOfRecord = "PreservedSpecimen", language = "en", db_query)
write.table(export_df,file = paste(file_name, ".txt", sep = ""), quote = FALSE, row.names = FALSE, sep = "\t", fileEncoding = "UTF-8")

}