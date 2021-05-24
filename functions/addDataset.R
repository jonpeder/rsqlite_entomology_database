# addDataset, Jon Peder Lindemann, 03.03.2021
# Add new dataset to Datasets table
# Parameters:
# CONN | a string that specifies a database connection



addDataset <- function(CONN,
                      datasetName,
                      collectionCode,
                      institutionCode,
                      rightsHolder) {
  
  y <- data.frame(datasetName,
                  collectionCode,
                  institutionCode,
                  rightsHolder,
                  occurrenceID = "")
  
  dbAppendTable(CONN, "Datasets", y)
}


