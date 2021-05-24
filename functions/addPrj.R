# addPrj, Jon Peder Lindemann, 03.03.2021
# Add new project to Ptojects table
# Parameters:
# CONN | a string that specifies a database connection

addPrj <- function(CONN,
                   projectTitle,
                   projectID = uuid::UUIDgenerate(),
                   projectOwner = "",
                   projectDescription = "",
                   projectStartDate = "",
                   projectEndDate = "",
                   projectURL = "") {
  
  y <- data.frame(projectTitle,
                  projectID,
                  projectOwner,
                  projectDescription,
                  projectStartDate,
                  projectEndDate,
                  projectURL,
                  occurrenceID = "")
  
  dbAppendTable(CONN, "Projects", y)
}


