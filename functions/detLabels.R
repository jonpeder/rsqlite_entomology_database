# detLabels, Jon Peder Lindemann, 22.02.2021
# Print determination labels
# CONN              | a sting that specifies the database connection
# scientificName    | a string or vector of strings specifying a taxon name from the Taxa table
# taxonRank         | a string specifying taxon rank ("species"/"species-group"/"genus"/"tribe"/"subfamily"/"family"/"superfamily"/"infraorder"/"order"/"class"/"spnov"/"other")
# family            | 
# order             |
# sex               | a string specifying sex ("male"/"female"/"")
# n                 | an integer or vector of integers specifying the number of labels to be plotted.
# filename          | a string specifying file name of output pdf
# owner_code        | a string specifying the collection code to be printed on the labels
# add_taxa          | a logical value that specifies whether or not new taxa should be added to the database. If TRUE the taxon data will be added to the Taxa table in the database when using the occurrence QR function
# setup             | a string specifying the label setup ("higher"/"lower"/"lower_small")

detLabels <- function(CONN, 
                      scientificName,
                      taxonRank = NULL,
                      family = "",
                      order = "",
                      sex = "",
                      n = 1, 
                      filename = "detlabels.pdf",
                      owner_code = "TMU",
                      add_taxa = TRUE,
                      setup = "lower") {
  
    # If 'n' is a single integer, create a vector of 'n' repeated
  if (length(n) == 1) {
    n <- rep(n, length(scientificName))
  }
  
  # Abort if length of 'n' differs from length of 'scientificName'
  if (length (n) != length(scientificName)) {
    stop("parameters 'scientificName' and 'n' differs in length")
  }
  # If 'add_taxa' is FALSE, get taxon data from database
  if (isFALSE(add_taxa)) {
    # Query the database name by name
    query <- NULL
    for (i in 1:length(scientificName)) {
      tmp <- dbGetQuery(CONN, paste("SELECT
      genus,
      specificEpithet,
      scientificNameAuthorship,
      family,
      Taxa.'order',
      taxonRank, 
      scientificName
      FROM Taxa 
      WHERE scientificName = '", scientificName[i],"'", sep = ""))
      
      # If no result, report name
      if (nrow(tmp) < 1) {
        print(paste("ERR: '", scientificName[i], "'", sep = ""))
      } else {
        query <- rbind(query, tmp) 
      }
    }
    
    # Abort if not all names were found in database
    if (nrow(query) < length(scientificName)) {
      stop("Make sure all taxon names are databased and correctly spelled before proceeding")
    }
    # add query results to new variable
    taxon_data <- query
  }
  # If 'add_taxa' is TRUE, add new taxon data
  if (isTRUE(add_taxa)) {
    if (is.null(taxonRank)){
      stop("a taxonRank should be specified")
    }
    # Create variables 'scientificNameAuthorship', 'specificEpithet' and 'genus' 
    scientificNameAuthorship <-  ""
    specificEpithet <- ""
    genus <- ""
    
    # Add input to dataframe. Adds default values to all rows
    y <- data.frame(
      genus,
      specificEpithet,
      scientificNameAuthorship,
      family,
      order,
      taxonRank,
      scientificName
      )
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
    taxon_data <- y
  }
  
  # Create variable with unicode famale and male symbols
  sex2 = sex
  sex2[sex2 == "female"] <- "F"
  sex2[sex2 == "male"] <- "M"
  sex2 = paste("   ",sex2)
  
  # Add collection code to table  
  #taxon_data <- data.frame(taxon_data, owner_code)
  
  # Add "det." prefix to 'scientificName'
  taxon_data <- data.frame(taxon_data, owner_code, sex, sex2, prefix_name = paste("det.", taxon_data$scientificName, sep = ""))
  
  # Write labels
  if (setup == "lower") {
    insectlabel::insectlabel(label_df = taxon_data, n = n, x = 16, y = 3, filename = filename, family = "sans", font = c(3,0,1),
                             text_order = list(c(1,2), 3, c(4,10)), QR_data = c(12,7,6,5,10,9,1), fontsize = c(4, 2.7, 3), linedist = 0.5, 
                             tx = 12, ty = -3, QRd = 5, QRx = 5, QRy = -3, delim = ";", qrlevel = 1)
  }
  
  if (setup == "lower_small") {
    insectlabel::insectlabel(label_df = taxon_data, n = n, x = 25, y = 4, filename = filename, family = "sans", font = c(3,0,1),
                             text_order = list(c(1,2), 3, c(4,10)), QR_data = c(12,7,6,5,10,9,1), fontsize = c(4, 2.7, 3), linedist = 0.6, 
                             tx = 12, ty = -3, QRd = 4, QRx = 5, QRy = -3, delim = ";", qrlevel = 0)
  }
  
  
  if (setup == "higher") { 
    insectlabel::insectlabel(label_df = taxon_data, n = n, x = 16, y = 3, filename = filename, family = "sans", font = c(0,1,0),
                             text_order = list(6,7,10), QR_data = c(12,7,6,5,10,9,1), fontsize = c(3.8, 5, 2.5), linedist = 0.5, tx = 12, 
                             ty = -8, QRd = 5, QRx = 5, QRy = -3, delim = ";", qrlevel = 1)
  }
  
}
  

  
  
  
  