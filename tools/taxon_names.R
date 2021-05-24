## taxon_names, search gbif taxonomy backbone tree, and taxon names occurrance data using, Jon Peder Lindemann. 22.09.2020.
# 
# Recently described species can be given the status "NEW". In this case Higher ranks should also be stated.
# If using a full species as a species-group-name, use following syntax, GROUP Mycetophila ruficollis.
# Parameters:
# 


# Load rgbif
library(rgbif)
# function
taxon_names <- function(taxon_name = "", rank = "") {
  # Loop over each row in data.frame
  taxonomy <- data.frame(name = "", status = "", order = "", family = "", genus = "", accepted_name = "")
  for (i in 1:length(taxon_name)) {
     # If species name is specified, use name_backbone function to look up species name 
    if (rank[i] == "species") {
      sp_search <- name_backbone(taxon_name[i], rank = "species") # If name_lookup yield results,  add species name, status, order, family and genus from first hit 
      if (isTRUE(nrow(sp_search) >= 1 & sp_search$matchType == "EXACT")) {
        if (sp_search$status == "ACCEPTED") { # If status ACCEPTED, add species name to "accepted_name"
          sp_name <- data.frame(name = sp_search$scientificName[1], status = "ACCEPTED", order = sp_search$order[1], family = sp_search$family[1], genus = sp_search$genus[1], accepted_name = sp_search$scientificName[1])
          # If taxonomic status is not ACCEPTED make new name-search 
        } 
        else {
          sp_search2 <- name_backbone(sp_search$species, rank = "species")
          sp_name <- data.frame(name = sp_search$scientificName[1], status = sp_search$status, order = sp_search$order[1], family = sp_search$family[1], genus = sp_search$genus[1], accepted_name = sp_search2$scientificName[1])
        }
        # If name-search yield no result, Add "UNKNOWN NAME" status
      } 
      else {
        sp_name <- data.frame(name = taxon_name[i], status = "UNKNOWN NAME", order = "", family = "", genus = "", accepted_name = "")
        }
      }
      # If status equals "genus", use name_backbone function to look up species name
    else if (rank[i] == "genus") {
      sp_search <- name_backbone(taxon_name[i], rank = "genus") # If name_lookup yield results
      if (isTRUE(nrow(sp_search) >= 1 & sp_search$matchType == "EXACT" & sp_search$status == "ACCEPTED")) {
        sp_name <- data.frame(name = taxon_name[i], status = "ACCEPTED GENUS", order = sp_search$order[1], family = sp_search$family[1], genus = sp_search$genus[1], accepted_name = "")
      } 
      else {
        # If name_lookup yeld no results, use input species, family, order, genus names. Add "UNKNOWN GENUS" status
        sp_name <- data.frame(name = taxon_name[i], status = "UNKNOWN GENUS", order = "", family = "", genus = taxon_name[i], accepted_name = "")
      }
    } 
    else if (rank[i] == "family") {
      sp_search <- name_backbone(taxon_name[i], rank = "family") # If name_lookup yield results
      if (isTRUE(nrow(sp_search) >= 1 & sp_search$matchType == "EXACT" & sp_search$status == "ACCEPTED")) {
        sp_name <- data.frame(name = taxon_name[i], status = "ACCEPTED FAMILY", order = sp_search$order[1], family = sp_search$family[1], genus = "", accepted_name = "")
      } 
      else {
        # If name_lookup yeld no results, use input species, family, order, genus names. Add "UNKNOWN FAMILY" status
        sp_name <- data.frame(name = taxon_name[i], status = "UNKNOWN FAMILY", order = "", family = taxon_name[i], genus = "", accepted_name = "")
      }
    } 
    else if (rank[i] == "order") {
      sp_search <- name_backbone(taxon_name[i], rank = "order") # If name_lookup yield results
      if (isTRUE(nrow(sp_search) >= 1 & sp_search$matchType == "EXACT" & sp_search$status == "ACCEPTED")) {
        sp_name <- data.frame(name = taxon_name[i], status = "ACCEPTED ORDER", order = sp_search$order[1], family = "", genus = "", accepted_name = "")
      } 
      else {
        # If name_lookup yeld no results, use input species, family, order, genus names. Add "UNKNOWN FAMILY" status
        sp_name <- data.frame(name = taxon_name[i], status = "UNKNOWN ORDER", order = "", family = "", genus = "", accepted_name = "")
      }
    } 
    else {
      sp_name <- data.frame(name = taxon_name[i], status = rank[i], order = "", family = "", genus = "", accepted_name = "")
    }
    taxonomy[i,] <- sp_name[1,]
  }
  # Output taxonomy-data together with input-data as data.frame
  OUT <- data.frame(taxonomy)
  return(OUT)
}


