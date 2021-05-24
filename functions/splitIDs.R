# splitIDs
# Parameters:
# qr_data

splitIDs <- function(qr_data) {
  qr_data <- strsplit(qr_data, "\n")[[1]] # Split qr_data by '\n' delimiter
  qr_data <- qr_data[qr_data != "" & qr_data != " "] # Remove empty rows
  qr_occ <- qr_data[grep("^((?!det\\.).)*$", qr_data, perl = TRUE)] # Identify specimen-label data (lacking "det." prefix)
  return(qr_occ)
}
