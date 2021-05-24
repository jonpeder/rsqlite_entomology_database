# splitDetIDs
# Parameters:
# qr_data

splitDetIDs <- function(qr_data) {
  qr_data <- strsplit(qr_data, "\n")[[1]] # Split qr_data by '\n' delimiter
  qr_data <- qr_data[qr_data != "" & qr_data != " "] # Remove empty rows
  qr_det <- qr_data[grep("^det\\.", qr_data)] # Identify det-label data (with "det." prefix)
  return(qr_det)
}