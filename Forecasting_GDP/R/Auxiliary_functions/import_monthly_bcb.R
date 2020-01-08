


import_monthly_bcb <- function(csv_path) {
  df = read.csv(csv_path,  sep = ";", colClasses = c("character","numeric"))
  colnames(df) <- c("Date","Value")
  fff = paste0("01/",df$Date)
  df$Date = as.Date(fff,format = "%d/%m/%Y")
  df
}


