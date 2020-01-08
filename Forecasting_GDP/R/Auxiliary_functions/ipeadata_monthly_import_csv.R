ipeadata_monthly_import_csv <- function(csv_path) {
  
  df = read_csv(csv_path,col_types = list(col_character(), col_double()))
  colnames(df) <- c("Date","Value")
  fff = paste0(sub("\\.","-",df$Date),"-01")
  df$Date = as.Date(fff,format = "%Y-%m-%d")
  df
  
}